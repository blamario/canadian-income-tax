{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The federal income tax forms

module Tax.Canada.Federal (InputForms, Forms(..), loadInputForms, fixFederalForms,
                           formFieldsForProvince, formFileNames, relevantFormKeys) where

import Control.Applicative ((<|>))
import Control.Monad ((=<<))
import Data.CAProvinceCodes qualified as Province
import Data.Fixed (Centi)
import Data.Foldable (find)
import Data.Functor.Compose (Compose(Compose))
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import Data.Maybe (isJust)
import Data.Map (Map, fromList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Semigroup (Any (Any, getAny), Sum(Sum, getSum))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day)
import GHC.Stack (HasCallStack)
import Rank2 qualified
import Rank2.TH qualified
import Transformation.Shallow.TH qualified
import Text.FDF (FDF)

import Tax.Canada.Federal.Schedule6 qualified as Schedule6
import Tax.Canada.Federal.Schedule6 (Schedule6, fixSchedule6, schedule6Fields)
import Tax.Canada.Federal.Schedule7 qualified
import Tax.Canada.Federal.Schedule7 (Schedule7, fixSchedule7, schedule7Fields)
import Tax.Canada.Federal.Schedule8 qualified as Schedule8
import Tax.Canada.Federal.Schedule8 (Schedule8(page4, page5, page6, page7, page8, page9, page10),
                                     fixSchedule8, schedule8Fields,
                                     Page4(line_50339_totalPensionableEarnings,
                                           line_50340_totalContributions,
                                           line_50341_totalSecondContributions),
                                     Page6(line1_netSelfEmploymentEarnings),
                                     Page7(line1_netSelfEmploymentEarnings))
import Tax.Canada.Federal.Schedule9 (Schedule9(page1), Page1(line23_sum), fixSchedule9, schedule9Fields)
import Tax.Canada.Federal.Schedule11 (Schedule11(page1), Page1(line5_trainingClaim, line17_sum), fixSchedule11, schedule11Fields)
import Tax.Canada.FormKey (FormKey)
import Tax.Canada.FormKey qualified as FormKey
import Tax.Canada.T1 (fixT1, t1FieldsForProvince)
import Tax.Canada.T1.Types (T1(page3, page4, page5, page6, page7, page8),
                            Page3(line_10100_EmploymentIncome, line_10120_Commissions, line_12200_PartnershipIncome,
                                  line29_sum),
                            Page4(line_20600_PensionAdjustment, line_20700_RPPDeduction, line_20800_RRSPDeduction,
                                  line_21200_Dues, line_22200_CPP_QPP_Contributions, line_22215_DeductionCPP_QPP,
                                  line_22900_OtherEmployExpenses),
                            Page5(step4_TaxableIncome),
                            Step4(line_24400_MilitaryPoliceDeduction, line_24900_SecurityDeductions),
                            Page6(line_30800, line_31000, line_31200, line_31205, line_32300, line_34900),
                            Page7(step6_RefundOrBalanceOwing),
                            Page7Step6(line_42100_CPPContributions),
                            Page8(step6_RefundOrBalanceOwing),
                            Page8Step6(line_43700_Total_income_tax_ded, line_44800_CPPOverpayment,
                                       line_45300_CWB, line_45350_CTC),
                            LanguageOfCorrespondence, MaritalStatus)
import Tax.Canada.T4 (T4, t4Fields, T4Slip(box16_employeeCPP, box26_pensionableEarnings))
import Tax.Canada.T4 qualified as T4
import Tax.Canada.Shared (SubCalculation(result))
import Tax.FDF (Entry (Amount), FieldConst (Field), load, within)
import Tax.Util (fixEq, totalOf)

-- | All supported federal input forms, not to be filled in
data InputForms line = InputForms{
  t4 :: Maybe (NonEmpty (T4 line))}

deriving instance Show (InputForms Maybe)

instance Semigroup (InputForms Maybe) where
  InputForms x <> InputForms y = InputForms $ x <> y

instance Monoid (InputForms Maybe) where
  mempty = InputForms Nothing

-- | All supported federal forms
data Forms line = Forms{
   t1 :: T1 line,
   schedule6 :: Schedule6 line,
   schedule7 :: Schedule7 line,
   schedule8 :: Schedule8 line,
   schedule9 :: Schedule9 line,
   schedule11 :: Schedule11 line}

deriving instance (Show (line Bool), Show (line Centi), Show (line Word), Show (line Int), Show (line Text),
                   Show (line Rational), Show (line Province.Code), Show (line Day),
                   Show (line LanguageOfCorrespondence), Show (line MaritalStatus))
               => Show (Forms line)
deriving instance (Eq (line Bool), Eq (line Centi), Eq (line Word), Eq (line Int), Eq (line Text),
                   Eq (line Rational), Eq (line Province.Code), Eq (line Day),
                   Eq (line LanguageOfCorrespondence), Eq (line MaritalStatus))
               => Eq (Forms line)

Rank2.TH.deriveAll ''Forms
Transformation.Shallow.TH.deriveAll ''Forms

loadInputForms :: [(FormKey, FDF)] -> Either String (InputForms Maybe)
loadInputForms forms = InputForms . nonEmpty <$> traverse (load T4.t4Fields . snd) forms

-- | Complete all the federal forms, also handling the inter-form field references.
fixFederalForms :: Province.Code -> InputForms Maybe -> Forms Maybe -> Forms Maybe
fixFederalForms province InputForms{t4} = fixEq $ \Forms{t1, schedule6, schedule7, schedule8, schedule9, schedule11}->
                                                    let fromT4s' = fromT4s t4 in Forms{
   t1 = fixT1 t1{
       page3 = fromT4s' (.slip1.box14_employmentIncome) (\amt pg-> pg{line_10100_EmploymentIncome = amt}) $
               fromT4s' (additionalT4 ["42"]) (\amt pg-> pg{line_10120_Commissions = amt}) $
               t1.page3,
       page4 = fromT4s' (.slip1.box52_pensionAdjustment) (\amt pg-> pg{line_20600_PensionAdjustment = amt}) $
               fromT4s' (.slip1.box20_employeeRPP) (\amt pg-> pg{line_20700_RPPDeduction = amt}) $
               fromT4s' (.slip1.box44_unionDues) (\amt pg-> pg{line_21200_Dues = amt}) $
               fromT4s' (additionalT4 ["77"]) (\amt step-> step{line_22900_OtherEmployExpenses = amt}) $
               t1.page4{line_20800_RRSPDeduction = schedule7.page3.partC.line20_deduction,
                        line_22200_CPP_QPP_Contributions = schedule8.page6.line17_sum <|>
                                                           schedule8.page10.line97_sum,
                        line_22215_DeductionCPP_QPP = schedule8.page5.part3a.line30_sum <|>
                                                      schedule8.page5.part3b.line43_sum <|>
                                                      schedule8.page9.line77_sum},
       page5 = t1.page5{
          step4_TaxableIncome =
             fromT4s' (additionalT4 ["39", "41", "91", "92"])
                (\amt step-> step{line_24900_SecurityDeductions = amt}) $
             fromT4s' (additionalT4 ["43"]) (\amt step-> step{line_24400_MilitaryPoliceDeduction = amt}) $
             t1.page5.step4_TaxableIncome},
       page6 = (case province
                of Province.QC ->
                     fromT4s' (.slip1.box18_employeeEI) (\amt pg-> pg{line_31200 = amt}) $
                     fromT4s' (.slip1.box55_premiumPPIP) (\amt pg-> pg{line_31205 = amt}) t1.page6
                   _ -> fromT4s' (\t4-> totalOf [t4.slip1.box18_employeeEI, t4.slip1.box55_premiumPPIP])
                           (\amt pg-> pg{line_31200 = amt}) t1.page6)
               {line_30800 = if any hasAnyField t4
                             then schedule8.page5.part3a.line27_copy <|>
                                  schedule8.page5.part3b.line32_join <|>
                                  schedule8.page9.line60_least
                             else t1.page6.line_30800,
                line_31000 = if any hasAnyField t4
                             then schedule8.page6.line15_half.result <|>
                                  schedule8.page9.line78_half.result
                             else t1.page6.line_31000,
                line_32300 = schedule11.page1.line17_sum,
                line_34900 = schedule9.page1.line23_sum},
       page7 = t1.page7{
          step6_RefundOrBalanceOwing = t1.page7.step6_RefundOrBalanceOwing{
             line_42100_CPPContributions = schedule8.page6.line14_sum <|>
                                           find (> 0) schedule8.page8.line55_difference}},
       page8 = t1.page8{
          step6_RefundOrBalanceOwing =
             (fromT4s' (.slip1.box22_incomeTaxDeducted) (\amt pt-> pt{line_43700_Total_income_tax_ded = amt})
                 t1.page8.step6_RefundOrBalanceOwing)
             {line_44800_CPPOverpayment = schedule8.page5.part3a.line31_copy <|> schedule8.page9.line56_half.result,
              line_45300_CWB = schedule6.page4.step3.line42_sum <|>
                               schedule6.page4.step2.line28_difference,
              line_45350_CTC = schedule11.page1.line5_trainingClaim}}},
   schedule6 = fixSchedule6 Nothing t1 schedule6,
   schedule7 = fixSchedule7 t1 schedule7,
   schedule8 = fixSchedule8 schedule8{
      page4 = schedule8.page4{
         line_50339_totalPensionableEarnings =
            totalOf . (liftA2 (<|>) (.slip1.box26_pensionableEarnings) (.slip1.box14_employmentIncome) <$>) =<< t4,
         line_50340_totalContributions = totalOf . fmap (.slip1.box16_employeeCPP) =<< t4},
      page6 = schedule8.page6{
         line1_netSelfEmploymentEarnings = totalOf [t1.page3.line_12200_PartnershipIncome,
                                                    t1.page3.line29_sum.result]},
      page7 = schedule8.page7{
         line1_netSelfEmploymentEarnings = totalOf [t1.page3.line_12200_PartnershipIncome,
                                                    t1.page3.line29_sum.result]}},
   schedule9 = fixSchedule9 t1 schedule9,
   schedule11 = fixSchedule11 t1 schedule11}

-- | The paths of all the fields in all federal forms, with the form key added as the head of every field path.
formFieldsForProvince :: Province.Code -> Forms FieldConst
formFieldsForProvince p = Forms{
  t1 = within (Text.pack $ show FormKey.T1) Rank2.<$> t1FieldsForProvince p,
  schedule6 = within (Text.pack $ show FormKey.Schedule6) Rank2.<$> schedule6Fields,
  schedule7 = within (Text.pack $ show FormKey.Schedule7) Rank2.<$> schedule7Fields,
  schedule8 = within (Text.pack $ show FormKey.Schedule8) Rank2.<$> schedule8Fields,
  schedule9 = within (Text.pack $ show FormKey.Schedule9) Rank2.<$> schedule9Fields,
  schedule11 = within (Text.pack $ show FormKey.Schedule11) Rank2.<$> schedule11Fields}

-- | A map of standard file paths, without the common suffix and extension, of all supported federal schedule forms.
formFileNames :: Map FormKey Text
formFileNames = fromList [
  (FormKey.Schedule6, "5000-s6"),
  (FormKey.Schedule7, "5000-s7"),
  (FormKey.Schedule8, "5000-s8"),
  (FormKey.Schedule9, "5000-s9"),
  (FormKey.Schedule11, "5000-s11")]

-- | The set of keys of all auxiliary forms that have effect in the given 'T1' form.
relevantFormKeys :: T1 Maybe -> Set FormKey
relevantFormKeys t1 = Set.fromList $
  [FormKey.Provincial428 | isJust t1.page8.step6_RefundOrBalanceOwing.line_45300_CWB] <>
  [FormKey.Schedule6 | isJust t1.page8.step6_RefundOrBalanceOwing.line_45300_CWB] <>
  [FormKey.Schedule7 | isJust t1.page4.line_20800_RRSPDeduction] <>
  [FormKey.Schedule8 | isJust (t1.page4.line_22200_CPP_QPP_Contributions
                               <|> t1.page4.line_22215_DeductionCPP_QPP
                               <|> t1.page6.line_30800
                               <|> t1.page6.line_31000
                               <|> t1.page7.step6_RefundOrBalanceOwing.line_42100_CPPContributions
                               <|> t1.page8.step6_RefundOrBalanceOwing.line_44800_CPPOverpayment)] <>
  [FormKey.Schedule9 | isJust t1.page6.line_34900] <>
  [FormKey.Schedule11 | isJust (t1.page6.line_32300 <|> t1.page8.step6_RefundOrBalanceOwing.line_45350_CTC)] <>
  [FormKey.T1]

hasAnyField :: Foldable f => f (T4 Maybe) -> Bool
hasAnyField = getAny . foldMap (Rank2.foldMap (Any . isJust))

fromT4s :: Maybe (NonEmpty (T4 Maybe)) -> (T4 Maybe -> Maybe Centi) -> (Maybe Centi -> form -> form) -> form -> form
fromT4s t4 field set
   | hasAnyField (Compose t4) = set $ totalOf $ field <$> Compose t4
   | otherwise = id

additionalT4 :: [Text] -> T4 Maybe -> Maybe Centi
additionalT4 codes t4 = getSum <$> foldMap findCode t4.slip1.otherInformation
   where findCode (Rank2.Pair (Rank2.Only (Just code)) (Rank2.Only (Just amt)))
           | elem code codes = Just (Sum amt)
         findCode _ = Nothing
