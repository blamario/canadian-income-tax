{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tax.Canada.T1.Fix (fixT1, fixEq, nonNegativeDifference, totalOf) where

import Control.Applicative ((<|>))
import Data.Fixed (Centi)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Time.Calendar (Year, dayPeriod)
import Rank2 qualified

import Tax.Canada.T1.Types

fixT1 :: T1 Maybe -> T1 Maybe
fixT1 = fixEq $ \t1@T1{..}-> T1{page1 = fixPage1 page1,
                                page2 = fixPage2 page2,
                                page3 = fixPage3 page3,
                                page4 = fixPage4 t1 page4,
                                page5 = fixPage5 t1 page5,
                                page6 = fixPage6 t1 page6,
                                page7 = fixPage7 t1 page7,
                                page8 = fixPage8 t1 page8}

fixPage1 :: Page1 Maybe -> Page1 Maybe
fixPage1 = id

fixPage2 :: Page2 Maybe -> Page2 Maybe
fixPage2 = id

fixPage3 :: Page3 Maybe -> Page3 Maybe
fixPage3 = fixEq $ \page@Page3{..}-> page{
   line_19 = totalOf [line_10100_EmploymentIncome ,
                      line_10400_OtherEmploymentIncome,
                      line_11300_OldAgeSecurityPension,
                      line_11400_CPP_QPP,
                      line_11500_OtherPensions,
                      line_11600_ElectedSplitPension,
                      line_11700_UCCB,
                      line_11900_EmploymentInsurance,
                      line_12000_TaxableDividends,
                      line_12100_InvestmentIncome,
                      line_12200_PartnershipIncome,
                      line_12500_RDSP,
                      line_12600_Amount,
                      line_12700_TaxableCapitalGains,
                      line_12800_Amount,
                      line_12900_RRSPIncome,
                      line_13000_OtherIncome,
                      line_13010_Taxablescholarship],
   line_25_sum = totalOf [line_13500_Amount,
                          line_13700_Amount,
                          line_13900_Amount,
                          line_14100_Amount,
                          line_14300_Amount],
   line_25_cont = line_25_sum,
   line_26 = totalOf [line_19, line_25_cont],
   line_14700_EqualsAmount = totalOf [line_14400_WorkersCompBen,
                                      line_14500_SocialAssistPay,
                                      line_14600_NetFedSupplements],
   line_14700_PlusAmount = line_14700_EqualsAmount,
   line_15000_TotalIncome = totalOf [line_26, line_14700_PlusAmount]}

fixPage4 :: T1 Maybe -> Page4 Maybe -> Page4 Maybe
fixPage4 t1 = fixEq $ \page@Page4{..}-> page{
   line_15000_TotalIncome_2 = t1.page3.line_15000_TotalIncome,
   line_23300_sum = totalOf [line_20700_RPPDeduction,
                             line_20800_RRSPDeduction,
                             line_21000_SplitPensionDeduction,
                             line_21200_Dues,
                             line_21300_UCCBRepayment,
                             line_21400_ChildCareExpenses,
                             line_21500_DisabilityDeduction,
                             line_21700_Amount,
                             line_21900_MovingExpenses,
                             line_22000_Amount,
                             line_22100_CarryingChargesInterest,
                             line_22200_CPP_QPP_Contributions,
                             line_22215_DeductionCPP_QPP,
                             line_22400_XplorationDevExpenses,
                             line_22900_OtherEmployExpenses,
                             line_23100_ClergyResDeduction,
                             line_23200_OtherDeductions,
                             line_23210],
   line_23300_cont = line_23300_sum,
   line_23400_NetBeforeAdjust = nonNegativeDifference line_15000_TotalIncome_2 line_23300_cont,
   line_23600_NetIncome = nonNegativeDifference line_23400_NetBeforeAdjust line_23500_SocialBenefits}

fixPage5 :: T1 Maybe -> Page5 Maybe -> Page5 Maybe
fixPage5 t1 = fixEq $ \Page5{..}-> Page5{
   step4_TaxableIncome = fixStep4 t1 step4_TaxableIncome,
   partA_FederalTax = fixPage5PartA t1 partA_FederalTax,
   partB_FederalTaxCredits = fixPage5PartB t1 partB_FederalTaxCredits}

fixPage6 :: T1 Maybe -> Page6 Maybe -> Page6 Maybe
fixPage6 t1 = fixEq $ \page@Page6{..}-> page{
   line_79 = t1.page5.partB_FederalTaxCredits.line_81,
   line94_sum = totalOf [line30800,
                         line31000,
                         line31200,
                         line31217,
                         line31220,
                         line31240,
                         line31260,
                         line31270,
                         line31285,
                         line31300,
                         line31350],
   line94_cont = line94_sum,
   line96 = totalOf [line94_cont, line31400],
   line99 = totalOf [line96, line31600, line31800],
   line104 = totalOf [line99, line31900, line32300, line32400, line32600],
   medical_expenses = fixMedicalExpenses t1 medical_expenses,
   line33200_sum = totalOf [medical_expenses.difference, medical_expenses.otherDependants],
   line33200_cont = line33200_sum,
   line33500 = totalOf [line104, line33200_cont],
   line33800 = (* 0.15) <$> line33500,
   line35000 = totalOf [line33800, line34900]}

fixPage7 :: T1 Maybe -> Page7 Maybe -> Page7 Maybe
fixPage7 t1 = fixEq $ \Page7{partC_NetFederalTax, step6_RefundOrBalanceOwing}-> Page7{
   partC_NetFederalTax = fixPage7PartC t1 partC_NetFederalTax,
   step6_RefundOrBalanceOwing = fixPage7Step6 t1 step6_RefundOrBalanceOwing}

fixPage8 :: T1 Maybe -> Page8 Maybe -> Page8 Maybe
fixPage8 t1 = fixEq $ \page@Page8{..}-> Page8{
   step6_RefundOrBalanceOwing = fixPage8Step6 t1 step6_RefundOrBalanceOwing,
   line48400_Refund = step6_RefundOrBalanceOwing.line164_Refund_or_BalanceOwing
                      >>= \x-> if x < 0 then Just (negate x) else Nothing,
   line48500_BalanceOwing = step6_RefundOrBalanceOwing.line164_Refund_or_BalanceOwing
                            >>= \x-> if x > 0 then Just x else Nothing,
   line_46600 = (-) <$> line48400_Refund <*> line_46500,
   ..}

fixStep4 :: T1 Maybe -> Step4 Maybe -> Step4 Maybe
fixStep4 t1 = fixEq $ \step@Step4{..}-> step{
   line_23600_NetIncome_2 = t1.page4.line_23600_NetIncome,
   line_25700_AddLines_sum = totalOf [line_24400_MilitaryPoliceDeduction,
                                      line_24900_SecurityDeductions,
                                      line_25000_OtherPayDeductions,
                                      line_25100_PartnershipLosses,
                                      line_25200_NoncapitalLosses,
                                      line_25300_NetCapitalLosses,
                                      line_25400_CapitalGainsDeduction,
                                      line_25500_NorthernDeductions,
                                      line_25600_AdditionalDeductions_Amount],
   line_25700_AddLines_cont = line_25700_AddLines_sum,
   line_26000_TaxableIncome = nonNegativeDifference line_23600_NetIncome_2 line_25700_AddLines_cont}

fixPage5PartA :: T1 Maybe -> Page5PartA Maybe -> Page5PartA Maybe
fixPage5PartA t1 = fixEq $ \part@Page5PartA{..}-> part{
   column1 = taxIncomeBracket 0 50197 0.15 0,
   column2 = taxIncomeBracket 50197 100392 0.205 7529.55,
   column3 = taxIncomeBracket 100392 155625 0.26 17819.53,
   column4 = taxIncomeBracket 155625 221708 0.29 32180.11,
   column5 = taxIncomeBracket 221708 (10^12) 0.33 51344.18} -- a trillion ought to be enough for anybody
   where taxIncomeBracket threshold ceiling rate baseTax
            | income > threshold && income <= ceiling = TaxIncomeBracket{
                 line67_income = Just income,
                 line69_overThreshold = Just (income - threshold),
                 line71_timesRate = Just ((income - threshold) * rate),
                 line73_equalsTax = Just ((income - threshold) * rate + baseTax)}
            | otherwise = Rank2.pure Nothing
         income = fromMaybe 0 t1.page5.step4_TaxableIncome.line_26000_TaxableIncome

fixPage5PartB :: T1 Maybe -> Page5PartB Maybe -> Page5PartB Maybe
fixPage5PartB t1 = fixEq $ \part@Page5PartB{..}-> part{
   line30000 = let income = fromMaybe 0 t1.page4.line_23600_NetIncome
               in if income <= 155625 then Just 14398
                  else if income >= 221708 then Just 12719
                       else Just $ 14398 - (14398 - 12719) * (income - 155625) / (221708 - 155625),
   line30100 = if any ((<= (1957 :: Year)) . dayPeriod) t1.page1.identification.dateBirth
               then Just 12719
               else Nothing,
   line30500 =  ((* 2350) . fromIntegral) <$> line30499_ChildrenNum,
   line_81 = totalOf [line30000,
                      line30100,
                      line30300,
                      line30400,
                      line30425,
                      line30450,
                      line30500]}

fixMedicalExpenses :: T1 Maybe -> MedicalExpenses Maybe -> MedicalExpenses Maybe
fixMedicalExpenses t1 = fixEq $ \expenses@MedicalExpenses{familyExpenses, taxableIncome,
                                                          taxableIncomeFraction, threshold}-> expenses{
   taxableIncome = t1.page4.line_23600_NetIncome,
   taxableIncomeFraction = (* 0.03) <$> taxableIncome,
   threshold = min 2479 <$> taxableIncomeFraction,
   difference = max 0 <$> nonNegativeDifference familyExpenses threshold}

fixPage7PartC :: T1 Maybe -> Page7PartC Maybe -> Page7PartC Maybe
fixPage7PartC t1 = fixEq $ \part@Page7PartC{..}-> part{
   line116 = let partA = t1.page5.partA_FederalTax
             in partA.column1.line73_equalsTax
                <|> partA.column2.line73_equalsTax
                <|> partA.column3.line73_equalsTax
                <|> partA.column4.line73_equalsTax
                <|> partA.column5.line73_equalsTax,
   line40400 = totalOf [line116, line40424],
   line119 = t1.page6.line35000,
   line40425,
   line40427,
   line122_sum = totalOf [line119, line40425, line40427],
   line122_cont = line122_sum,
   line123 = nonNegativeDifference line40400 line122_cont,
   line125 = totalOf [line123, line124],
   line127 = difference line125 line40500,
   line129 = totalOf [line127, line128],
   line131 = nonNegativeDifference line129 line130,
   line41000 = case line40900
               of Just x
                    | x <= 400 -> Just (x * 0.75)
                    | x <= 750 -> Just ((x - 400) * 0.5 + 300)
                    | otherwise-> Just ((x - 750) * 0.3333 + 475)
                  Nothing -> Nothing,
   line41600_sum = totalOf [line41000, line41200, line41400],
   line41600_cont = line41600_sum,
   line41700 = nonNegativeDifference line131 line41600_cont,
   line42000 = totalOf [line41700, line41500, line41800]}

fixPage7Step6 :: T1 Maybe -> Page7Step6 Maybe -> Page7Step6 Maybe
fixPage7Step6 t1 = fixEq $ \step@Page7Step6{..}-> step{
   line140 = t1.page7.partC_NetFederalTax.line42000,
   line_43500_TotalPayable = totalOf [line140, line_42100_CPPContributions, line_42120_EIPremiums,
                                      line_42200_SocialBenefits, line_42800_ProvTerrTax]}

fixPage8Step6 :: T1 Maybe -> Page8Step6 Maybe -> Page8Step6 Maybe
fixPage8Step6 t1 = fixEq $ \step@Page8Step6{..}-> step{
   line_43500_totalpayable = t1.page7.step6_RefundOrBalanceOwing.line_43500_TotalPayable,
   line_46900 = (* 0.25) <$> line_46800,
   line_48200_sum = totalOf [line_43700_Total_income_tax_ded,
                             line_44000,
                             line_44800_CPPOverpayment,
                             line_45000_EIOverpayment,
                             line_45200_MedicalExpense,
                             line_45300_CWB,
                             line_45350_CTC,
                             line_45400_InvestmentTaxCredit,
                             line_45600_TrustTaxCredit,
                             line_45700_GST_HST_Rebate,
                             line_46900,
                             line_47555_TaxPaid,
                             line_47556,
                             line_47557,
                             line_47600_TaxPaid,
                             line_47900_ProvTerrCredits],
   line_48200_cont = line_48200_sum,
   line164_Refund_or_BalanceOwing = difference line_43500_totalpayable line_48200_sum}

fixEq :: Eq a => (a -> a) -> a -> a
fixEq f a
   | a == a' = a
   | otherwise = fixEq f a'
   where a' = f a

totalOf :: [Maybe Centi] -> Maybe Centi
totalOf = fmap sum . nonEmpty . mapMaybe id

difference :: Maybe Centi -> Maybe Centi -> Maybe Centi
difference Nothing Nothing = Nothing
difference a b = Just (fromMaybe 0 a - fromMaybe 0 b)

nonNegativeDifference :: Maybe Centi -> Maybe Centi -> Maybe Centi
nonNegativeDifference Nothing Nothing = Nothing
nonNegativeDifference a b = Just (max 0 $ fromMaybe 0 a - fromMaybe 0 b)
