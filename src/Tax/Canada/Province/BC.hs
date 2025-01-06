{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Tax.Canada.Province.BC (BC428, bc428Fields, bc479Fields, fixBC428, fixBC479, fixReturns, returnFields, t1Fields) where

import Data.CAProvinceCodes (Code(BC))
import Rank2 qualified
import Rank2.TH qualified
import Transformation.Shallow.TH qualified

import Tax.Canada.Federal qualified as Federal
import Tax.Canada.Federal (Forms(t1), fixFederalForms)
import Tax.Canada.T1.Types (T1 (T1, page7, page8), Page7(Page7, step6_RefundOrBalanceOwing), Page8(Page8))
import Tax.Canada.T1.Types qualified as T1
import Tax.Canada.T1.Types qualified as Page8 (Page8(..))
import Tax.Canada.T1.FieldNames.BC (t1Fields)

import Tax.Canada.Province.BC.BC428.Types qualified as BC
import Tax.Canada.Province.BC.BC428.Types qualified as BC.Page1 (Page1(..))
import Tax.Canada.Province.BC.BC428.Types qualified as BC.Page2 (Page2(..))
import Tax.Canada.Province.BC.BC428.Types (BC428 (BC428))
import Tax.Canada.Province.BC.BC428.Fix (fixBC428)
import Tax.Canada.Province.BC.BC428.FieldNames (bc428Fields)
import Tax.Canada.Province.BC.BC479.Types qualified as BCC
import Tax.Canada.Province.BC.BC479.Types (BC479 (BC479))
import Tax.Canada.Province.BC.BC479.Fix (fixBC479)
import Tax.Canada.Province.BC.BC479.FieldNames (bc479Fields)

import Tax.Canada.Shared(MedicalExpenses(..), BaseCredit(..))
import Tax.FDF (FieldConst, within)
import Tax.Util (fixEq, totalOf)

data Returns line = Returns {
  federal :: Federal.Forms line,
  bc428 :: BC428 line,
  bc479 :: BC479 line}

deriving instance (Show (Federal.Forms line), Show (BC428 line), Show (BC479 line)) => Show (Returns line)
deriving instance (Eq (Federal.Forms line), Eq (BC428 line), Eq (BC479 line)) => Eq (Returns line)
Rank2.TH.deriveFunctor ''Returns
Rank2.TH.deriveApply ''Returns
Rank2.TH.deriveApplicative ''Returns
Rank2.TH.deriveFoldable ''Returns
Rank2.TH.deriveTraversable ''Returns
Transformation.Shallow.TH.deriveAll ''Returns

fixReturns :: Federal.InputForms Maybe -> Returns Maybe -> Returns Maybe
fixReturns inputs =
  fixEq $ \Returns{federal = ff@Federal.Forms{t1 = t1@T1{page7 = page7@Page7{step6_RefundOrBalanceOwing},
                                                         page8 = page8@Page8{step6_RefundOrBalanceOwing = page8step6}}},
                   bc428 = bc428@BC428{page1 = page1@BC.Page1{partA, partB = partB1@BC.Page1PartB{spouseAmount}},
                                       page2 = page2@BC.Page2{BC.partB = partB2@BC.Page2PartB{BC.medicalExpenses}},
                                       page3 = page3@BC.Page3{BC.partC}},
                   bc479}
          -> Returns{federal = fixFederalForms BC inputs $
                               ff{t1 = t1{page7 =
                                          page7{step6_RefundOrBalanceOwing =
                                                step6_RefundOrBalanceOwing{T1.line_42800_ProvTerrTax =
                                                                           bc428.page3.line91_tax}},
                                          page8 =
                                          page8{Page8.step6_RefundOrBalanceOwing =
                                                page8step6{T1.line_47900_ProvTerrCredits =
                                                           bc479.page3.line42_credits}}}},
                     bc428 = fixBC428
                             bc428{BC.page1 =
                                   page1{BC.Page1.partA =
                                         partA{BC.income = t1.page5.step4_TaxableIncome.line_26000_TaxableIncome},
                                         BC.Page1.partB =
                                         partB1{BC.spouseAmount = spouseAmount{reduction = t1.page1.spouse.line_23600}}
                                        },
                                   BC.page2 =
                                   page2{BC.Page2.partB = partB2{BC.line27_cppQpp = t1.page6.line_30800,
                                                                 BC.line28_cppQpp = t1.page6.line_31000,
                                                                 BC.line29_employmentInsurance = t1.page6.line_31200,
                                                                 BC.line30_employmentInsurance = t1.page6.line_31217,
                                                                 BC.line41_interest = t1.page6.line_31900,
                                                                 BC.medicalExpenses =
                                                                 medicalExpenses{
                                                                    expenses = t1.page6.medical_expenses.familyExpenses,
                                                                    netIncome = t1.page4.line_23600_NetIncome}}},
                                    BC.page3 =
                                    page3{BC.partC = partC{BC.line66_copy = t1.page7.partC_NetFederalTax.line_40427},
                                          BC.line74_copy = t1.page4.line_23600_NetIncome}},
                     bc479 = fixBC479
                             bc479{BCC.page1 =
                                   bc479.page1{BCC.line1_netIncome_self = t1.page4.line_23600_NetIncome,
                                               BCC.line1_netIncome_spouse = t1.page1.spouse.line_23600,
                                               BCC.line4_uccb_rdsp_income_self = totalOf [t1.page3.line_11700_UCCB,
                                                                                          t1.page3.line_12500_RDSP],
                                               BCC.line7_threshold = if t1.page1.identification.maritalStatus
                                                                        `elem` [Just T1.Married,
                                                                                Just T1.LivingCommonLaw]
                                                                     then Just 18_000
                                                                     else 15_000 <$ t1.page1.identification.maritalStatus}}}

returnFields :: Returns FieldConst
returnFields = Returns{
  federal = Federal.formFieldsForProvince BC,
  bc428 = within "428" Rank2.<$> bc428Fields,
  bc479 = within "479" Rank2.<$> bc479Fields}
