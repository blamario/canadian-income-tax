{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Tax.Canada.Province.BC (BC428, bc428Fields, fixBC428, fixReturns, t1Fields) where

import Tax.Canada.T1.Types qualified as T1
import Tax.Canada.T1.Types (T1 (T1, page7, page8), Page7(Page7, step6_RefundOrBalanceOwing),
                            Page8(Page8, step6_RefundOrBalanceOwing))
import Tax.Canada.T1.FieldNames.BC (t1Fields)
import Tax.Canada.T1.Fix (fixT1)

import Tax.Canada.Province.BC.BC428.Types qualified as BC
import Tax.Canada.Province.BC.BC428.Types qualified as BC.Page1 (Page1(..))
import Tax.Canada.Province.BC.BC428.Types qualified as BC.Page2 (Page2(..))
import Tax.Canada.Province.BC.BC428.Types (BC428 (BC428))
import Tax.Canada.Province.BC.BC428.Fix (fixBC428)
import Tax.Canada.Province.BC.BC428.FieldNames (bc428Fields)

import Tax.Canada.Shared(MedicalExpenses(..), BaseCredit(..))
import Tax.Util (fixEq)

fixReturns :: (T1 Maybe, BC428 Maybe) -> (T1 Maybe, BC428 Maybe)
fixReturns =
  fixEq $ \(t1@T1{page7 = page7@Page7{step6_RefundOrBalanceOwing}},
            bc428@BC428{page1 = page1@BC.Page1{partA, partB = partB1@BC.Page1PartB{spouseAmount}},
                        page2 = page2@BC.Page2{BC.partB = partB2@BC.Page2PartB{BC.medicalExpenses}},
                        page3 = page3@BC.Page3{BC.partC}})
          -> (fixT1 t1{page7 =
                       page7{step6_RefundOrBalanceOwing =
                             step6_RefundOrBalanceOwing{T1.line_42800_ProvTerrTax = bc428.page3.line91_tax}}},
              fixBC428 bc428{BC.page1 =
                             page1{BC.Page1.partA = partA{BC.income = t1.page5.step4_TaxableIncome.line_26000_TaxableIncome},
                                   BC.Page1.partB = partB1{BC.spouseAmount = spouseAmount{reduction = t1.page1.spouse.line23600}}
                                  },
                             BC.page2 =
                             page2{BC.Page2.partB = partB2{BC.line27_cppQpp = t1.page6.line30800,
                                                           BC.line28_cppQpp = t1.page6.line31000,
                                                           BC.line29_employmentInsurance = t1.page6.line31200,
                                                           BC.line30_employmentInsurance = t1.page6.line31217,
                                                           BC.line41_interest = t1.page6.line31900,
                                                           BC.medicalExpenses =
                                                           medicalExpenses{
                                                              expenses = t1.page6.medical_expenses.familyExpenses,
                                                              netIncome = t1.page4.line_23600_NetIncome}}},
                              BC.page3 =
                              page3{BC.partC = partC{BC.line66_copy = t1.page7.partC_NetFederalTax.line40427},
                                    BC.line74_copy = t1.page4.line_23600_NetIncome}})
