{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Tax.Canada.Province.AB (AB428, ab428Fields, fixAB428, fixReturns, t1Fields) where

import Tax.Canada.T1.Types qualified as T1
import Tax.Canada.T1.Types (T1 (T1, page7, page8), Page7(Page7, step6_RefundOrBalanceOwing),
                            Page8(Page8, step6_RefundOrBalanceOwing))
import Tax.Canada.T1.FieldNames.AB (t1Fields)
import Tax.Canada.T1.Fix (fixT1)

import Tax.Canada.Province.AB.AB428.Types qualified as AB
import Tax.Canada.Province.AB.AB428.Types qualified as AB.Page1 (Page1(..))
import Tax.Canada.Province.AB.AB428.Types qualified as AB.Page2 (Page2(..))
import Tax.Canada.Province.AB.AB428.Types (AB428 (AB428))
import Tax.Canada.Province.AB.AB428.Fix (fixAB428)
import Tax.Canada.Province.AB.AB428.FieldNames (ab428Fields)

import Tax.Canada.Shared(MedicalExpenses(..), BaseCredit(..))
import Tax.Util (fixEq)

fixReturns :: (T1 Maybe, AB428 Maybe) -> (T1 Maybe, AB428 Maybe)
fixReturns =
  fixEq $ \(t1@T1{page7 = page7@Page7{step6_RefundOrBalanceOwing},
                  page8 = page8@Page8{step6_RefundOrBalanceOwing = page8step6}},
            ab428@AB428{page1 = page1@AB.Page1{partA, partB = partB1@AB.Page1PartB{spouseAmount}},
                        page2 = page2@AB.Page2{AB.partB = partB2@AB.Page2PartB{AB.medicalExpenses}},
                        page3 = page3@AB.Page3{AB.partC}})
          -> (fixT1 t1{page7 =
                       page7{step6_RefundOrBalanceOwing =
                             step6_RefundOrBalanceOwing{T1.line_42800_ProvTerrTax = ab428.page3.partC.line66_tax}},
                       page8 =
                       page8{step6_RefundOrBalanceOwing =
                             page8step6{T1.line_47900_ProvTerrCredits = ab428.page3.partD.line69_credits}}},
              fixAB428 ab428{AB.page1 =
                             page1{AB.Page1.income = t1.page5.step4_TaxableIncome.line_26000_TaxableIncome,
                                   AB.Page1.partB = partB1{AB.spouseAmount = spouseAmount{reduction = t1.page1.spouse.line23600},
                                                           AB.line19_cppQpp = t1.page6.line30800,
                                                           AB.line20_cppQpp = t1.page6.line31000,
                                                           AB.line21_employmentInsurance = t1.page6.line31200,
                                                           AB.line22_employmentInsurance = t1.page6.line31217}},
                             AB.page2 =
                             page2{AB.Page2.partB = partB2{AB.line33_interest = t1.page6.line31900,
                                                           AB.medicalExpenses =
                                                           medicalExpenses{
                                                              expenses = t1.page6.medical_expenses.familyExpenses,
                                                              netIncome = t1.page4.line_23600_NetIncome}}},
                             AB.page3 =
                             page3{AB.partC = partC{AB.line57_copy = t1.page7.partC_NetFederalTax.line40427}}})
