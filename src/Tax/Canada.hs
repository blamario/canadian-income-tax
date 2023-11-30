{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Tax.Canada (T1, fixAlbertaReturns, fixBritishColumbiaReturns, fixOntarioReturns, fixT1) where

import GHC.Stack (HasCallStack)

import Tax.Canada.T1.Types qualified as T1
import Tax.Canada.T1.Types (T1 (T1, page7, page8), Page7(Page7, step6_RefundOrBalanceOwing),
                            Page8(Page8, step6_RefundOrBalanceOwing))
import Tax.Canada.T1.Fix (fixT1)
import Tax.Canada.Province.AB.AB428.Types qualified as AB
import Tax.Canada.Province.AB.AB428.Types qualified as AB.Page1 (Page1(..))
import Tax.Canada.Province.AB.AB428.Types qualified as AB.Page2 (Page2(..))
import Tax.Canada.Province.AB.AB428.Types (AB428 (AB428))
import Tax.Canada.Province.AB.AB428.Fix (fixAB428)
import Tax.Canada.Province.AB.AB428.FieldNames (ab428Fields)
import Tax.Canada.Province.BC.BC428.Types qualified as BC
import Tax.Canada.Province.BC.BC428.Types qualified as BC.Page1 (Page1(..))
import Tax.Canada.Province.BC.BC428.Types qualified as BC.Page2 (Page2(..))
import Tax.Canada.Province.BC.BC428.Types (BC428 (BC428))
import Tax.Canada.Province.BC.BC428.Fix (fixBC428)
import Tax.Canada.Province.BC.BC428.FieldNames (bc428Fields)
import Tax.Canada.Province.ON.ON428.Types qualified as ON
import Tax.Canada.Province.ON.ON428.Types (ON428 (ON428))
import Tax.Canada.Province.ON.ON428.Types qualified as ON.Page1 (Page1(..))
import Tax.Canada.Province.ON.ON428.Types qualified as ON.Page2 (Page2(..))
import Tax.Canada.Province.ON.ON428.Fix (fixON428)
import Tax.Canada.Province.ON.ON428.FieldNames (on428Fields)
import Tax.Util (fixEq)

fixAlbertaReturns :: HasCallStack => (T1 Maybe, AB428 Maybe) -> (T1 Maybe, AB428 Maybe)
fixAlbertaReturns =
  fixEq $ \(t1@T1{page7 = page7@Page7{step6_RefundOrBalanceOwing},
                  page8 = page8@Page8{step6_RefundOrBalanceOwing = page8step6}},
            ab428@AB428{page1 = page1@AB.Page1{partA, partB},
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
                                   AB.Page1.partB = partB{AB.line12_spouseIncome = t1.page1.spouse.line23600,
                                                          AB.line19_cppQpp = t1.page6.line30800,
                                                          AB.line20_cppQpp = t1.page6.line31000,
                                                          AB.line21_employmentInsurance = t1.page6.line31200,
                                                          AB.line22_employmentInsurance = t1.page6.line31217}},
                             AB.page2 =
                             page2{AB.Page2.partB = partB2{AB.line33_interest = t1.page6.line31900,
                                                           AB.medicalExpenses =
                                                           medicalExpenses{AB.line37_expenses =
                                                                           t1.page6.medical_expenses.familyExpenses,
                                                                           AB.line38_income =
                                                                           t1.page4.line_23600_NetIncome}}},
                             AB.page3 =
                             page3{AB.partC = partC{AB.line57_copy = t1.page7.partC_NetFederalTax.line40427}}})

fixBritishColumbiaReturns :: HasCallStack => (T1 Maybe, BC428 Maybe) -> (T1 Maybe, BC428 Maybe)
fixBritishColumbiaReturns =
  fixEq $ \(t1@T1{page7 = page7@Page7{step6_RefundOrBalanceOwing}},
            bc428@BC428{page1 = page1@BC.Page1{partA, partB},
                        page2 = page2@BC.Page2{BC.partB = partB2@BC.Page2PartB{BC.medicalExpenses}},
                        page3 = page3@BC.Page3{BC.partC}})
          -> (fixT1 t1{page7 =
                       page7{step6_RefundOrBalanceOwing =
                             step6_RefundOrBalanceOwing{T1.line_42800_ProvTerrTax = bc428.page3.line91_tax}}},
              fixBC428 bc428{BC.page1 =
                             page1{BC.Page1.partA = partA{BC.income = t1.page5.step4_TaxableIncome.line_26000_TaxableIncome},
                                   BC.Page1.partB = partB{BC.line19_spouseIncome = t1.page1.spouse.line23600}},
                             BC.page2 =
                             page2{BC.Page2.partB = partB2{BC.line27_cppQpp = t1.page6.line30800,
                                                           BC.line28_cppQpp = t1.page6.line31000,
                                                           BC.line29_employmentInsurance = t1.page6.line31200,
                                                           BC.line30_employmentInsurance = t1.page6.line31217,
                                                           BC.line41_interest = t1.page6.line31900,
                                                           BC.medicalExpenses =
                                                           medicalExpenses{BC.line46_expenses =
                                                                           t1.page6.medical_expenses.familyExpenses,
                                                                           BC.line47_income =
                                                                           t1.page4.line_23600_NetIncome}}},
                              BC.page3 =
                              page3{BC.partC = partC{BC.line66_copy = t1.page7.partC_NetFederalTax.line40427},
                                    BC.line74_copy = t1.page4.line_23600_NetIncome}})

fixOntarioReturns :: HasCallStack => (T1 Maybe, ON428 Maybe) -> (T1 Maybe, ON428 Maybe)
fixOntarioReturns =
  fixEq $ \(t1@T1{page7 = page7@Page7{step6_RefundOrBalanceOwing}},
            on428@ON428{page1 = page1@ON.Page1{partB},
                        page2 = page2@ON.Page2{ON.partB = partB2@ON.Page2PartB{ON.medicalExpenses},
                                               ON.partC}})
          -> (fixT1 t1{page7 =
                       page7{step6_RefundOrBalanceOwing =
                             step6_RefundOrBalanceOwing{T1.line_42800_ProvTerrTax = on428.page4.line90}}},
              fixON428 on428{ON.page1 =
                             page1{ON.line1 = t1.page5.step4_TaxableIncome.line_26000_TaxableIncome,
                                   ON.Page1.partB = partB{ON.line12_spouseIncome = t1.page1.spouse.line23600,
                                                          ON.line19_cppQpp = t1.page6.line30800,
                                                          ON.line20_cppQpp = t1.page6.line31000,
                                                          ON.line21_employmentInsurance = t1.page6.line31200,
                                                          ON.line22_employmentInsurance = t1.page6.line31217}},
                             ON.page2 =
                             page2{ON.Page2.partB = partB2{ON.line32_interest = t1.page6.line31900,
                                                           ON.medicalExpenses =
                                                           medicalExpenses{ON.line37_income =
                                                                           t1.page4.line_23600_NetIncome}},
                                   ON.partC = partC{ON.line59_copy = t1.page7.partC_NetFederalTax.line40427}}})
