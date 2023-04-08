{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Tax.Canada (T1, ON428, fixOntarioReturns, fixT1, fixON428, t1Fields, on428Fields) where

import Tax.Canada.T1.Types qualified as T1
import Tax.Canada.T1.Types (T1 (T1, page7), Page7(Page7, step6_RefundOrBalanceOwing))
import Tax.Canada.T1.Fix (fixT1)
import Tax.Canada.T1.FieldNames (t1Fields)
import Tax.Canada.ON428.Types qualified as ON hiding (Page1(..))
import Tax.Canada.ON428.Types (ON428 (ON428, page1, page2),
                               Page1(Page1, line1, partB))
import Tax.Canada.ON428.Fix (fixON428)
import Tax.Canada.ON428.FieldNames (on428Fields)
import Tax.Util (fixEq)

fixOntarioReturns :: (T1 Maybe, ON428 Maybe) -> (T1 Maybe, ON428 Maybe)
fixOntarioReturns =
  fixEq $ \(t1@T1{page7 = page7@Page7{step6_RefundOrBalanceOwing}},
            on428@ON428{page1 = page1@Page1{partB},
                        page2 = page2@ON.Page2{ON.partB = partB2@ON.Page2PartB{ON.medicalExpenses},
                                               ON.partC}})
          -> (fixT1 t1{page7 =
                       page7{step6_RefundOrBalanceOwing =
                             step6_RefundOrBalanceOwing{T1.line_42800_ProvTerrTax = on428.page4.line90}}},
              fixON428 on428{page1 =
                             page1{line1 = t1.page5.step4_TaxableIncome.line_26000_TaxableIncome,
                                   partB = partB{ON.line12_spouseIncome = t1.page1.spouse.line23600,
                                                 ON.line19_cppQpp = t1.page6.line30800,
                                                 ON.line20_cppQpp = t1.page6.line31000,
                                                 ON.line21_employmentInsurance = t1.page6.line31200,
                                                 ON.line22_employmentInsurance = t1.page6.line31217}},
                             page2 =
                             page2{ON.partB = partB2{ON.line32_interest = t1.page6.line31900,
                                                     ON.medicalExpenses =
                                                        medicalExpenses{ON.line37_income =
                                                                        t1.page4.line_23600_NetIncome}},
                                   ON.partC = partC{ON.line59_copy = t1.page7.partC_NetFederalTax.line40427}}})
