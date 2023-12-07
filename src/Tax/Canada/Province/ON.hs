{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Tax.Canada.Province.ON (ON428, fixON428, fixON479, fixReturns, on428Fields, on479Fields, t1Fields) where

import Tax.Canada.T1.Types qualified as T1
import Tax.Canada.T1.Types (T1 (T1, page7, page8), Page7(Page7, step6_RefundOrBalanceOwing),
                            Page8(Page8, step6_RefundOrBalanceOwing))
import Tax.Canada.T1.FieldNames.ON (t1Fields)
import Tax.Canada.T1.Fix (fixT1)

import Tax.Canada.Province.ON.ON428.Types qualified as ON
import Tax.Canada.Province.ON.ON428.Types qualified as ON.Page1 (Page1(..))
import Tax.Canada.Province.ON.ON428.Types qualified as ON.Page2 (Page2(..))
import Tax.Canada.Province.ON.ON428.Types (ON428 (ON428))
import Tax.Canada.Province.ON.ON428.Fix (fixON428)
import Tax.Canada.Province.ON.ON428.FieldNames (on428Fields)
import Tax.Canada.Province.ON.ON479.Types (ON479 (ON479))
import Tax.Canada.Province.ON.ON479.Fix (fixON479)
import Tax.Canada.Province.ON.ON479.FieldNames (on479Fields)

import Tax.Canada.Shared(MedicalExpenses(..), BaseCredit(..))
import Tax.Util (fixEq)

fixReturns :: (T1 Maybe, ON428 Maybe) -> (T1 Maybe, ON428 Maybe)
fixReturns =
  fixEq $ \(t1@T1{page7 = page7@Page7{step6_RefundOrBalanceOwing}},
            on428@ON428{page1 = page1@ON.Page1{partB = partB1@ON.Page1PartB{spouseAmount}},
                        page2 = page2@ON.Page2{ON.partB = partB2@ON.Page2PartB{ON.medicalExpenses},
                                               ON.partC}})
          -> (fixT1 t1{page7 =
                       page7{step6_RefundOrBalanceOwing =
                             step6_RefundOrBalanceOwing{T1.line_42800_ProvTerrTax = on428.page4.line90}}},
              fixON428 on428{ON.page1 =
                             page1{ON.line1 = t1.page5.step4_TaxableIncome.line_26000_TaxableIncome,
                                   ON.Page1.partB = partB1{ON.spouseAmount = spouseAmount{baseAmount = t1.page1.spouse.line23600},
                                                           ON.line19_cppQpp = t1.page6.line30800,
                                                           ON.line20_cppQpp = t1.page6.line31000,
                                                           ON.line21_employmentInsurance = t1.page6.line31200,
                                                           ON.line22_employmentInsurance = t1.page6.line31217}},
                             ON.page2 =
                             page2{ON.Page2.partB = partB2{ON.line32_interest = t1.page6.line31900,
                                                           ON.medicalExpenses =
                                                           medicalExpenses{
                                                              netIncome = t1.page4.line_23600_NetIncome}},
                                   ON.partC = partC{ON.line59_copy = t1.page7.partC_NetFederalTax.line40427}}})
