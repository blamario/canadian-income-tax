{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.Province.MB (MB428, formFileNames, fixMB428, fixReturns, mb428Fields, returnFields, t1Fields) where

import qualified Rank2

import Data.CAProvinceCodes (Code(MB))
import Data.Map (Map, fromList)
import Data.Text (Text)

import Tax.Canada.Federal qualified as Federal
import Tax.Canada.Federal (Forms(t1), fixFederalForms)
import Tax.Canada.Federal.Schedule9 qualified as Schedule9
import Tax.Canada.FormKey (FormKey)
import Tax.Canada.FormKey qualified as FormKey
import Tax.Canada.T1.Types (T1 (T1, page7), Page7(Page7, step6_RefundOrBalanceOwing))
import Tax.Canada.T1.Types qualified as T1
import Tax.Canada.T1.FieldNames.AB (t1Fields)  -- same T1 form as Alberta

import Tax.Canada.Province.MB.MB428.Types qualified as MB
import Tax.Canada.Province.MB.MB428.Types qualified as MB.Page1 (Page1(..))
import Tax.Canada.Province.MB.MB428.Types qualified as MB.Page2 (Page2(..))
import Tax.Canada.Province.MB.MB428.Types (MB428 (MB428))
import Tax.Canada.Province.MB.MB428.Fix (fixMB428)
import Tax.Canada.Province.MB.MB428.FieldNames (mb428Fields)

import Tax.Canada.Shared(MedicalExpenses(..), BaseCredit(..))
import Tax.FDF (FieldConst, within)
import Tax.Util (fixEq)

import Data.Functor.Product (Product(Pair))

type Returns = Product Federal.Forms MB428

fixReturns :: Federal.InputForms Maybe -> Returns Maybe -> Returns Maybe
fixReturns inputs =
  fixEq $ \(Pair ff@Federal.Forms{t1 = t1@T1{page7 = page7@Page7{step6_RefundOrBalanceOwing}},
                                  schedule9}
                 mb428@MB428{page1 = page1@MB.Page1{partB = partB1@MB.Page1PartB{spouseAmount}},
                             page2 = page2@MB.Page2{MB.partB = partB2@MB.Page2PartB{MB.medicalExpenses}},
                             page3 = page3@MB.Page3{MB.partC}})
          -> Pair (fixFederalForms MB inputs $
                   ff{t1 = t1{page7 =
                              page7{step6_RefundOrBalanceOwing =
                                    step6_RefundOrBalanceOwing{T1.line_42800_ProvTerrTax =
                                                               mb428.page3.partC.line82_tax}}}})
                  (fixMB428 mb428{MB.page1 =
                                  page1{MB.Page1.income = t1.page5.step4_TaxableIncome.line_26000_TaxableIncome,
                                        MB.Page1.partB = partB1{MB.spouseAmount =
                                                                   spouseAmount{reduction = t1.page1.spouse.line_23600},
                                                                MB.line19_cppQpp = t1.page6.line_30800,
                                                                MB.line20_cppQpp = t1.page6.line_31000,
                                                                MB.line21_employmentInsurance = t1.page6.line_31200,
                                                                MB.line22_employmentInsurance = t1.page6.line_31217}},
                                  MB.page2 =
                                  page2{MB.Page2.partB = partB2{MB.line37_interest = t1.page6.line_31900,
                                                                MB.medicalExpenses =
                                                                medicalExpenses{
                                                                   expenses = t1.page6.medical_expenses.familyExpenses,
                                                                   netIncome = t1.page4.line_23600_NetIncome},
                                                                MB.donations = partB2.donations{
                                                                   MB.line54_base = schedule9.page1.line13_min,
                                                                   MB.line55_base = schedule9.page1.line14_difference}}},
                                  MB.page3 =
                                  page3{MB.partC = partC{MB.line63_copy = t1.page7.partC_NetFederalTax.line_40427}}})

returnFields :: Returns FieldConst
returnFields = Pair (Federal.formFieldsForProvince MB) (within "Provincial428" Rank2.<$> mb428Fields)

formFileNames :: Map FormKey Text
formFileNames = fromList [
  (FormKey.T1, "T1/5015-r"),
  (FormKey.Provincial428, "428/5007-c")]
