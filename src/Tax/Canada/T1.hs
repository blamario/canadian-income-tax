{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The T1 forms look similar, but there are subtle differences between different provinces and
-- territories. Therefore they share the same 'T1' form type and the same 'fixT1' completion function, but field
-- paths are separately provided by 't1FieldsForProvince'.
module Tax.Canada.T1 (examine, fixT1, fileNameForProvince, formPrefixForProvince, t1FieldsForProvince,
                      module Tax.Canada.T1.Types) where

import Data.CAProvinceCodes qualified as Province
import Control.Monad (guard)
import Data.Enum.Memo (memoize)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as Text

import Tax.FDF (FieldConst)
import Tax.Canada.FormKey qualified as FormKey
import Tax.Canada.Shared(Message(..), Severity(..), overLimitMessage)
import Tax.Canada.T1.Types
import Tax.Canada.T1.Fix (fixT1)
import Tax.Canada.T1.FieldNames.AB qualified as AB
import Tax.Canada.T1.FieldNames.BC qualified as BC
import Tax.Canada.T1.FieldNames.NB qualified as NB
import Tax.Canada.T1.FieldNames.NL qualified as NL
import Tax.Canada.T1.FieldNames.NT qualified as NT
import Tax.Canada.T1.FieldNames.NU qualified as NU
import Tax.Canada.T1.FieldNames.ON qualified as ON
import Tax.Canada.T1.FieldNames.QC qualified as QC
import Tax.Canada.T1.FieldNames.YT qualified as YT

-- | Reports summary and potential problems from input and output T1 forms
-- | Given the original and filled-in federal forms, return a list of observations for the user
examine :: T1 Maybe -> T1 Maybe -> [Message]
examine inputs outputs = catMaybes [
  guard (isNothing inputs.page1.identification.dateBirth)
  *> Just Message{
    severity = Notice,
    line = "Date of birth",
    form = FormKey.T1,
    explanation= "You have not entered your date of birth. I'll assume you were under 65 years old."},
  guard (isNothing inputs.page1.identification.maritalStatus)
  *> Just Message{
    severity = Notice,
    line = "Marital status",
    form = FormKey.T1,
    explanation= "You have not entered your marital status.\
                 \ I'll assume you were single, with no spouse or common-law partner credits."},
  guard (isNothing outputs.page3.line_15000_TotalIncome)
  *> Just Message{
    severity = Warning,
    line = "15000",
    form = FormKey.T1,
    explanation= "You've reported no income to tax in Step 2 of the T1 form."},
  guard (isJust outputs.page3.line_10100_EmploymentIncome)
  *> guard (isNothing outputs.page6.line_30800 || isNothing outputs.page6.line_31200)
  *> Just Message{
    severity = Warning,
    line = if isNothing outputs.page6.line_30800 then "30800" else "31200",
    form = FormKey.T1,
    explanation= "You have reported employment income on line 15000 but no "
      <> if isNothing outputs.page6.line_30800 then "CPP contributions on line 30800"
         else "EI contributions on line 31200"},
  overLimitMessage 1074 outputs.page4.line_22215_DeductionCPP_QPP "22215" FormKey.T1
    "Deduction for CPP or QPP enhanced contributions on employment income",
  overLimitMessage 16_129 outputs.page5.partB_FederalTaxCredits.line_30000 "30000" FormKey.T1 "Basic personal amount",
  overLimitMessage 9028 outputs.page5.partB_FederalTaxCredits.line_30100 "30100" FormKey.T1 "Age amount",
  overLimitMessage 1077.48 outputs.page6.line_31200 "31200" FormKey.T1 "EI contributions",
  overLimitMessage 10_000 outputs.page6.line_31270 "31270" FormKey.T1 "Home buyers' amount",
  overLimitMessage 20_000 outputs.page6.line_31285 "31285" FormKey.T1 "Home accessibility expenses",
  overLimitMessage 2000 outputs.page6.line_31400 "31400" FormKey.T1 "Pension income amount",
  overLimitMessage 650 outputs.page7.partC_NetFederalTax.line_41000 "41000" FormKey.T1
    "Federal political contribution tax credit",
  overLimitMessage 1000 outputs.page8.step6_RefundOrBalanceOwing.line_46900 "46900" FormKey.T1
    "School supplies expenses",
  Just Message{
      severity = Summary,
      line = if isJust outputs.page8.line_48400_Refund then "48400"
             else if isJust outputs.page8.line_48500_BalanceOwing then "48500"
             else "167",
      form = FormKey.T1,
      explanation = "You " <> (if isJust outputs.page8.line_48400_Refund then "have refund" else "owe balance")
        <> " of "
        <> Text.show (abs $ fromMaybe 0 outputs.page8.step6_RefundOrBalanceOwing.line164_Refund_or_BalanceOwing)
        <> " dollars"}]

fileNameForProvince :: Province.Code -> Text
fileNameForProvince p = formPrefixForProvince p <> "-r-fill-24e"

-- | The distinct provincial prefix of the T1 form PDF file, such as @5006@ in @5006-r-fill-23e.pdf@ for Ontario
formPrefixForProvince :: Province.Code -> Text
formPrefixForProvince = memoize $ \case
   Province.AB -> "5015"
   Province.BC -> "5010"
   Province.MB -> "5015"
   Province.NB -> "5000"
   Province.NL -> "5001"
   Province.NS -> "5015"
   Province.NT -> "5012"
   Province.NU -> "5014"
   Province.ON -> "5006"
   Province.PE -> "5000"
   Province.QC -> "5005"
   Province.SK -> "5015"
   Province.YT -> "5011"

-- | T1 field paths for the given province
t1FieldsForProvince :: Province.Code -> T1 FieldConst
t1FieldsForProvince = memoize $ \case
   Province.AB -> AB.t1Fields
   Province.BC -> BC.t1Fields
   Province.MB -> AB.t1Fields
   Province.NB -> NB.t1Fields
   Province.NL -> NL.t1Fields
   Province.NS -> AB.t1Fields
   Province.NT -> NT.t1Fields
   Province.NU -> NU.t1Fields
   Province.ON -> ON.t1Fields
   Province.PE -> NB.t1Fields
   Province.QC -> QC.t1Fields
   Province.SK -> AB.t1Fields
   Province.YT -> YT.t1Fields
