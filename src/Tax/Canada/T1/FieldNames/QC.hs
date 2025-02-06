{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.T1.FieldNames.QC (t1Fields) where

import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Rank2 qualified

import Tax.FDF (FieldConst (Field, NoField), Entry (..), within)
import Tax.Canada.Shared (TaxIncomeBracket (..), subCalculationFields)
import Tax.Canada.Shared qualified as TaxIncomeBracket (TaxIncomeBracket (..))
import Tax.Canada.T1.Types
import Tax.Canada.T1.Types qualified as Page8 (Page8(..))
import Tax.Canada.T1.Types qualified as MedicalExpenses (MedicalExpenses(..))
import Tax.Canada.T1.FieldNames.ON qualified as ON
import Tax.Canada.T1.FieldNames.BC qualified as BC

t1Fields :: T1 FieldConst
t1Fields = within "form1" Rank2.<$> T1 {
   page1 = within "Page1" . within "Return-pg1" Rank2.<$> ON.page1Fields,
   page2 = within "Page2" . within "Return-pg2" Rank2.<$> page2Fields,
   page3 = within "Page3" . within "Return-pg3" Rank2.<$> ON.page3Fields,
   page4 = within "Page4" . within "Step3" Rank2.<$> page4Fields,
   page5 = within "Page5" Rank2.<$> page5Fields,
   page6 = within "Page6" . within "PartB" Rank2.<$> page6Fields,
   page7 = within "Page7" Rank2.<$> page7Fields,
   page8 = within "Page8" Rank2.<$> page8Fields}

page2Fields = ON.page2Fields {
   cai = NoField,
   organ_donor = NoField}

page4Fields = ON.page4Fields{
   line_20810_PRPP = Field ["Line20810", "Amount"] Amount,
   line_21698_Amount = Field ["Line45", "Line21698", "Line_21699_Amount"] Amount,
   line_22300_DeductionPPIP = Field ["Line22300", "Line_22300_Amount"] Amount}

page5Fields = Page5 {
   step4_TaxableIncome = within "Step4" Rank2.<$> step4Fields,
   partA_FederalTax = within "Part_A" Rank2.<$> partA1{column4 = column4, column5 = partA2.column5},
   partB_FederalTaxCredits = within "Part_B" Rank2.<$> partBFields}
   where partA1 = ON.partAFieldsWith fieldName1 "Column" 71
         partA2 = ON.partAFieldsWith fieldName2 "Column" 39
         column4 = partA2.column4{
           TaxIncomeBracket.income = partA1.column4.income,
           TaxIncomeBracket.threshold = partA1.column4.threshold}
         fieldName1 line _column True = "Percent_Line" <> toText (decimal line)
         fieldName1 line _column False = "Amount_Line" <> toText (decimal line)
         fieldName2 line column isRate =
            toText $ "Line" <> decimal line <> (if isRate then "Rate" else "Amount") <> decimal column
         toText = toStrict . toLazyText

step4Fields = ON.step4Fields {
   line_23600_NetIncome_2 = Field ["Line60", "Amount"] Amount,
   line_24901_SecurityDeductions = Field ["Line24901", "Line_24901_Amount"] Amount,
   line_25395_BusinessTransfer = Field ["Line25395", "Line_25400_Amount"] Amount,
   line72_difference = Field ["Line73", "Line_26000_Amount"] Amount,
   line_25999_CapitalGainsReductionAddBack = Field ["Line25999", "Line_25999_Amount"] Amount}

partBFields = ON.partBFields {
   line_30500 = Field ["Line30500", "Line_30499_Amount"] Amount,
   pageBreakSummary = Field ["Line90", "Amount"] Amount}

page6Fields = ON.page6Fields {
   pageBreakCarry = Field ["Line91", "Amount"] Amount,
   line_31205 = Field ["Line31205", "Line_31205_Amount"] Amount,
   line_31210 = Field ["Line31210", "Line_31210_Amount"] Amount,
   line_31215 = Field ["Line31215", "Line_31215_Amount"] Amount,
   line102_sum = subCalculationFields "Line106" ["Amount1"] ["Amount2"],
   line_31600 = Field ["Line31600", "Line_31600_mount"] Amount,
   line104_sum = Field ["Line108", "Amount"] Amount,
   line107_sum = Field ["Line111", "Amount"] Amount,
   line112_sum = Field ["Line116", "Amount"] Amount,
   medical_expenses = page6MedicalExpensesFields,
   line_33200_sum = subCalculationFields "Line33200" ["Line_33200_Amount1"] ["Line_33200_Line32Amount2"],
   line120_taxCreditRate = Field ["Line124", "Percent"] $ Constant 0.15 Percent}

page6MedicalExpensesFields = ON.page6MedicalExpensesFields {
   taxableIncome = Field ["Line118", "Amount1"] Amount,
   taxableIncomeFraction = Field ["Line118", "Amount2"] Amount,
   MedicalExpenses.threshold = Field ["Line119", "Amount"] Amount,
   difference = Field ["Line120", "Amount"] Amount}

page7Fields = Page7 {
   partC_NetFederalTax = within "PartC" Rank2.<$> partCFields,
   step6_RefundOrBalanceOwing = within "Step6" Rank2.<$> page7step6Fields}

partCFields = ON.partCFields {
   tax_copy = Field ["Line128", "Amount"] Amount,
   credits_copy = Field ["Line131", "Amount"] Amount,
   line130_sum = subCalculationFields "Line134" ["Amount1"] ["Amount2"],
   line132_foreignSurtax = Field ["Line136", "Amount"] Amount,
   line133_sum = Field ["Line137", "Amount"] Amount,
   line135_difference = Field ["Line139", "Amount"] Amount,
   line136_recapture = Field ["Line140", "Amount"] Amount,
   line137_sum = Field ["Line141", "Amount"] Amount,
   line138_logging = Field ["Line142", "Amount"] Amount}

page7step6Fields = ON.page7step6Fields {
   tax_copy = Field ["Line152", "Amount"] Amount,
   line_42100_CPPContributions = NoField}

page8Fields = ON.page8Fields {
   Page8.step6_RefundOrBalanceOwing = within "Step6-Continued" Rank2.<$> page8step6Fields,
   line1_ONOpportunitiesFund = NoField,
   line_46500 = NoField,
   line_46600 = NoField,
   line_48400_Refund = Field ["Refund_or_Balancing-owing", "Line48400", "Line_48400_Amount"] Amount,
   line_48500_BalanceOwing = Field ["Refund_or_Balancing-owing", "Line48500", "Line_48500_Amount"] Amount}

page8step6Fields = ON.page8step6Fields {
   line_43500_totalpayable = Field ["Line157", "Amount"] Amount,
   line_43800_TaxTransferQC = Field ["Line43800", "Line_43800_Amount"] Amount,
   line_43850_diff = subCalculationFields "Line43900" ["Line_43900_Amount1"] ["Line_43900_Amount2"],
   line_42900_copy = Field ["Line44000", "Line_44000_Amount1"] Amount,
   line_44000 = Field ["Line44000", "Line_44000_Amount2"] Amount,
   line_44800_CPPOverpayment = NoField,
   line_31210_copy = Field ["Line163", "Amount"] Amount,
   line_45100_diff = subCalculationFields "Line45100" ["Line_45100_Amount1"] ["Line_45100_Amount2"],
   line_45350_CTC = Field ["Line45350", "Line_45300_Amount"] Amount,
   line_47555_TaxPaid = Field ["Line47555", "Line_47555_Amount"] Amount,
   line_47900_ProvTerrCredits = NoField,
   line164_Refund_or_BalanceOwing = Field ["Line177", "Refund_or_BalanceOwing_Amount"] Amount}
