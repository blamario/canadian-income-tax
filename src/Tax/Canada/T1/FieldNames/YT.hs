{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.T1.FieldNames.YT (t1Fields) where

import Rank2 qualified

import Tax.FDF (FieldConst (Field, NoField), Entry (..), within)
import Tax.Canada.Shared (subCalculationFields)
import Tax.Canada.T1.Types
import Tax.Canada.T1.Types qualified as Page8 (Page8(..))
import Tax.Canada.T1.FieldNames.BC qualified as BC
import Tax.Canada.T1.FieldNames.NT qualified as NT
import Tax.Canada.T1.FieldNames.ON qualified as ON

t1Fields :: T1 FieldConst
t1Fields = NT.t1Fields{
   page1 = within "form1" . within "Page1" Rank2.<$> NT.page1Fields,
   page3 = within "form1" . within "Page3" Rank2.<$> NT.page3Fields,
   page5 = within "form1" . within "Page5" Rank2.<$> page5Fields,
   page6 = within "form1" . within "Page6" . within "PartB" Rank2.<$> page6Fields,
   page7 = within "form1" . within "Page7" Rank2.<$> page7Fields,
   page8 = within "form1" . within "Page8" Rank2.<$> page8Fields}

page5Fields = NT.page5Fields{
   partB_FederalTaxCredits = within "Step5" . within "PartB" Rank2.<$> partBFields}

partBFields = NT.partBFields {
   line30499_ChildrenNum = Field ["Line77", "Line7_ChildrenNum"] Count,
   line30500 = Field ["Line77", "Line7_Amount"] Amount,
   line_81 = Field ["Line_78", "Line30_Amount"] Amount}

page6Fields = NT.page6Fields {
   line82 = Field ["Line_79", "Line43Amount"] Amount,
   line94_sum = subCalculationFields "Line93" ["Line30_Amount1"] ["Line30_Amount2"],
   line96 = Field ["Line95", "Line30_Amount"] Amount,
   line104 = Field ["Line103", "Line30_Amount"] Amount,
   medical_expenses = page6MedicalExpensesFields,
   line112 = Field ["Line104_Sub", "Line31_Rate"] $ Constant 0.15 Percent}

page6MedicalExpensesFields = NT.page6MedicalExpensesFields {
   familyExpenses = Field ["Line33099", "Line25_Amount"] Amount,
   threshold = Field ["Line26_Sub", "Line26_Amount"] Amount,
   difference = Field ["Line27_Sub", "Line27_Amount"] Amount}

page7Fields = Page7 {
   partC_NetFederalTax = within "PartC" Rank2.<$> partCFields,
   step6_RefundOrBalanceOwing = within "Step6" Rank2.<$> page7step6Fields}

partCFields = ON.partCFields {
   line116 = Field ["Line108_sub", "Line43Amount"] Amount,
   line42900 = Field ["Line42900_sub", "Line50Amount"] Amount,
   line124 = Field ["Line123", "Line50Amount"] Amount,
   line128 = Field ["Line127", "Line50Amount"] Amount,
   line129 = Field ["Line128", "Line52Amount"] Amount,
   line130 = Field ["Line129", "Line50Amount"] Amount,
   line40900 = Field ["Line131", "Line40900", "Line53Amount"] Amount,
   line41000 = Field ["Line131", "Line54Amount"] Amount,
   line41300 = Field ["Line41400_sub", "Line41300", "Line56Amount1"] Amount,
   line41800 = Field ["Line41800_sub", "Line60Amount"] Amount,
   line42000 = Field ["Line42000_sub", "Line61Amount"] Amount}

page7step6Fields = ON.page7step6Fields {
   line140 = Field ["Line_140", "Line_42120_Amount"] Amount,
   line_43200_FirstNationsTax = Field ["Line_43200_YukonTax", "Line_42800_Amount"] Amount}

page8Fields = BC.page8Fields {
   Page8.step6_RefundOrBalanceOwing = within "RefundOrBalanceOwing" Rank2.<$> page8step6Fields,
   line48400_Refund = Field ["Line48400_48500", "Line48400", "Line_48400_Amount"] Amount,
   line48500_BalanceOwing = Field ["Line48400_48500", "Line48500", "Line_48500_Amount"] Amount}

page8step6Fields = BC.page8step6Fields{
   line_44100 = Field ["Line_44100_FederalAbatement", "Line_44000_Amount"] Amount,
   line_45300_CWB = Field ["Line_45300_CWB", "Line_45300_Amount"] Amount,
   line_45600_TrustTaxCredit = Field ["Line_45600_TrustTaxCredit", "Line_45600_Amount"] Amount,
   line_46800 = Field ["Line159", "sfline46800end", "Line_46800_Amount"] Amount,
   line_46900 = Field ["Line159", "Line_46900_Amount"] Amount,
   line_47556 = Field ["Line161", "Amount"] Amount,
   line164_Refund_or_BalanceOwing = Field ["Line166", "Refund_or_BalanceOwing_Amount"] Amount}
