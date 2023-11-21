{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.T1.FieldNames.NT (module Tax.Canada.T1.FieldNames.NT, page1Fields) where

import Rank2 qualified

import Tax.FDF (FieldConst (Field, NoField), Entry (..), within)
import Tax.Canada.T1.Types
import Tax.Canada.T1.Types qualified as Page8 (Page8(..))
import Tax.Canada.T1.FieldNames
  hiding (t1Fields,
          page2Fields, page3Fields, page4Fields, page5Fields, page6Fields, page7Fields, page8Fields,
          step4Fields, partBFields, page6MedicalExpensesFields, partCFields, page7step6Fields, page8step6Fields,
          selfEmploymentFields, taxPreparerFields)
import Tax.Canada.T1.FieldNames.AB qualified as AB
import Tax.Canada.T1.FieldNames.BC qualified as BC
import Tax.Canada.T1.FieldNames qualified as ON

t1Fields :: T1 FieldConst
t1Fields = within "form1" Rank2.<$> T1 {
   page1 = within "Page1" . within "Step1" Rank2.<$> ON.page1Fields,
   page2 = within "Page2" Rank2.<$> BC.page2Fields,
   page3 = within "Page3" . within "Step2" Rank2.<$> page3Fields,
   page4 = within "Page4" Rank2.<$> page4Fields,
   page5 = within "Page5" Rank2.<$> page5Fields,
   page6 = within "Page6" . within "PartB" Rank2.<$> page6Fields,
   page7 = within "Page7" Rank2.<$> page7Fields,
   page8 = within "Page8" Rank2.<$> page8Fields}

page3Fields = ON.page3Fields{
   line_12000_TaxableDividends = Field ["Line_9", "Line_12000_Amount"] Amount,
   line_13010_Taxablescholarship = Field ["Line_13010_Taxablescholarship", "Line_13010_Amount"] Amount,
   line_19 = Field ["Line_19", "Amount"] Amount,
   selfEmployment = within "SelfEmploymentIncome" Rank2.<$> BC.selfEmploymentFields,
   line_25_sum = Field ["Line_25", "Amount1"] Amount,
   line_25_cont = Field ["Line_25", "Amount2"] Amount,
   line_26 = Field ["Line_26", "Amount"] Amount}

page4Fields = BC.page4Fields{
   line_15000_TotalIncome_2 = ON.page4Fields.line_15000_TotalIncome_2,
   line_21699_Amount = Field ["Line_21700_BusinessLoss", "Line_21699_Amount"] Amount,
   line_21700_Amount = Field ["Line_21700_BusinessLoss", "Line_21700_Amount"] Amount,
   line_21999_Amount = Field ["Line_22000_SupportPayments", "Line_21999_Amount"] Amount,
   line_22000_Amount = Field ["Line_22000_SupportPayments", "Line_22000_Amount"] Amount,
   line_23600_NetIncome = Field ["Line_23600", "Line_23600_Amount"] Amount}

page5Fields = ON.page5Fields{
   step4_TaxableIncome = within "Step4" . within "TaxableIncome" Rank2.<$> step4Fields,
   partA_FederalTax = within "Step5" . within "PartA" . within "Chart" Rank2.<$> ON.partAFields "Col" 36,
   partB_FederalTaxCredits = within "Step5" . within "PartB" Rank2.<$> partBFields}

step4Fields = ON.step4Fields{
   line_23600_NetIncome_2 = Field ["Line_23600_TotalIncome_2", "Line_15000_Amount"] Amount,
   line_24400_MilitaryPoliceDeduction = Field ["Line_24400", "Line_24400_Amount"] Amount}

partBFields = ON.partBFields {
   line30499_ChildrenNum = Field ["Line30500_Sub", "Line30499", "Line7_ChildrenNum"] Count,
   line30500 = Field ["Line30500_Sub", "Line7_Amount"] Amount}

page6Fields = ON.page6Fields {
   line82 = Field ["Line_82", "Line43Amount"] Amount,
   line30800 = Field ["Line30800_Sub", "Line8_Amount"] Amount,
   line31000 = Field ["Line31000_Sub", "Line9_Amount"] Amount,
   line31200 = Field ["Line31200_Sub", "Line10_Amount"] Amount,
   line31350 = Field ["Line31350_Sub", "Line24_Amount"] Amount,
   line94_sum = Field ["Line94", "Line30_Amoun1"] Amount,
   line94_cont = Field ["Line94", "Line30_Amount2"] Amount,
   line96 = Field ["Line96", "Line30_Amount"] Amount,
   line99 = Field ["Line99", "Line30_Amount"] Amount,
   line104 = Field ["Line104", "Line30_Amount"] Amount,
   medical_expenses = page6MedicalExpensesFields,
   line112 = Field ["Line112", "Line31_Rate"] $ Constant 0.15 Percent}

page6MedicalExpensesFields = AB.page6MedicalExpensesFields {
   familyExpenses = Field ["Line33099_Sub", "Line25_Amount"] Amount,
   otherDependants = Field ["Line33199_Sub", "Line28_Amount"] Amount}

page7Fields = Page7 {
   partC_NetFederalTax = within "PartC" Rank2.<$> partCFields,
   step6_RefundOrBalanceOwing = within "Step7" Rank2.<$> BC.page7step6Fields}

partCFields = BC.partCFields {
   line40424 = Field ["Line40424_Sub", "Line44Amount"] Amount,
   line40400 = Field ["Line40400_Sub", "Line45Amount1"] Amount,
   line42900 = Field ["Line123", "Line50Amount"] Amount,
   line124 = Field ["Line124", "Line50Amount"] Amount,
   line40600 = Field ["Line40600_Sub", "Amount"] Amount,
   line40900 = Field ["Line41000_Sub", "Line53Amount"] Amount,
   line41000 = Field ["Line41000_Sub", "Line54Amount"] Amount,
   line41300 = Field ["Line41400_sub", "Line56Amount1"] Amount,
   line41400 = Field ["Line41400_sub", "Line56Amount2"] Amount}

page8Fields = AB.page8Fields {
   Page8.step6_RefundOrBalanceOwing = page8step6Fields,
   telephone = Field ["Certification", "Telephone"] Amount,
   date = Field ["Certification", "Date"] Amount,
   taxPreparer = within "Efile" Rank2.<$> taxPreparerFields,
   line_1_ONOpportunitiesFund = NoField,
   line_46500 = NoField,
   line_46600 = NoField}

page8step6Fields = BC.page8step6Fields {
   line_46800 = Field ["Line46900", "Line46800", "Line_46800_Amount"] Amount,
   line_46900 = Field ["Line46900", "Line_46900_Amount"] Amount,
   line164_Refund_or_BalanceOwing = Field ["Line164", "Amount"] Amount}

taxPreparerFields = ON.taxPreparerFields {
   eFileNumber = Field ["Line48900", "EFile"] Textual,
   line49000_WasAFeeCharged = Field ["Line49000", "Line49000_CheckBoxGroup"] $ Switch' "Line49000_CheckBox_EN"}
