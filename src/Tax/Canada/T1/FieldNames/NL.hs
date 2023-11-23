{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.T1.FieldNames.NL (t1Fields) where

import Rank2 qualified

import Tax.FDF (FieldConst (Field, NoField), Entry (..), within)
import Tax.Canada.Shared (TaxIncomeBracket(timesRate))
import Tax.Canada.T1.Types
import Tax.Canada.T1.Types qualified as Page8 (Page8(..))
import Tax.Canada.T1.FieldNames.ON
  hiding (t1Fields,
          page1Fields, page2Fields, page3Fields, page4Fields, page5Fields, page6Fields, page7Fields, page8Fields,
          page1IdentificationFields,
          step4Fields, partBFields, page6MedicalExpensesFields, partCFields, page7step6Fields, page8step6Fields,
          selfEmploymentFields, taxPreparerFields)
import Tax.Canada.T1.FieldNames.ON qualified as ON
import Tax.Canada.T1.FieldNames.AB qualified as AB

t1Fields :: T1 FieldConst
t1Fields = within "form1" Rank2.<$> T1 {
   page1 = within "Page1" Rank2.<$> page1Fields,
   page2 = within "Page2" Rank2.<$> page2Fields,
   page3 = within "Page3" Rank2.<$> page3Fields,
   page4 = within "Page4" Rank2.<$> page4Fields,
   page5 = within "Page5" Rank2.<$> page5Fields,
   page6 = within "Page6" . within "PartB" Rank2.<$> page6Fields,
   page7 = within "Page7" Rank2.<$> page7Fields,
   page8 = within "Page8" Rank2.<$> page8Fields}

page1Fields = Page1 {
   identification = within "Identification" Rank2.<$> page1IdentificationFields,
   residence = within "Residence_Info" Rank2.<$> page1ResidenceFields,
   spouse = within "Info_Spouse_CLP" Rank2.<$> page1SpouseFields}

page1IdentificationFields = ON.page1IdentificationFields{
   dateBirth = Field ["DateYYYYMMDD_Comb_BordersAll", "DateYYYYMMDD_Comb"] Date,
   dateDeath = Field ["DateYYYYMMDD_Comb_BordersAll1", "DateYYYYMMDD_Comb1"] Date,
   maritalStatus = Field ["MaritalStatus_Checkbox"] $ RadioButtons 1 2 "MaritalStatus1" [Married .. Single]}

page2Fields = AB.page2Fields {
   foreign_property = Field ["Foreign_property", "Line26600"] $ Switch' "ForeignProperty_CheckBox",
   tax_exempt = Field ["Indian_Act", "IndianAct_Question", "Checkbox"] Checkbox}

page3Fields = ON.page3Fields {
   line_12000_TaxableDividends = Field ["Line_12000_TaxableDividends", "Line_12000_Amount"] Amount,
   line_12599_12600_RentalIncome = Field ["Line13", "Line12599", "Line_12599_Amount"] Amount,
   line_12600_Amount = Field ["Line13", "Line_12600_Amount"] Amount,
   line_12799_Amount = Field ["Line15", "Line12799", "Line_12799_Amount"] Amount,
   line_12800_Amount = Field ["Line15", "Line_12800_Amount"] Amount,
   line_13010_Taxablescholarship = Field ["Line_13010", "Line13010_Amount"] Amount,
   line_19 = Field ["Line_19", "Line19_Amount"] Amount,
   selfEmployment = selfEmploymentFields,
   line_25_sum = Field ["Line_25", "I1", "Amount1"] Amount,
   line_25_cont = Field ["Line_25", "I2", "Amount2"] Amount,
   line_26 = Field ["Line_26", "Line26_Amount"] Amount,
   line_14400_WorkersCompBen = Field ["Line_27", "Line_14400_Amount"] Amount,
   line_14500_SocialAssistPay = Field ["Line_28", "Line_14500_Amount"] Amount,
   line_14600_NetFedSupplements = Field ["Line_29", "Line_14600_Amount"] Amount,
   line_14700_EqualsAmount = Field ["Line_30", "I1", "Amount1"] Amount,
   line_14700_PlusAmount = Field ["Line_30", "I2", "Amount2"] Amount,
   line_15000_TotalIncome = Field ["Line_31", "Line_15000_Amount"] Amount}

selfEmploymentFields = SelfEmploymentIncome {
   line_13499_Amount = Field ["Line_20", "Line_13499", "Line13499_Amount"] Amount,
   line_13500_Amount = Field ["Line_20", "Line13500_Amount"] Amount,
   line_13699_Amount = Field ["Line_21", "Line_13699", "Line13699_Amount"] Amount,
   line_13700_Amount = Field ["Line_21", "Line13700_Amount"] Amount,
   line_13899_Amount = Field ["Line_22", "Line_13899", "Line13899_Amount"] Amount,
   line_13900_Amount = Field ["Line_22", "Line_13900_Amount"] Amount,
   line_14099_Amount = Field ["Line_23", "Line_14099", "Line_14099_Amount"] Amount,
   line_14100_Amount = Field ["Line_23", "Line_14100_Amount"] Amount,
   line_14299_Amount = Field ["Line_24", "Line_14299", "Line_14299_Amount"] Amount,
   line_14300_Amount = Field ["Line_24", "Line_14300_Amount"] Amount}

page4Fields = ON.page4Fields{
   line_15000_TotalIncome_2 = Field ["Line_32", "Line32_Amount"] Amount,
   line_21699_Amount = Field ["Line40", "Line_21699", "Line_21699_Amount"] Amount,
   line_21999_Amount = Field ["Line42", "Line_21999", "Line_21999_Amount"] Amount,
   line_23210 = Field ["Line_23210_Covid", "Line_23210_Amount"] Amount}

page5Fields = ON.page5Fields{
   step4_TaxableIncome = step4Fields,
   partA_FederalTax = ON.page5Fields.partA_FederalTax {
      column1 = ON.page5Fields.partA_FederalTax.column1 {
         timesRate = Field ["PartA", "Column1", "Line40Amount2"] Amount}},
   partB_FederalTaxCredits = within "PartB" Rank2.<$> partBFields}

step4Fields = ON.step4Fields {
   line_23600_NetIncome_2 = Field ["Line_23600_TotalIncome_2", "Line_15000_Amount"] Amount}

partBFields = ON.partBFields {
   line30499_ChildrenNum = Field ["Line30500", "Line30499", "Line7_ChildrenNum"] Count,
   line_81 = Field ["Line_78", "Line30_Amount"] Amount}

page6Fields = ON.page6Fields {
   line82 = Field ["Line_82", "Line43Amount"] Amount,
   line30800 = Field ["Line30800_Sub", "Line8_Amount"] Amount,
   line31000 = Field ["Line31000_Sub", "Line9_Amount"] Amount,
   line31200 = Field ["Line31200_Sub", "Line10_Amount"] Amount,
   line31350 = Field ["Line31350_Sub", "Line24_Amount"] Amount,
   line94_sum = Field ["Line94", "Amount1"] Amount,
   line94_cont = Field ["Line94", "Amount2"] Amount,
   line96 = Field ["Line96", "Amount"] Amount,
   line99 = Field ["Line99", "Amount"] Amount,
   line104 = Field ["Line104", "Amount"] Amount,
   medical_expenses = page6MedicalExpensesFields,
   line112 = Field ["Line112", "Rate"] $ Constant 0.15 Percent}

page6MedicalExpensesFields = ON.page6MedicalExpensesFields {
   familyExpenses = Field ["Line33099", "Amount"] Amount,
   taxableIncome = Field ["Line106", "Amount1"] Amount,
   taxableIncomeFraction = Field ["Line106", "Amount2"] Amount,
   threshold = Field ["Line107", "Amount"] Amount,
   difference = Field ["Line108", "Amount"] Amount}

page7Fields = Page7 {
   partC_NetFederalTax = within "PartC" Rank2.<$> partCFields,
   step6_RefundOrBalanceOwing = within "Step6" Rank2.<$> page7step6Fields}

partCFields = ON.partCFields {
   line116 = Field ["Line116", "Line43Amount"] Amount,
   line40400 = Field ["Line40400_Sub", "Line45Amount2"] Amount,
   line122_sum = Field ["Line122_sub", "Line49Amount1"] Amount,
   line122_cont = Field ["Line122_sub", "Line49Amount2"] Amount,
   line42900 = Field ["Line123", "Amount"] Amount,
   line124 = Field ["Line124", "Amount"] Amount,
   line125 = Field ["Line125", "Amount"] Amount,
   line127 = Field ["Line40600_sub", "Line52Amount"] Amount,
   line128 = Field ["Line128", "Amount"] Amount,
   line129 = Field ["Line129", "Amount"] Amount,
   line130 = Field ["Line130", "Amount"] Amount,
   line40600 = Field ["Line131", "Amount"] Amount,
   line40900 = Field ["Line132", "Line40900", "Amount"] Amount,
   line41000 = Field ["Line132", "Amount"] Amount,
   line41200 = Field ["Line133", "Line55Amount"] Amount,
   line41300 = Field ["Line134", "Line41300", "Amount"] Amount,
   line41400 = Field ["Line134", "Amount"] Amount,
   line41800 = Field ["Line41800_sub", "Line60Amount"] Amount,
   line42000 = Field ["Line42000_sub", "Line61Amount"] Amount}

page7step6Fields = ON.page7step6Fields {
   line140 = Field ["Line_140", "Line_42120_Amount"] Amount}

page8Fields = ON.page8Fields {
   Page8.step6_RefundOrBalanceOwing = page8step6Fields,
   line48400_Refund = Field ["Lines_48400-48500", "Line_48400_Refund", "Line_48400_Amount"] Amount,
   line48500_BalanceOwing = Field ["Lines_48400-48500", "Line_48500_BalanceOwing", "Line_48500_Amount"] Amount,
   telephone = Field ["Certification", "Telephone"] Amount,
   date = Field ["Certification", "Date"] Amount,
   taxPreparer = within "Line_49000_IfFeeWasCharged" Rank2.<$> taxPreparerFields,
   line_1_ONOpportunitiesFund = NoField,
   line_46500 = NoField,
   line_46600 = NoField}

page8step6Fields = ON.page8step6Fields {
   line_43500_totalpayable = Field ["Line_146", "Line_42000_Amount"] Amount,
   line_43700_Total_income_tax_ded = Field ["Line_43700_TotalIncomeTaxDeducted", "Line_43700_Amount"] Amount,
   line_44000 = Field ["Line_44000_QuebecAbatement", "Line_44000_Amount"] Amount,
   line_45000_EIOverpayment = Field ["Line_45000_EmploymentInsurance", "Line_45000_Amount"] Amount,
   line_45300_CWB = Field ["Line_45300_WITB", "Line_45300_Amount"] Amount,
   line_45700_GST_HST_Rebate = Field ["Line_45700_GST_HST_rebate", "Line_45700_Amount"] Amount,
   line_46800 = Field ["Line157", "Line46800", "Line_46800_Amount"] Amount,
   line_46900 = Field ["Line157", "Line_46900_Amount"] Amount,
   line_47557 = Field ["Line_47557_AirQuality", "Amount"] Amount,
   line164_Refund_or_BalanceOwing = Field ["Line162", "Refund_or_BalanceOwing_Amount"] Amount}

taxPreparerFields = ON.taxPreparerFields {
   eFileNumber = Field ["Line48900", "EFile"] Textual,
   line49000_WasAFeeCharged = Field ["Line49000", "Line49000_CheckBoxGroup"] $ Switch' "Line49000_CheckBox_EN"}
