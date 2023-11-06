{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.T1.FieldNames.BC where

import Rank2 qualified

import Tax.FDF (FieldConst (Field, NoField), Entry (..), within)
import Tax.Canada.T1.Types
import Tax.Canada.T1.FieldNames
  hiding (t1Fields,
          page2Fields, page3Fields, page4Fields, page5Fields, page6Fields, page7Fields, page8Fields,
          step4Fields, partBFields, page6MedicalExpensesFields, partCFields, page7step6Fields, page8step6Fields,
          selfEmploymentFields, taxPreparerFields)
import Tax.Canada.T1.FieldNames qualified as BaseNames

t1Fields :: T1 FieldConst
t1Fields = within "form1" Rank2.<$> T1 {
   page1 = within "Page1" Rank2.<$> page1Fields,
   page2 = within "Page2" Rank2.<$> page2Fields,
   page3 = within "Page3" . within "Step2" Rank2.<$> page3Fields,
   page4 = within "Page4" . within "Step3" Rank2.<$> page4Fields,
   page5 = within "Page5" Rank2.<$> page5Fields,
   page6 = within "Page6" . within "PartB" Rank2.<$> page6Fields,
   page7 = within "Page7" Rank2.<$> page7Fields,
   page8 = within "Page8" Rank2.<$> page8Fields}

page2Fields = BaseNames.page2Fields {
   cai = NoField,
   organ_donor = NoField}

page3Fields = BaseNames.page3Fields{
   line_12000_TaxableDividends = Field ["Line_12000_TaxableDividends", "Line_12000_Amount"] Amount,
   line_13010_Taxablescholarship = Field ["Line_13010", "Amount"] Amount,
   line_19 = Field ["Line19", "Amount"] Amount,
   line_25_sum = Field ["Line25", "Amount1"] Amount,
   line_25_cont = Field ["Line25", "Amount2"] Amount,
   line_26 = Field ["Line26", "Amount"] Amount,
   selfEmployment = selfEmploymentFields,
   line_14700_EqualsAmount = Field ["Line_14700", "Amount1"] Amount,
   line_14700_PlusAmount = Field ["Line_14700", "Amount2"] Amount}

selfEmploymentFields = SelfEmploymentIncome {
   line_13499_Amount = Field ["Line_13499_13500_BusinessIncome", "Line_13499_Amount"] Amount,
   line_13500_Amount = Field ["Line_13499_13500_BusinessIncome", "Line_13500_Amount"] Amount,
   line_13700_Amount = Field ["Line_13699_13700_ProfessionalIncome", "Line_13700_Amount"] Amount,
   line_13699_Amount = Field ["Line_13699_13700_ProfessionalIncome", "Line_13699_Amount"] Amount,
   line_13900_Amount = Field ["Line_13899_13900_CommissionIncome", "Line_13900_Amount"] Amount,
   line_13899_Amount = Field ["Line_13899_13900_CommissionIncome", "Line_13899_Amount"] Amount,
   line_14100_Amount = Field ["Line_14099_14100_FarmingIncome", "Line_14100_Amount"] Amount,
   line_14099_Amount = Field ["Line_14099_14100_FarmingIncome", "Line_14099_Amount"] Amount,
   line_14299_Amount = Field ["Line_14299_14300_FishingIncome", "Line_14299_Amount"] Amount,
   line_14300_Amount = Field ["Line_14299_14300_FishingIncome", "Line_14300_Amount"] Amount}

page4Fields = BaseNames.page4Fields{
   line_15000_TotalIncome_2 = Field ["Line32", "Line_15000_Amount"] Amount,
   line_21699_Amount = Field ["Line_21699_21700_BusinessLoss", "Line_21699_Amount"] Amount,
   line_21700_Amount = Field ["Line_21699_21700_BusinessLoss", "Line_21700_Amount"] Amount,
   line_22000_Amount = Field ["Line_21999_22000_SupportPayments", "Line_22000_Amount"] Amount,
   line_21999_Amount = Field ["Line_21999_22000_SupportPayments", "Line_21999_Amount"] Amount}

page5Fields = BaseNames.page5Fields{
   step4_TaxableIncome = within "Step4" Rank2.<$> step4Fields,
   partA_FederalTax = within "Step5" . within "PartA" Rank2.<$> partAFields 36,
   partB_FederalTaxCredits = within "Step5" . within "PartB" Rank2.<$> partBFields}

step4Fields = BaseNames.step4Fields{
   line_23600_NetIncome_2 = Field ["Line55", "Line_15000_Amount"] Amount}

partBFields = BaseNames.partBFields {
   line30499_ChildrenNum = Field ["Line30500", "Multiplication", "Line7_ChildrenNum"] Count,
   line_81 = Field ["Line81", "Line30_Amount"] Amount}

page6Fields = BaseNames.page6Fields{
   line82 = Field ["Line_82", "Line43Amount"] Amount,
   line31217 = Field ["EIPremiums_Sub", "Line31217_Sub", "Line11_Amount"] Amount,
   line31350 = Field ["Line31350_Sub", "Line24_Amount"] Amount,
   line94_sum = Field ["Line94", "Amount1"] Amount,
   line94_cont = Field ["Line94", "Amount2"] Amount,
   line96 = Field ["Line96", "Amount"] Amount,
   line99 = Field ["Line99", "Amount"] Amount,
   line104 = Field ["Line104", "Amount"] Amount,
   medical_expenses = page6MedicalExpensesFields,
   line112 = Field ["Line112", "Rate"] $ Constant 0.15 Percent}

page6MedicalExpensesFields = MedicalExpenses {
   familyExpenses = Field ["Line33099_Sub", "Amount"] Amount,
   taxableIncome = Field ["Line106", "Amount"] Amount,
   taxableIncomeFraction = Field ["Line106", "Amount2"] Amount,
   threshold = Field ["Line107", "Amount"] Amount,
   difference = Field ["Line108", "Amount"] Amount,
   otherDependants = Field ["Line33199_Sub", "Line28_Amount"] Amount}

page7Fields = Page7 {
   partC_NetFederalTax = within "PartC" Rank2.<$> partCFields,
   step6_RefundOrBalanceOwing = within "Step6" Rank2.<$> page7step6Fields}

partCFields = BaseNames.partCFields {
   line116 = Field ["Line116", "Line43Amount"] Amount,
   line40400 = Field ["Line40400_Sub", "Line45Amount2"] Amount,
   line119 = Field ["Line119", "Line46Amount"] Amount,
   line122_sum = Field ["Line122", "Line49Amount1"] Amount,
   line122_cont = Field ["Line122", "Line49Amount2"] Amount,
   line123 = Field ["Line123", "Amount"] Amount,
   line124 = Field ["Line124", "Amount"] Amount,
   line125 = Field ["Line42900_Sub", "Amount"] Amount,
   line127 = Field ["Line127", "Line52Amount"] Amount,
   line128 = Field ["Line128", "Amount"] Amount,
   line129 = Field ["Line129", "Amount"] Amount,
   line130 = Field ["Line130", "Amount"] Amount,
   line131 = Field ["Line131", "Amount"] Amount,
   line40900 = Field ["Line41000_40900_Sub", "Line40900", "Amount"] Amount,
   line41000 = Field ["Line41000_40900_Sub", "Amount"] Amount,
   line41300 = Field ["Line41400_sub", "Line41300", "Amount"] Amount,
   line41400 = Field ["Line41400_sub", "Amount"] Amount,
   line41800 = Field ["Line41800_sub", "Line60Amount"] Amount,
   line42000 = Field ["Line42000_sub", "Line61Amount"] Amount}

page7step6Fields = BaseNames.page7step6Fields {
   line140 = Field ["Line140", "Line_42120_Amount"] Amount}

page8Fields = Page8 {
   step6_RefundOrBalanceOwing = within "Step6" Rank2.<$> page8step6Fields,
   line48400_Refund = Field ["Lines_48400-48500", "Line_48400_Refund", "Line_48400_Amount"] Amount,
   line48500_BalanceOwing = Field ["Lines_48400-48500", "Line_48500_BalanceOwing", "Line_48500_Amount"] Amount,
   telephone = Field ["Certification", "Telephone"] Amount,
   date = Field ["Certification", "Date"] Amount,
   taxPreparer = within "Line_49000_IfFeeWasCharged" Rank2.<$> taxPreparerFields,
   line_1_ONOpportunitiesFund = NoField,
   line_46500 = NoField,
   line_46600 = NoField}

page8step6Fields = BaseNames.page8step6Fields {
   line_43500_totalpayable = Field ["Line_43500_TotalPayable2", "Line_42000_Amount"] Amount,
   line_43700_Total_income_tax_ded = Field ["Line_43700_TotalIncomeTaxDeducted", "Line_43700_Amount"] Amount,
   line_44000 = Field ["Line_44000_QuebecAbatement", "Line_44000_Amount"] Amount,
   line_45000_EIOverpayment = Field ["Line_45000_EmploymentInsurance", "Line_45000_Amount"] Amount,
   line_45300_CWB = Field ["Line_45300_WITB", "Line_45300_Amount"] Amount,
   line_45700_GST_HST_Rebate = Field ["Line_45700_GST_HST_rebate", "Line_45700_Amount"] Amount,
   line_46800 = Field ["Line157", "Line_46800_Amount"] Amount,
   line_46900 = Field ["Line157", "Line_46900_Amount"] Amount,
   line_47557 = Field ["Line_47557_AirQuality", "Amount"] Amount,
   line164_Refund_or_BalanceOwing = Field ["Line164", "Refund_or_BalanceOwing_Amount"] Amount}

taxPreparerFields = BaseNames.taxPreparerFields {
   eFileNumber = Field ["Line48900", "EFile"] Textual,
   line49000_WasAFeeCharged = Field ["Line49000", "Line49000_CheckBoxGroup"] $ Switch' "Line49000_CheckBox_EN"}
