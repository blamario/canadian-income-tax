{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.T1.FieldNames.AB (module Tax.Canada.T1.FieldNames.AB, page1Fields) where

import Rank2 qualified

import Tax.FDF (FieldConst (Field, NoField), Entry (..), within)
import Tax.Canada.T1.Types
import Tax.Canada.T1.Types qualified as Page8 (Page8(..))
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
   page3 = within "Page3" Rank2.<$> page3Fields,
   page4 = within "Page4" Rank2.<$> page4Fields,
   page5 = within "Page5" Rank2.<$> page5Fields,
   page6 = within "Page6" . within "PartB" Rank2.<$> page6Fields,
   page7 = within "Page7" Rank2.<$> page7Fields,
   page8 = within "Page8" Rank2.<$> page8Fields}

page2Fields = BaseNames.page2Fields {
   cai = Field ["CAI", "AB_CAI", "Tick_box"] Checkbox,
   organ_donor = NoField}

page3Fields = Page3{
   line_10100_EmploymentIncome = Field ["Line1", "Line_10100_Amount"] Amount,
   line_10105_Taxexemptamount = Field ["Line10105", "Line_10105_Amount"] Amount,
   line_10120_Commissions = Field ["Line10120", "Line_10120_Amount"] Amount,
   line_10130_sf = Field ["Line10130", "Line_10130_Amount"] Amount,
   line_10400_OtherEmploymentIncome = Field ["Line2", "Line_10400_Amount"] Amount,
   line_11300_OldAgeSecurityPension = Field ["Line3", "Line_11300_Amount"] Amount,
   line_11400_CPP_QPP = Field ["Line4", "Line_11400_Amount"] Amount,
   line_11410_DisabilityBenefits = Field ["Line11410", "Line_11410_Amount"] Amount,
   line_11500_OtherPensions = Field ["Line5", "Line_11500_Amount"] Amount,
   line_11600_ElectedSplitPension = Field ["Line6", "Line_11600_Amount"] Amount,
   line_11700_UCCB = Field ["Line7", "Line_11700_Amount"] Amount,
   line_11701_UCCBDesignated = Field ["Line11701", "Line_11701_Amount"] Amount,
   line_11900_EmploymentInsurance = Field ["Line8", "Line_11900_Amount"] Amount,
   line_11905_Employmentmaternity = Field ["Line11905", "Line_11905_Amount"] Amount,
   line_12000_TaxableDividends = Field ["Line9", "Line_12000_Amount"] Amount,
   line_12010_OtherTaxableDividends = Field ["Line12010", "Line_12010_Amount"] Amount,
   line_12100_InvestmentIncome = Field ["Line10", "Line_12100_Amount"] Amount,
   line_12200_PartnershipIncome = Field ["Line11", "Line_12200_Amount"] Amount,
   line_12500_RDSP = Field ["Line12", "Line_12500_Amount"] Amount,
   line_12599_12600_RentalIncome = Field ["Line13", "Line12599", "Line_12599_Amount"] Amount,
   line_12600_Amount = Field ["Line13", "Line_12600_Amount"] Amount,
   line_12700_TaxableCapitalGains = Field ["Line14", "Line_12700_Amount"] Amount,
   line_12799_Amount = Field ["Line15", "Line_12799", "Line_12799_Amount"] Amount,
   line_12800_Amount = Field ["Line15", "Line_12800_Amount"] Amount,
   line_12900_RRSPIncome = Field ["Line16", "Line_12900_Amount"] Amount,
   line_13000_OtherIncome = Field ["Line17", "Line_13000_Amount"] Amount,
   line_13000_OtherIncomeSource = Field ["Line17", "Line_13000_Specify"] Textual,
   line_13010_Taxablescholarship = Field ["Line18", "Line_13010_Amount"] Amount,
   line_19 = Field ["Line19", "Line_15000_Amount"] Amount,
   selfEmployment = selfEmploymentFields,
   line_25_sum = Field ["Line25", "I1", "Amount"] Amount,
   line_25_cont = Field ["Line25", "I2", "Amount"] Amount,
   line_26 = Field ["Line26", "Line_15000_Amount"] Amount,
   line_14400_WorkersCompBen = Field ["Line27", "Line_14400_Amount"] Amount,
   line_14500_SocialAssistPay = Field ["Line28", "Line_14500_Amount"] Amount,
   line_14600_NetFedSupplements = Field ["Line29", "Line_14600_Amount"] Amount,
   line_14700_EqualsAmount = Field ["Line30", "I1", "Amount"] Amount,
   line_14700_PlusAmount = Field ["Line30", "I2", "Amount"] Amount,
   line_15000_TotalIncome = Field ["Line31", "Amount"] Amount}

selfEmploymentFields = BaseNames.selfEmploymentFields {
   line_13499_Amount = Field ["Line20", "Line13499", "Amount"] Amount,
   line_13500_Amount = Field ["Line20", "Amount"] Amount,
   line_13699_Amount = Field ["Line21", "Line13699", "Line_13699_Amount"] Amount,
   line_13899_Amount = Field ["Line22", "Line13899", "Line_13899_Amount"] Amount,
   line_14099_Amount = Field ["Line23", "Line14099", "Line_14099_Amount"] Amount,
   line_14299_Amount = Field ["Line24", "Line14299", "Line_14299_Amount"] Amount}

page4Fields = BaseNames.page4Fields{
   line_15000_TotalIncome_2 = Field ["Line32", "Line_15000_Amount"] Amount,
   line_20600_PensionAdjustment = Field ["Line20600", "Line_20600_Amount"] Amount,
   line_20700_RPPDeduction = Field ["Line33", "Line_20700_Amount"] Amount,
   line_20800_RRSPDeduction = Field ["Line34", "Line_20800_Amount"] Amount,
   line_20810_PRPP = Field ["Line20810", "Line_20810_Amount"] Amount,
   line_21000_SplitPensionDeduction = Field ["Line35", "Line_21000_Amount"] Amount,
   line_21200_Dues = Field ["Line36", "Line_21200_Amount"] Amount,
   line_21300_UCCBRepayment = Field ["Line37", "Line_21300_Amount"] Amount,
   line_21400_ChildCareExpenses = Field ["Line38", "Line_21400_Amount"] Amount,
   line_21500_DisabilityDeduction = Field ["Line39", "Line_21500_Amount"] Amount,
   line_21699_Amount = Field ["Line40", "Line21699", "Line_21699_Amount"] Amount,
   line_21900_MovingExpenses = Field ["Line41", "Line_21900_Amount"] Amount,
   line_21999_Amount = Field ["Line42", "Line21999", "Line_21999_Amount"] Amount,
   line_22100_CarryingChargesInterest = Field ["Line43", "Line_22100_Amount"] Amount,
   line_22200_CPP_QPP_Contributions = Field ["Line44", "Line_22200_Amount"] Amount,
   line_22215_DeductionCPP_QPP = Field ["Line45", "Line_22215_Amount"] Amount,
   line_22400_XplorationDevExpenses = Field ["Line46", "Line_22400_Amount"] Amount,
   line_22900_OtherEmployExpenses = Field ["Line47", "Line_22900_Amount"] Amount,
   line_23100_ClergyResDeduction = Field ["Line48", "Line_23100_Amount"] Amount,
   line_23200_OtherDeductions = Field ["Line49", "Line_23200_Amount"] Amount,
   line_23200_Specify = Field ["Line49", "Line_23200_Specify"] Textual,
   line_23210 = Field ["Line50", "Amount"] Amount,
   line_23300_sum = Field ["Line51", "Line_23300_Amount1"] Amount,
   line_23300_cont = Field ["Line51", "Line_23300_Amount2"] Amount,
   line_23400_NetBeforeAdjust = Field ["Line52", "Line_23400_Amount"] Amount,
   line_23500_SocialBenefits = Field ["Line53", "Line_23500_Amount"] Amount,
   line_23600_NetIncome = Field ["Line54", "Line_23600_Amount"] Amount}

page5Fields = BaseNames.page5Fields{
   step4_TaxableIncome = within "Step4" Rank2.<$> step4Fields,
   partA_FederalTax = within "PartA" Rank2.<$> partAFields 36,
   partB_FederalTaxCredits = within "PartB" Rank2.<$> partBFields}

step4Fields = Step4{
   line_23600_NetIncome_2 = Field ["Line55", "Line_15000_Amount"] Amount,
   line_24400_MilitaryPoliceDeduction = Field ["Line56", "Line_24400_Amount"] Amount,
   line_24900_SecurityDeductions = Field ["Line57", "Line_24900_Amount"] Amount,
   line_25000_OtherPayDeductions = Field ["Line58", "Line_25000_Amount"] Amount,
   line_25100_PartnershipLosses = Field ["Line59", "Line_25100_Amount"] Amount,
   line_25200_NoncapitalLosses = Field ["Line60", "Line_25200_Amount"] Amount,
   line_25300_NetCapitalLosses = Field ["Line61", "Line_25300_Amount"] Amount,
   line_25400_CapitalGainsDeduction = Field ["Line62", "Line_25400_Amount"] Amount,
   line_25500_NorthernDeductions = Field ["Line63", "Line_25500_Amount"] Amount,
   line_25600_AdditionalDeductions_Amount = Field ["Line64", "Line_25600_Amount"] Amount,
   line_25600_AdditionalDeductions_Specify = Field ["Line64", "Line_25600_Specify"] Textual,
   line_25700_AddLines_sum = Field ["Line65", "Line_25700_Amount1"] Amount,
   line_25700_AddLines_cont = Field ["Line65", "Line_25700_Amount2"] Amount,
   line_26000_TaxableIncome = Field ["Line66", "Line_26000_Amount"] Amount}

partBFields = Page5PartB {
   line30000 = Field ["Line74", "Line1_Amount"] Amount,
   line30100 = Field ["Line75", "Line2_Amount"] Amount,
   line30300 = Field ["Line76", "Line3_Amount"] Amount,
   line30400 = Field ["Line77", "Line4_Amount"] Amount,
   line30425 = Field ["Line78", "Line4_Amount"] Amount,
   line30450 = Field ["Line79", "Line6_Amount"] Amount,
   line30499_ChildrenNum = Field ["Line80", "Line30499", "Line7_ChildrenNum"] Count,
   line30500 = Field ["Line80", "Line7_Amount"] Amount,
   line_81 = Field ["Line81", "Line30_Amount"] Amount}

page6Fields = BaseNames.page6Fields {
   line82 = Field ["Line82", "Line43Amount"] Amount,
   line30800 = Field ["Line83", "Line8_Amount"] Amount,
   line31000 = Field ["Line84", "Line9_Amount"] Amount,
   line31200 = Field ["Line85", "Line10_Amount"] Amount,
   line31217 = Field ["Line86", "Line11_Amount"] Amount,
   line31220 = Field ["Line87", "Line12_Amount"] Amount,
   line31240 = Field ["Line88", "Line13_Amount"] Amount,
   line31260 = Field ["Line89", "Line14_Amount"] Amount,
   line31270 = Field ["Line90", "Amount"] Amount,
   line31285 = Field ["Line91", "Amount"] Amount,
   line31300 = Field ["Line92", "Line17_Amount"] Amount,
   line31350 = Field ["Line93", "Line24_Amount"] Amount,
   line94_sum = Field ["Line94", "Line30_Amount"] Amount,
   line94_cont = Field ["Line94", "Line30_Amount[1]"] Amount,
   line31400 = Field ["Line95", "Line18_Amount"] Amount,
   line96 = Field ["Line96", "Line30_Amount"] Amount,
   line31600 = Field ["Line97", "Line19_Amount"] Amount,
   line31800 = Field ["Line98", "Line20_Amount"] Amount,
   line99 = Field ["Line99", "Line30_Amount"] Amount,
   line31900 = Field ["Line100", "Line21_Amount"] Amount,
   line32300 = Field ["Line101", "Line22_Amount"] Amount,
   line32400 = Field ["Line102", "Line23_Amount"] Amount,
   line32600 = Field ["Line103", "Line24_Amount"] Amount,
   line104 = Field ["Line104", "Line30_Amount"] Amount,
   medical_expenses = page6MedicalExpensesFields,
   line33200_sum = Field ["Line110", "Line29_Amount1"] Amount,
   line33200_cont = Field ["Line110", "Line29_Amount2"] Amount,
   line33500 = Field ["Line111", "Line30_Amount"] Amount,
   line112 = Field ["Line112", "Line31_Rate"] $ Constant 0.15 Percent,
   line33800 = Field ["Line113", "Line32_Amount"] Amount,
   line34900 = Field ["Line114", "Line33_Amount"] Amount,
   line35000 = Field ["Line115", "Line34_Amount"] Amount}

page6MedicalExpensesFields = MedicalExpenses {
   familyExpenses = Field ["Line105", "Line25_Amount"] Amount,
   taxableIncome = Field ["Line106", "Amount"] Amount,
   taxableIncomeFraction = Field ["Line106", "Amount2"] Amount,
   threshold = Field ["Line107", "Line26_Amount"] Amount,
   difference = Field ["Line108", "Line27_Amount"] Amount,
   otherDependants = Field ["Line109", "Line28_Amount"] Amount}

page7Fields = Page7 {
   partC_NetFederalTax = within "PartC" Rank2.<$> partCFields,
   step6_RefundOrBalanceOwing = within "Step6" Rank2.<$> page7step6Fields}

partCFields = Page7PartC {
   line116 = Field ["Line116", "Line43Amount"] Amount,
   line40424 = Field ["Line117", "Line44Amount"] Amount,
   line40400 = Field ["Line118", "Line45Amount1"] Amount,
   line119 = Field ["Line119", "Line46Amount"] Amount,
   line40425 = Field ["Line120", "Line47Amount"] Amount,
   line40427 = Field ["Line121", "Line48Amount"] Amount,
   line122_sum = Field ["Line122", "Line49Amount1"] Amount,
   line122_cont = Field ["Line122", "Line49Amount2"] Amount,
   line42900 = Field ["Line123", "Line50Amount"] Amount,
   line124 = Field ["Line124", "Line50Amount"] Amount,
   line125 = Field ["Line125", "Amount"] Amount,
   line40500 = Field ["Line126", "Line51Amount"] Amount,
   line127 = Field ["Line127", "Line52Amount"] Amount,
   line128 = Field ["Line128", "Amount"] Amount,
   line129 = Field ["Line129", "Amount"] Amount,
   line130 = Field ["Line130", "Amount"] Amount,
   line40600 = Field ["Line131", "Amount"] Amount,
   line40900 = Field ["Line132", "Line40900", "Line53Amount"] Amount,
   line41000 = Field ["Line132", "Line54Amount"] Amount,
   line41200 = Field ["Line133", "Line55Amount"] Amount,
   line41300 = Field ["Line134", "Line41300", "Line56Amount1"] Amount,
   line41400 = Field ["Line134", "Line56Amount2"] Amount,
   line41600_sum = Field ["Line135", "Line57Amount1"] Amount,
   line41600_cont = Field ["Line135", "Line57Amount2"] Amount,
   line41700 = Field ["Line136", "Line58Amount"] Amount,
   line41500 = Field ["Line137", "Line59Amount"] Amount,
   line41800 = Field ["Line138", "Line60Amount"] Amount,
   line42000 = Field ["Line139", "Line61Amount"] Amount}

page7step6Fields = Page7Step6 {
   line140 = Field ["Line140", "Line_42120_Amount"] Amount,
   line_42100_CPPContributions = Field ["Line141", "Line_42100_Amount"] Amount,
   line_42120_EIPremiums = Field ["Line142", "Line_42120_Amount"] Amount,
   line_42200_SocialBenefits = Field ["Line143", "Line_42200_Amount"] Amount,
   line_42800_ProvTerrTax = Field ["Line144", "Line_42800_Amount"] Amount,
   line_43500_TotalPayable = Field ["Line145", "Line_43500_Amount"] Amount}

page8Fields = BaseNames.page8Fields {
   Page8.step6_RefundOrBalanceOwing = page8step6Fields,
   telephone = Field ["Certification", "Telephone"] Amount,
   date = Field ["Certification", "Date"] Amount,
   taxPreparer = within "Efile" Rank2.<$> taxPreparerFields,
   line_1_ONOpportunitiesFund = NoField,
   line_46500 = NoField,
   line_46600 = NoField}

page8step6Fields = BaseNames.page8step6Fields {
   line_43500_totalpayable = Field ["Line146", "Line_42000_Amount"] Amount,
   line_43700_Total_income_tax_ded = Field ["Line147", "Line_43700_Amount"] Amount,
   line_44000 = Field ["Line148", "Line_44000_Amount"] Amount,
   line_44800_CPPOverpayment = Field ["Line149", "Line_44800_Amount"] Amount,
   line_45000_EIOverpayment = Field ["Line150", "Line_45000_Amount"] Amount,
   line_45200_MedicalExpense = Field ["Line151", "Line_45200_Amount"] Amount,
   line_45300_CWB = Field ["Line152", "Line_45300_Amount"] Amount,
   line_45350_CTC = Field ["Line153", "Line_45300_Amount"] Amount,
   line_45400_InvestmentTaxCredit = Field ["Line154", "Line_45400_Amount"] Amount,
   line_45600_TrustTaxCredit = Field ["Line155", "Line_45600_Amount"] Amount,
   line_45700_GST_HST_Rebate = Field ["Line156", "Line_45700_Amount"] Amount,
   line_46800 = Field ["Line157", "Line46800", "Line_46800_Amount"] Amount,
   line_46900 = Field ["Line157", "Line_46900_Amount"] Amount,
   line_47555_TaxPaid = Field ["Line158", "Line_47600_Amount"] Amount,
   line_47600_TaxPaid = Field ["Line159[1]", "Line_47600_Amount"] Amount,
   line_47900_ProvTerrCredits = Field ["Line160[1]", "Line_47900_Amount"] Amount,
   line_48200_sum = Field ["Line161", "Line_48200_Amount1"] Amount,
   line_48200_cont = Field ["Line161", "Line_48200_Amount2"] Amount,
   line164_Refund_or_BalanceOwing = Field ["Line162", "Amount"] Amount}

taxPreparerFields = BaseNames.taxPreparerFields {
   eFileNumber = Field ["Line48900", "EFile"] Textual,
   line49000_WasAFeeCharged = Field ["Line49000", "Line49000_CheckBoxGroup"] $ Switch' "Line49000_CheckBox_EN"}
