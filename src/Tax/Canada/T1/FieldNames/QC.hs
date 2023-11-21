{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.T1.FieldNames.QC (t1Fields) where

import Rank2 qualified

import Tax.FDF (FieldConst (Field, NoField), Entry (..), within)
import Tax.Canada.T1.Types
import Tax.Canada.T1.Types qualified as Page8 (Page8(..))
import Tax.Canada.T1.FieldNames
  hiding (t1Fields,
          page2Fields, page3Fields, page4Fields, page5Fields, page6Fields, page7Fields, page8Fields,
          page2ElectionsCanadaFields, step4Fields, partBFields, page6MedicalExpensesFields,
          partCFields, page7step6Fields, page8step6Fields, selfEmploymentFields, taxPreparerFields)
import Tax.Canada.T1.FieldNames qualified as BaseNames
import Tax.Canada.T1.FieldNames.BC qualified as BC

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
   foreign_property = Field ["Foreign_property", "Line26000"] $ Switch' "ForeignProperty_CheckBox",
   tax_exempt = Field ["Tax_exempt", "Q", "Spouse_SelfEmployed"] Checkbox,
   electionsCanada = within "ElectionsCanada" Rank2.<$> page2ElectionsCanadaFields,
   cai = NoField,
   organ_donor = NoField}

page2ElectionsCanadaFields = ElectionsCanada {
   citizenship = Field ["LineA"] $ Switch' "A_CheckBox",
   authorization = Field ["LineB"] $ Switch' "B_Authorize_CheckBox"}

page3Fields = BaseNames.page3Fields{
   line_10100_EmploymentIncome = Field ["Line_1", "Line_10100_Amount"] Amount,
   line_10105_Taxexemptamount = Field ["Line_10105_Taxexempt", "Line_10105_Amount"] Amount,
   line_10120_Commissions = Field ["Line_10120_Comissions", "Line_10120_Amount"] Amount,
   line_10130_sf = Field ["Line_10130_Wage_Loss", "Line_10130_Amount"] Amount,
   line_10400_OtherEmploymentIncome = Field ["Line_2", "Line_10400_Amount"] Amount,
   line_11300_OldAgeSecurityPension = Field ["Line_3", "Line_11300_Amount"] Amount,
   line_11400_CPP_QPP = Field ["Line_4", "Line_11400_Amount"] Amount,
   line_11410_DisabilityBenefits = Field ["Line_11410_Disability_Benefits", "Line_11410_Amount"] Amount,
   line_11500_OtherPensions = Field ["Line_5", "Line_11500_Amount"] Amount,
   line_11600_ElectedSplitPension = Field ["Line_6", "Line_11600_Amount"] Amount,
   line_11700_UCCB = Field ["Line_7", "Line_11700_Amount"] Amount,
   line_11701_UCCBDesignated = Field ["Line_18500_UCCBDesignated", "Line_18500_Amount"] Amount,
   line_11900_EmploymentInsurance = Field ["Line_8", "Line_11900_Amount"] Amount,
--   line_11905_Employmentmaternity = Field ["Line11905", "Line_11905_Amount"] Amount,
   line_12000_TaxableDividends = Field ["Line_9", "Line_12000_Amount"] Amount,
--   line_12010_OtherTaxableDividends = Field ["Line_12010_OtherTaxableDividends", "Line_12010_Amount"] Amount,
   line_12100_InvestmentIncome = Field ["Line_10", "Line_12100_Amount"] Amount,
   line_12200_PartnershipIncome = Field ["Line_11", "Line_12200_Amount"] Amount,
   line_12500_RDSP = Field ["Line_12", "Line_12500_Amount"] Amount,
   line_12599_12600_RentalIncome = Field ["Line_13", "Line_12599_Amount"] Amount,
   line_12600_Amount = Field ["Line_13", "Line_12600_Amount"] Amount,
   line_12700_TaxableCapitalGains = Field ["Line_14", "Line_12700_Amount"] Amount,
   line_12799_Amount = Field ["Line_15", "Line_12799_Amount"] Amount,
   line_12800_Amount = Field ["Line_15", "Line_12800_Amount"] Amount,
   line_12900_RRSPIncome = Field ["Line_16", "Line_12900_Amount"] Amount,
   line_13000_OtherIncome = Field ["Line_17", "Line_13000_Amount"] Amount,
   line_13000_OtherIncomeSource = Field ["Line_17", "Line_13000_Specify"] Textual,
   line_13010_Taxablescholarship = Field ["Line_18", "Amount"] Amount,
   line_19 = Field ["Line_19", "Amount"] Amount,
   selfEmployment = within "Self_Employment_Group" Rank2.<$> selfEmploymentFields,
   line_25_sum = Field ["Line_25", "Amount1"] Amount,
   line_25_cont = Field ["Line_25", "Amount2"] Amount,
   line_26 = Field ["Line_26", "Amount"] Amount,
   line_14400_WorkersCompBen = Field ["Line_27", "Line_14400_Amount"] Amount,
   line_14500_SocialAssistPay = Field ["Line_28", "Line_14500_Amount"] Amount,
   line_14600_NetFedSupplements = Field ["Line_29", "Line_14600_Amount"] Amount,
   line_14700_EqualsAmount = Field ["Line_30", "Line_14700_EqualsAmount"] Amount,
   line_14700_PlusAmount = Field ["Line_30", "Line_14700_PlusAmount"] Amount,
   line_15000_TotalIncome = Field ["Line_31", "Line_15000_Amount"] Amount}

selfEmploymentFields = SelfEmploymentIncome {
   line_13499_Amount = Field ["Line_20", "Line_13499_Amount"] Amount,
   line_13500_Amount = Field ["Line_20", "Line_13500_Amount"] Amount,
   line_13699_Amount = Field ["Line_21", "Line_13699_Amount"] Amount,
   line_13700_Amount = Field ["Line_21", "Line_13700_Amount"] Amount,
   line_13899_Amount = Field ["Line_22", "Line_13899_Amount"] Amount,
   line_13900_Amount = Field ["Line_22", "Line_13900_Amount"] Amount,
   line_14099_Amount = Field ["Line_23", "Line_14099_Amount"] Amount,
   line_14100_Amount = Field ["Line_23", "Line_14100_Amount"] Amount,
   line_14299_Amount = Field ["Line_24", "Line_14299_Amount"] Amount,
   line_14300_Amount = Field ["Line_24", "Line_14300_Amount"] Amount}

page4Fields = BaseNames.page4Fields{
   line_15000_TotalIncome_2 = Field ["Line_32", "Line_15000_Amount"] Amount,
--   line_20600_PensionAdjustment = Field ["Line_20600_PensionAdjustment", "Line_20600_Amount"] Amount,
   line_20700_RPPDeduction = Field ["Line_33", "Line_20700_Amount"] Amount,
   line_20800_RRSPDeduction = Field ["Line_34", "Line_20800_Amount"] Amount,
--   line_20810_PRPP = Field ["Line_20810_PRPP", "Line_20810_Amount"] Amount,
   line_21000_SplitPensionDeduction = Field ["Line_35", "Line_21000_Amount"] Amount,
   line_21200_Dues = Field ["Line_36", "Line_21200_Amount"] Amount,
   line_21300_UCCBRepayment = Field ["Line_37", "Line_21300_Amount"] Amount,
   line_21400_ChildCareExpenses = Field ["Line_38", "Line_21400_Amount"] Amount,
   line_21500_DisabilityDeduction = Field ["Line_39", "Line_21500_Amount"] Amount,
   line_21699_Amount = Field ["Line_40", "Line_21699_Amount"] Amount,
   line_21700_Amount = Field ["Line_40", "Line_21700_Amount"] Amount,
   line_21900_MovingExpenses = Field ["Line_41", "Line_21900_Amount"] Amount,
   line_21999_Amount = Field ["Line_42", "Line_21999_Amount"] Amount,
   line_22000_Amount = Field ["Line_42", "Line_22000_Amount"] Amount,
   line_22100_CarryingChargesInterest = Field ["Line_43", "Line_22100_Amount"] Amount,
   line_22200_CPP_QPP_Contributions = Field ["Line_44", "Line_22200_Amount"] Amount,
   line_22215_DeductionCPP_QPP = Field ["Line_45", "Line_22215_Amount"] Amount,
   line_22300_DeductionPPIP = Field ["Line_46", "Line_22300_Amount"] Amount,
   line_22400_XplorationDevExpenses = Field ["Line_47", "Line_22400_Amount"] Amount,
   line_22900_OtherEmployExpenses = Field ["Line_48", "Line_22900_Amount"] Amount,
   line_23100_ClergyResDeduction = Field ["Line_49", "Line_23100_Amount"] Amount,
   line_23200_OtherDeductions = Field ["Line_50", "Line_23200_Amount"] Amount,
   line_23200_Specify = Field ["Line_50", "Line_23200_Specify"] Textual,
   line_23210 = Field ["Line_51", "Amount"] Amount,
   line_23300_sum = Field ["Line_52", "Line_23300_Amount1"] Amount,
   line_23300_cont = Field ["Line_52", "Line_23300_Amount2"] Amount,
   line_23400_NetBeforeAdjust = Field ["Line_53", "Line_23400_Amount"] Amount,
   line_23500_SocialBenefits = Field ["Line_54", "Line_23500_Amount"] Amount,
   line_23600_NetIncome = Field ["Line_55", "Line_23600_Amount"] Amount}

page5Fields = BaseNames.page5Fields{
   step4_TaxableIncome = within "Step4" Rank2.<$> step4Fields,
   partA_FederalTax = within "Part_A" Rank2.<$> partAFields "Column" 39,
   partB_FederalTaxCredits = within "Part_B" Rank2.<$> partBFields}

step4Fields = Step4{
   line_23600_NetIncome_2 = Field ["Line_56", "Line_15000_Amount"] Amount,
   line_24400_MilitaryPoliceDeduction = Field ["Line_57", "Line_24400_Amount"] Amount,
   line_24900_SecurityDeductions = Field ["Line_58", "Line_24900_Amount"] Amount,
   line_25000_OtherPayDeductions = Field ["Line_59", "Line_25000_Amount"] Amount,
   line_25100_PartnershipLosses = Field ["Line_60", "Line_25100_Amount"] Amount,
   line_25200_NoncapitalLosses = Field ["Line_61", "Line_25200_Amount"] Amount,
   line_25300_NetCapitalLosses = Field ["Line_62", "Line_25300_Amount"] Amount,
   line_25400_CapitalGainsDeduction = Field ["Line_63", "Line_25400_Amount"] Amount,
   line_25500_NorthernDeductions = Field ["Line_64", "Line_25500_Amount"] Amount,
   line_25600_AdditionalDeductions_Amount = Field ["Line_65", "Line_25600_Amount"] Amount,
   line_25600_AdditionalDeductions_Specify = Field ["Line_65", "Line_25600_Specify"] Textual,
   line_25700_AddLines_sum = Field ["Line_66", "Line_25700_Amount1"] Amount,
   line_25700_AddLines_cont = Field ["Line_66", "Line_25700_Amount2"] Amount,
   line_26000_TaxableIncome = Field ["Line_67", "Line_26000_Amount"] Amount}

partBFields = Page5PartB {
   line30000 = Field ["Line_75", "Line1Amount"] Amount,
   line30100 = Field ["Line_76", "Line2Amount"] Amount,
   line30300 = Field ["Line_77", "Line3Amount"] Amount,
   line30400 = Field ["Line_78", "Line4Amount"] Amount,
   line30425 = Field ["Line_79", "Line5Amount"] Amount,
   line30450 = Field ["Line_80", "Line6Amount"] Amount,
   line30499_ChildrenNum = Field ["Line_81", "Line30499", "Line7Amount1"] Count,
   line30500 = Field ["Line_81", "Line7Amount2"] Amount,
   line_81 = Field ["Line_82", "Line30_Amount"] Amount}

page6Fields = Page6 {
   line82 = Field ["Line_83", "Line43Amount"] Amount,
   line30800 = Field ["Line_84", "Line8Amount"] Amount,
   line31000 = Field ["Line_85", "Line9Amount"] Amount,
   line31200 = Field ["Line_86", "Line10Amount"] Amount,
   line31205 = Field ["Line_88", "Line12Amount"] Amount,
   line31210 = Field ["Line_89", "Amount"] Amount,
   line31215 = Field ["Line_90", "Line14Amount"] Amount,
   line31217 = Field ["Line_87", "Line11Amount"] Amount,
   line31220 = Field ["Line_91", "Line15Amount"] Amount,
   line31240 = Field ["Line_92", "Line16Amount"] Amount,
   line31260 = Field ["Line_93", "Line17Amount"] Amount,
   line31270 = Field ["Line_94", "Amount"] Amount,
   line31285 = Field ["Line_95", "Amount"] Amount,
   line31300 = Field ["Line_96", "Line20Amount"] Amount,
   line31350 = Field ["Line_97", "Amount"] Amount,
   line94_sum = Field ["Line_98", "Amount1"] Amount,
   line94_cont = Field ["Line_98", "Amount2"] Amount,
   line31400 = Field ["Line_99", "Line21Amount"] Amount,
   line96 = Field ["Line_100", "Amount1"] Amount,
   line31600 = Field ["Line_101", "Line22Amount"] Amount,
   line31800 = Field ["Line_102", "Line23Amount"] Amount,
   line99 = Field ["Line_103", "Amount1"] Amount,
   line31900 = Field ["Line_104", "Line24Amount"] Amount,
   line32300 = Field ["Line_105", "Line25Amount"] Amount,
   line32400 = Field ["Line_106", "Line26Amount"] Amount,
   line32600 = Field ["Line_107", "Line27Amount"] Amount,
   line104 = Field ["Line_108", "Amount1"] Amount,
   medical_expenses = page6MedicalExpensesFields,
   line33200_sum = Field ["Line_114", "Line32Amount1"] Amount,
   line33200_cont = Field ["Line_114", "Line32Amount2"] Amount,
   line33500 = Field ["Line_115", "Line33Amount"] Amount,
   line112 = Field ["Line_116", "Line34Rate"] $ Constant 0.15 Percent,
   line33800 = Field ["Line_117", "Line35Amount"] Amount,
   line34900 = Field ["Line_118", "Line36Amount"] Amount,
   line35000 = Field ["Line_119", "Line37Amount"] Amount}

page6MedicalExpensesFields = MedicalExpenses {
   familyExpenses = Field ["Line_109", "Amount"] Amount,
   taxableIncome = Field ["Line_110", "Amount1"] Amount,
   taxableIncomeFraction = Field ["Line_110", "Amount2"] Amount,
   threshold = Field ["Line_111", "Line29Amount"] Amount,
   difference = Field ["Line_112", "Line30Amount"] Amount,
   otherDependants = Field ["Line_113", "Line31Amount"] Amount}

page7Fields = Page7 {
   partC_NetFederalTax = within "PartC" Rank2.<$> partCFields,
   step6_RefundOrBalanceOwing = within "Step6" Rank2.<$> page7step6Fields}

partCFields = Page7PartC {
   line116 = Field ["Line_120", "Line46Amount"] Amount,
   line40424 = Field ["Line_121", "Amount"] Amount,
   line40400 = Field ["Line_122", "Line48Amount1"] Amount,
   line119 = Field ["Line_123", "Line49Amount"] Amount,
   line40425 = Field ["Line_124", "Line50Amount"] Amount,
   line40427 = Field ["Line_125", "Line51Amount"] Amount,
   line122_sum = Field ["Line_126", "Line52Amount1"] Amount,
   line122_cont = Field ["Line_126", "Line52Amount2"] Amount,
   line42900 = Field ["Line_127", "Amount"] Amount,
   line124 = Field ["Line_128", "Amount"] Amount,
   line125 = Field ["Line_129", "Amount"] Amount,
   line40500 = Field ["Line_130", "Amount"] Amount,
   line127 = Field ["Line_131", "Line55Amount"] Amount,
   line128 = Field ["Line_132", "Amount"] Amount,
   line129 = Field ["Line_133", "Amount"] Amount,
   line130 = Field ["Line_134", "Amount"] Amount,
   line40600 = Field ["Line_135", "Amount"] Amount,
   line40900 = Field ["Line_136", "Line40900", "Amount"] Amount,
   line41000 = Field ["Line_136", "Amount"] Amount,
   line41200 = Field ["Line_137", "Line58Amount"] Amount,
   line41300 = Field ["Line_138", "Line41300", "Line59Amount1"] Amount,
   line41400 = Field ["Line_138", "Line59Amount2"] Amount,
   line41600_sum = Field ["Line_139", "Fill1"] Amount,
   line41600_cont = Field ["Line_139", "Fill2"] Amount,
   line41700 = Field ["Line_140", "Line61Amount"] Amount,
   line41500 = Field ["Line_141", "Line62Amount"] Amount,
   line41800 = Field ["Line_142", "Line63Amount"] Amount,
   line42000 = Field ["Line_143", "Line64Amount"] Amount}

page7step6Fields = Page7Step6 {
   line140 = Field ["Line_144", "Line_42120_Amount"] Amount,
   line_42100_CPPContributions = NoField,
   line_42120_EIPremiums = Field ["Line_145", "Line_42120_Amount"] Amount,
   line_42200_SocialBenefits = Field ["Line_146", "Line_42200_Amount"] Amount,
   line_42800_ProvTerrTax = Field ["Line_147", "Line_42800_Amount"] Amount,
   line_43500_TotalPayable = Field ["Line_148", "Line_43500_Amount"] Amount}

page8Fields = BaseNames.page8Fields {
   Page8.step6_RefundOrBalanceOwing = within "RefundOrBalanceOwing" Rank2.<$> page8step6Fields,
   line48400_Refund = Field ["Lines48400_48500", "Line48400", "Line_48400_Amount"] Amount,
   line48500_BalanceOwing = Field ["Lines48400_48500", "Line48500", "Line_48500_Amount"] Amount,
   telephone = Field ["Certification", "Telephone"] Amount,
   date = Field ["Certification", "Date"] Amount,
   taxPreparer = within "Line_49000_IfFeeWasCharged" Rank2.<$> taxPreparerFields,
   line_1_ONOpportunitiesFund = NoField,
   line_46500 = NoField,
   line_46600 = NoField}

page8step6Fields = BaseNames.page8step6Fields {
   line_43500_totalpayable = Field ["Line_149", "Line_42000_Amount"] Amount,
   line_43700_Total_income_tax_ded = Field ["Line_150", "Line_43700_Amount"] Amount,
   line_43800_TaxTransferQC = Field ["Line_151", "Line_43800_Amount"] Amount,
   line_43850_diff = Field ["Line_152", "Line_43900_Amount1"] Amount,
   line_43850_cont = Field ["Line_152", "Line_43900_Amount2"] Amount,
   line_42900_copy = Field ["Line_153", "Line_43900_Amount2"] Amount,
   line_44000 = Field ["Line_153", "Line_44000_Amount"] Amount,
   line_44800_CPPOverpayment = NoField,
   line_45000_EIOverpayment = Field ["Line_45000_A_EIOverpayment", "Line_45000_Amount"] Amount,
   line_31210_copy = Field ["Line_B_Schedule1Amount", "Line_B_Amount"] Amount,
   line_45100_diff = Field ["Line_156", "Line_45100_Amount1"] Amount,
   line_45100_cont = Field ["Line_156", "Line_45100_Amount2"] Amount,
   line_45200_MedicalExpense = Field ["Line_157", "Line_45200_Amount"] Amount,
   line_45300_CWB = Field ["Line_158", "Line_45300_Amount"] Amount,
   line_45350_CTC = Field ["Line_159", "Line_45300_Amount"] Amount,
   line_45400_InvestmentTaxCredit = Field ["Line_160", "Line_45400_Amount"] Amount,
   line_45600_TrustTaxCredit = Field ["Line_161", "Line_45600_Amount"] Amount,
   line_45700_GST_HST_Rebate = Field ["Line_162", "Line_45700_Amount"] Amount,
   line_46800 = Field ["Line_163", "Line_46800_Amount"] Amount,
   line_46900 = Field ["Line_163", "Line_46900_Amount"] Amount,
   line_47555_TaxPaid = Field ["Line_164", "Line_47600_Amount"] Amount,
   line_47556 = Field ["Line_165", "Amount"] Amount,
   line_47557 = Field ["Line_166", "Amount"] Amount,
   line_47600_TaxPaid = Field ["Line_167", "Line_47600_Amount"] Amount,
   line_47900_ProvTerrCredits = NoField,
   line_48200_sum = Field ["Line_168", "Line_48200_Amount1"] Amount,
   line_48200_cont = Field ["Line_168", "Line_48200_Amount2"] Amount,
   line164_Refund_or_BalanceOwing = Field ["Line_169", "Refund_or_BalanceOwing_Amount"] Amount}

taxPreparerFields = BaseNames.taxPreparerFields {
   line49000_WasAFeeCharged = Field ["Line49000_CheckBoxGroup"] $ Switch' "Line49000_CheckBox"}
