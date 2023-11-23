{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.T1.FieldNames.ON where

import Rank2 qualified

import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Tax.FDF (FieldConst (Field, NoField), Entry (..), within)
import Tax.Canada.Shared (TaxIncomeBracket (..))
import Tax.Canada.T1.Types

t1Fields :: T1 FieldConst
t1Fields = within "form1" Rank2.<$> T1 {
   page1 = within "Page1" . within "Step1" Rank2.<$> page1Fields,
   page2 = within "Page2" Rank2.<$> page2Fields,
   page3 = within "Page3" . within "Step2" Rank2.<$> page3Fields,
   page4 = within "Page4" . within "Step3" Rank2.<$> page4Fields,
   page5 = within "Page5" Rank2.<$> page5Fields,
   page6 = within "Page6" . within "PartB" Rank2.<$> page6Fields,
   page7 = within "Page7" Rank2.<$> page7Fields,
   page8 = within "Page8" Rank2.<$> page8Fields}

page1Fields = Page1 {
   identification = within "Identification" Rank2.<$> page1IdentificationFields,
   residence = within "Residence_Info" Rank2.<$> page1ResidenceFields,
   spouse = within "Info_Spouse_CLP" Rank2.<$> page1SpouseFields}

page1IdentificationFields = Identification {
   emailAddress = Field ["EmailAddress"] Textual,
   dateDeath = Field ["DateDeath_Comb_BordersAll", "DateDeath_Comb"] Date,
   postalCode = Field ["PostalCode_Comb_BordersAll", "PostalCode"] Textual,
   your_Language = Field ["Your_Language", "RadioButtonlanguaget"] $ RadioButton [English, French],
   id_City = Field ["ID_City"] Textual,
   sin = Field ["SIN_Comb_BordersAll", "SIN_Comb"] Textual,
   id_LastName = Field ["ID_LastName"] Textual,
   dateBirth = Field ["DateBirth_Comb_BordersAll", "DateBirth_Comb"] Date,
   id_FirstNameInitial = Field ["ID_FirstNameInitial"] Textual,
   id_MailingAddress = Field ["ID_MailingAddress"] Textual,
   maritalStatus = Field ["MaritalStatus_Checkbox"] $ RadioButtons 0 1 "MaritalStatus" [Married .. Single],
   id_RuralRoute = Field ["ID_RuralRoute"] Textual,
   id_POBox = Field ["ID_POBox"] Textual,
   prov_DropDown = Field ["Prov_DropDown"] Province}

page1ResidenceFields = Residence {
   prov_DropDown_Business = Field ["Prov_DropDown-Business"] Province,
   prov_DropDown_Residence = Field ["Prov_DropDown-Residence"] Province,
   date_Departure = Field ["Date_Departure", "DateMMDD_Comb_BordersAll_Std", "DateMMDD_Comb"] Date,
   date_Entry = Field ["Date_Entry", "DateMMDD_Comb_BordersAll_Std", "DateMMDD_Comb"] Date,
   prov_DropDown = Field ["Prov_DropDown"] Textual}
  
page1SpouseFields = Spouse {
   line23600 = Field ["Line23600", "Amount"] Amount,
   self_employment = Field ["Self-employment", "Checkbox"] Checkbox,
   spouse_First_Name = Field ["Spouse_First_Name"] Textual,
   line11700 = Field ["Line11700", "Amount"] Amount,
   line21300 = Field ["Line21300", "Amount"] Amount,
   sin = Field ["SIN_Comb_BordersAll", "SIN_Comb"] Textual}

page2Fields = Page2 {
   foreign_property = Field ["Foreign_property", "Line26600"] $ Switch "Option1" "Option2" "ForeignProperty_CheckBox",
   tax_exempt = Field ["Tax_exempt", "Exempt", "Spouse_SelfEmployed"] Checkbox,
   electionsCanada = within "ElectionsCanada" Rank2.<$> page2ElectionsCanadaFields,
   cai = Field ["CAI", "CAI_ON", "Tick_box"] Checkbox,
   organ_donor = Field ["Organ_donor", "Question"] $ Switch "Option1" "Option2" "OrganDonor_CheckBox"
}

page2ElectionsCanadaFields = ElectionsCanada {
   citizenship = Field ["LineA"] $ Switch "Option1" "Option2" "A_CheckBox",
   authorization = Field ["LineB"] $ Switch "Option1" "Option2" "B_Authorize_CheckBox"}

page3Fields = Page3 {
   line_10100_EmploymentIncome = Field ["Line_10100_EmploymentIncome", "Line_10100_Amount"] Amount,
   line_10105_Taxexemptamount = Field ["Line_10105_Taxexemptamount", "Line_10105_Amount"] Amount,
   line_10120_Commissions = Field ["Line_10120_Commissions", "Line_10120_Amount"] Amount,
   line_10130_sf = Field ["Line_10130_sf", "Line_10130_Amount"] Amount,
   line_10400_OtherEmploymentIncome = Field ["Line_10400_OtherEmploymentIncome", "Line_10400_Amount"] Amount,
   line_11300_OldAgeSecurityPension = Field ["Line_11300_OldAgeSecurityPension", "Line_11300_Amount"] Amount,
   line_11400_CPP_QPP = Field ["Line_11400_CPP_QPP", "Line_11400_Amount"] Amount,
   line_11410_DisabilityBenefits = Field ["Line_11410_DisabilityBenefits", "Line_11410_Amount"] Amount,
   line_11500_OtherPensions = Field ["Line_11500_OtherPensions", "Line_11500_Amount"] Amount,
   line_11600_ElectedSplitPension = Field ["Line_11600_ElectedSplitPension", "Line_11600_Amount"] Amount,
   line_11700_UCCB = Field ["Line_11700_UCCB", "Line_11700_Amount"] Amount,
   line_11701_UCCBDesignated = Field ["Line_11701_UCCBDesignated", "Line_11701_Amount"] Amount,
   line_11900_EmploymentInsurance = Field ["Line_11900_EmploymentInsurance", "Line_11900_Amount"] Amount,
   line_11905_Employmentmaternity = Field ["Line_11905_Employmentmaternity", "Line_11905_Amount"] Amount,
   line_12000_TaxableDividends = Field ["Line_12000_TaxableDividends", "Amount"] Amount,
   line_12010_OtherTaxableDividends = Field ["Line_12010_OtherTaxableDividends", "Line_12010_Amount"] Amount,
   line_12100_InvestmentIncome = Field ["Line_12100_InvestmentIncome", "Line_12100_Amount"] Amount,
   line_12200_PartnershipIncome = Field ["Line_12200_PartnershipIncome", "Line_12200_Amount"] Amount,
   line_12500_RDSP = Field ["Line_12500_RDSP", "Line_12500_Amount"] Amount,
   line_12599_12600_RentalIncome = Field ["Line_12599_12600_RentalIncome", "Line_12599_Amount"] Amount,
   line_12600_Amount = Field ["Line_12599_12600_RentalIncome", "Line_12600_Amount"] Amount,
   line_12700_TaxableCapitalGains = Field ["Line_12700_TaxableCapitalGains", "Line_12700_Amount"] Amount,
   line_12799_Amount = Field ["Line_12799_12800_SupportPayReceived", "Line_12799_Amount"] Amount,
   line_12800_Amount = Field ["Line_12799_12800_SupportPayReceived", "Line_12800_Amount"] Amount,
   line_12900_RRSPIncome = Field ["Line_12900_RRSPIncome", "Line_12900_Amount"] Amount,
   line_13000_OtherIncome = Field ["Line_13000_OtherIncome", "Line_13000_Amount"] Amount,
   line_13000_OtherIncomeSource = Field ["Line_13000_OtherIncome", "Line_13000_Specify"] Textual,
   line_13010_Taxablescholarship = Field ["Line_13010_Taxablescholarship", "Amount"] Amount,
   line_19 = Field ["Line_19", "Amount"] Amount,
   selfEmployment = selfEmploymentFields,
   line_25_sum = Field ["Line_25", "Amount1"] Amount,
   line_25_cont = Field ["Line_25", "Amount2"] Amount,
   line_26 = Field ["Line_26", "Amount1"] Amount,
   line_14400_WorkersCompBen = Field ["Line_14400_WorkersCompBen", "Line_14400_Amount"] Amount,
   line_14500_SocialAssistPay = Field ["Line_14500_SocialAssistPay", "Line_14500_Amount"] Amount,
   line_14600_NetFedSupplements = Field ["Line_14600_NetFedSupplements", "Line_14600_Amount"] Amount,
   line_14700_EqualsAmount = Field ["Line_14700_AddLines", "Line_14700_EqualsAmount"] Amount,
   line_14700_PlusAmount = Field ["Line_14700_AddLines", "Line_14700_PlusAmount"] Amount,
   line_15000_TotalIncome = Field ["Line_15000_TotalIncome", "Line_15000_Amount"] Amount}

selfEmploymentFields = SelfEmploymentIncome {
   line_13499_Amount = Field ["Line20", "Line_13499_Amount"] Amount,
   line_13500_Amount = Field ["Line20", "Line_13500_Amount"] Amount,
   line_13699_Amount = Field ["Line21", "Line_13699_Amount"] Amount,
   line_13700_Amount = Field ["Line21", "Line_13700_Amount"] Amount,
   line_13899_Amount = Field ["Line22", "Line_13899_Amount"] Amount,
   line_13900_Amount = Field ["Line22", "Line_13900_Amount"] Amount,
   line_14099_Amount = Field ["Line23", "Line_14099_Amount"] Amount,
   line_14100_Amount = Field ["Line23", "Line_14100_Amount"] Amount,
   line_14299_Amount = Field ["Line24", "Line_14299_Amount"] Amount,
   line_14300_Amount = Field ["Line24", "Line_14300_Amount"] Amount}

page4Fields = Page4 {
   line_15000_TotalIncome_2 = Field ["Line_15000_TotalIncome_2", "Line_15000_Amount"] Amount,
   line_20600_PensionAdjustment = Field ["Line_20600_PensionAdjustment", "Line_20600_Amount"] Amount,
   line_20700_RPPDeduction = Field ["Line_20700_RPPDeduction", "Line_20700_Amount"] Amount,
   line_20800_RRSPDeduction = Field ["Line_20800_RRSPDeduction", "Line_20800_Amount"] Amount,
   line_20810_PRPP = Field ["Line_20810_PRPP", "Line_20810_Amount"] Amount,
   line_21000_SplitPensionDeduction = Field ["Line_21000_SplitPensionDeduction", "Line_21000_Amount"] Amount,
   line_21200_Dues = Field ["Line_21200_Dues", "Line_21200_Amount"] Amount,
   line_21300_UCCBRepayment = Field ["Line_21300_UCCBRepayment", "Line_21300_Amount"] Amount,
   line_21400_ChildCareExpenses = Field ["Line_21400_ChildCareExpenses", "Line_21400_Amount"] Amount,
   line_21500_DisabilityDeduction = Field ["Line_21500_DisabilityDeduction", "Line_21500_Amount"] Amount,
   line_21699_Amount = Field ["Line40", "Line_21699_Amount"] Amount,
   line_21700_Amount = Field ["Line40", "Line_21700_Amount"] Amount,
   line_21900_MovingExpenses = Field ["Line_21900_MovingExpenses", "Line_21900_Amount"] Amount,
   line_21999_Amount = Field ["Line42", "Line_21999_Amount"] Amount,
   line_22000_Amount = Field ["Line42", "Line_22000_Amount"] Amount,
   line_22100_CarryingChargesInterest = Field ["Line_22100_CarryingChargesInterest", "Line_22100_Amount"] Amount,
   line_22200_CPP_QPP_Contributions = Field ["Line_22200_CPP_QPP_Contributions", "Line_22200_Amount"] Amount,
   line_22215_DeductionCPP_QPP = Field ["Line_22215_DeductionCPP_QPP", "Line_22215_Amount"] Amount,
   line_22300_DeductionPPIP = NoField,
   line_22400_XplorationDevExpenses = Field ["Line_22400_XplorationDevExpenses", "Line_22400_Amount"] Amount,
   line_22900_OtherEmployExpenses = Field ["Line_22900_OtherEmployExpenses", "Line_22900_Amount"] Amount,
   line_23100_ClergyResDeduction = Field ["Line_23100_ClergyResDeduction", "Line_23100_Amount"] Amount,
   line_23200_OtherDeductions = Field ["Line_23200_OtherDeductions", "Line_23200_Amount"] Amount,
   line_23200_Specify = Field ["Line_23200_OtherDeductions", "Line_23200_Specify"] Textual,
   line_23210 = Field ["Line23210", "Amount"] Amount,
   line_23300_sum = Field ["Line_23300_AddLines", "Line_23300_Amount1"] Amount,
   line_23300_cont = Field ["Line_23300_AddLines", "Line_23300_Amount2"] Amount,
   line_23400_NetBeforeAdjust = Field ["Line_23400_NetBeforeAdjust", "Line_23400_Amount"] Amount,
   line_23500_SocialBenefits = Field ["Line_23500_SocialBenefits", "Line_23500_Amount"] Amount,
   line_23600_NetIncome = Field ["Line_23600_NetIncome", "Line_23600_Amount"] Amount}

page5Fields = Page5 {
   step4_TaxableIncome = within "Step4_TaxableIncome" Rank2.<$> step4Fields,
   partA_FederalTax = within "PartA" Rank2.<$> partAFields "Column" 36,
   partB_FederalTaxCredits = within "PartB" Rank2.<$> partBFields}

step4Fields = Step4 {
   line_23600_NetIncome_2 = Field ["Line_23600_NetIncome_2", "Line_15000_Amount"] Amount,
   line_24400_MilitaryPoliceDeduction = Field ["Line_24400_MilitaryPoliceDeduction", "Line_24400_Amount"] Amount,
   line_24900_SecurityDeductions = Field ["Line_24900_SecurityDeductions", "Line_24900_Amount"] Amount,
   line_25000_OtherPayDeductions = Field ["Line_25000_OtherPayDeductions", "Line_25000_Amount"] Amount,
   line_25100_PartnershipLosses = Field ["Line_25100_PartnershipLosses", "Line_25100_Amount"] Amount,
   line_25200_NoncapitalLosses = Field ["Line_25200_NoncapitalLosses", "Line_25200_Amount"] Amount,
   line_25300_NetCapitalLosses = Field ["Line_25300_NetCapitalLosses", "Line_25300_Amount"] Amount,
   line_25400_CapitalGainsDeduction = Field ["Line_25400_CapitalGainsDeduction", "Line_25400_Amount"] Amount,
   line_25500_NorthernDeductions = Field ["Line_25500_NorthernDeductions", "Line_25500_Amount"] Amount,
   line_25600_AdditionalDeductions_Amount = Field ["Line_25600_AdditionalDeductions", "Line_25600_Amount"] Amount,
   line_25600_AdditionalDeductions_Specify = Field ["Line_25600_AdditionalDeductions", "Line_25600_Specify"] Textual,
   line_25700_AddLines_sum = Field ["Line_25700_AddLines", "Line_25700_Amount1"] Amount,
   line_25700_AddLines_cont = Field ["Line_25700_AddLines", "Line_25700_Amount2"] Amount,
   line_26000_TaxableIncome = Field ["Line_26000_TaxableIncome", "Line_26000_Amount"] Amount}

partAFields :: Text -> Int -> Page5PartA FieldConst
partAFields columnPrefix startLine = Page5PartA {
   column1 = column 1 0 0.15 0,
   column2 = column 2 50_197.00 0.205 7_529.55,
   column3 = column 3 100_392.00 0.26 17_819.53,
   column4 = column 4 155_625.00 0.29 32_180.11,
   column5 = column 5 221_708.00 0.33 51_344.18}
   where column n threshold rate baseTax = within (columnPrefix <> toText (decimal n)) Rank2.<$> TaxIncomeBracket {
            income = Field [toText $ "Line" <> decimal startLine <> "Amount" <> decimal n] Amount,
            threshold = Field [toText $ "Line" <> decimal (startLine + 1) <> "Amount" <> decimal n]
                        $ Constant threshold Amount,
            overThreshold = Field [toText $ "Line" <> decimal (startLine + 2) <> "Amount" <> decimal n] Amount,
            rate = Field [toText $ "Line" <> decimal (startLine + 3) <> "Rate" <> decimal n] $ Constant rate Percent,
            timesRate = Field [toText $ "Line" <> decimal (startLine + 4) <> "Amount" <> decimal n] Amount,
            baseTax = Field [toText $ "Line" <> decimal (startLine + 5) <> "Amount" <> decimal n]
                      $ Constant baseTax Amount,
            equalsTax = Field [toText $ "Line" <> decimal (startLine + 6) <> "Amount" <> decimal n] Amount}
         toText = toStrict . toLazyText

partBFields = Page5PartB {
   line30000 = Field ["Line30000_Sub", "Line1_Amount"] Amount,
   line30100 = Field ["Line30100_Sub", "Line2_Amount"] Amount,
   line30300 = Field ["Line30300_Sub", "Line3_Amount"] Amount,
   line30400 = Field ["Line30400_Sub", "Line4_Amount"] Amount,
   line30425 = Field ["Line30425_sub", "Line4_Amount"] Amount,
   line30450 = Field ["Line30450_Sub", "Line6_Amount"] Amount,
   line30499_ChildrenNum = Field ["Line30500", "Line7_ChildrenNum"] Count,
   line30500 = Field ["Line30500", "Line7_Amount"] Amount,
   line_81 = Field ["Line_81", "Line30_Amount"] Amount}
                                                                                           
page6Fields = Page6 {
   line82 = Field ["Line_79", "Line43Amount"] Amount,
   line30800 = Field ["CPP_QPP_Sub", "Line30800_Sub", "Line8_Amount"] Amount,
   line31000 = Field ["CPP_QPP_Sub", "Line31000_Sub", "Line9_Amount"] Amount,
   line31200 = Field ["EIPremiums_Sub", "Line31200_Sub", "Line10_Amount"] Amount,
   line31205 = NoField,
   line31210 = NoField,
   line31215 = NoField,
   line31217 = Field ["Line31217_Sub", "Line11_Amount"] Amount,
   line31220 = Field ["Line31220_Sub", "Line12_Amount"] Amount,
   line31240 = Field ["Line31240_Sub", "Line13_Amount"] Amount,
   line31260 = Field ["Line31260_Sub", "Line14_Amount"] Amount,
   line31270 = Field ["Line31270_Sub", "Line15_Amount"] Amount,
   line31285 = Field ["Line31285_sub", "Line16_Amount"] Amount,
   line31300 = Field ["Line31300_Sub", "Line17_Amount"] Amount,
   line31350 = Field ["Line31350_Sub", "Amount"] Amount,
   line94_sum = Field ["Line93", "Amount1"] Amount,
   line94_cont = Field ["Line93", "Amount2"] Amount,
   line31400 = Field ["Line31400_Sub", "Line18_Amount"] Amount,
   line96 = Field ["Line95", "Amount1"] Amount,
   line31600 = Field ["Line31600_Sub", "Line19_Amount"] Amount,
   line31800 = Field ["Line31800_Sub", "Line20_Amount"] Amount,
   line99 = Field ["Line98", "Amount1"] Amount,
   line31900 = Field ["Line31900_Sub", "Line21_Amount"] Amount,
   line32300 = Field ["Line32300_Sub", "Line22_Amount"] Amount,
   line32400 = Field ["Line32400_Sub", "Line23_Amount"] Amount,
   line32600 = Field ["Line32600_Sub", "Line24_Amount"] Amount,
   line104 = Field ["Line103", "Amount1"] Amount,
   medical_expenses = within "Medical_expenses" Rank2.<$> page6MedicalExpensesFields,
   line33200_sum = Field ["Line33200_Sub", "Line29_Amount1"] Amount,
   line33200_cont = Field ["Line33200_Sub", "Line29_Amount2"] Amount,
   line33500 = Field ["Line33500_Sub", "Line30_Amount"] Amount,
   line112 = Field ["Line31_Sub", "Line31_Rate"] $ Constant 0.15 Percent,
   line33800 = Field ["Line33800_Sub", "Line32_Amount"] Amount,
   line34900 = Field ["Line34900_Sub", "Line33_Amount"] Amount,
   line35000 = Field ["Line35000_Sub", "Line34_Amount"] Amount}

page6MedicalExpensesFields = MedicalExpenses {
   familyExpenses = Field ["Line104", "Amount"] Amount,
   taxableIncome = Field ["Line105", "Amount1"] Amount,
   taxableIncomeFraction = Field ["Line105", "Amount2"] Amount,
   threshold = Field ["Line26_Sub", "Line26_Amount"] Amount,
   difference = Field ["Line27_Sub", "Line27_Amount"] Amount,
   otherDependants = Field ["Line33199_Sub", "Line28_Amount"] Amount}

page7Fields = Page7 {
   partC_NetFederalTax = within "PartC" Rank2.<$> partCFields,
   step6_RefundOrBalanceOwing = within "Step6" Rank2.<$> page7step6Fields}

partCFields = Page7PartC {
   line116 = Field ["Line108_sub", "Line43Amount"] Amount,
   line40424 = Field ["Line40424_Sub", "Line44Amount"] Amount,
   line40400 = Field ["Line40400_Sub", "Line45Amount1"] Amount,
   line119 = Field ["Line35000_sub", "Line46Amount"] Amount,
   line40425 = Field ["Line40425_Sub", "Line47Amount"] Amount,
   line40427 = Field ["Line40427_Sub", "Line48Amount"] Amount,
   line122_sum = Field ["Line114_sub", "Line49Amount1"] Amount,
   line122_cont = Field ["Line114_sub", "Line49Amount2"] Amount,
   line42900 = Field ["Line122", "Amount"] Amount,
   line124 = Field ["Line123", "Amount"] Amount,
   line125 = Field ["Line124", "Amount"] Amount,
   line40500 = Field ["Line40500_sub", "Line51Amount"] Amount,
   line127 = Field ["Line40600_sub", "Line52Amount"] Amount,
   line128 = Field ["Line127", "Amount"] Amount,
   line129 = Field ["Line128", "Amount"] Amount,
   line130 = Field ["Line129", "Amount"] Amount,
   line40600 = Field ["Line130", "Amount"] Amount,
   line40900 = Field ["Line131", "F40900", "Amount"] Amount,
   line41000 = Field ["Line131", "Amount"] Amount,
   line41200 = Field ["Line41200_sub", "Line55Amount"] Amount,
   line41300 = Field ["Line41400_sub", "Line56Amount1"] Amount,
   line41400 = Field ["Line41400_sub", "Line56Amount2"] Amount,
   line41600_sum = Field ["Line41600_sub", "Line57Amount1"] Amount,
   line41600_cont = Field ["Line41600_sub", "Line57Amount2"] Amount,
   line41700 = Field ["Line41700_sub", "Line58Amount"] Amount,
   line41500 = Field ["Line41500_sub", "Line59Amount"] Amount,
   line41800 = Field ["Line41800_sub", "Line61Amount"] Amount,
   line42000 = Field ["Line42000_sub", "Line60Amount"] Amount}

page7step6Fields = Page7Step6 {
   line140 = Field ["Line_Y", "Line_42120_Amount"] Amount,
   line_42100_CPPContributions = Field ["Line_42100_CPPContributions", "Line_42100_Amount"] Amount,
   line_42120_EIPremiums = Field ["Line_42120_EIPremiums", "Line_42120_Amount"] Amount,
   line_42200_SocialBenefits = Field ["Line_42200_SocialBenefits", "Line_42200_Amount"] Amount,
   line_42800_ProvTerrTax = Field ["Line_42800_ProvTerrTax", "Line_42800_Amount"] Amount,
   line_43200_FirstNationsTax = NoField,
   line_43500_TotalPayable = Field ["Line_43500_TotalPayable", "Line_43500_Amount"] Amount}
                                                                                           
page8Fields = Page8 {
   step6_RefundOrBalanceOwing = within "Step6" Rank2.<$> page8step6Fields,
   line48400_Refund = Field ["Line48400_48500", "Line48400", "Line_48400_Amount"] Amount,
   line48500_BalanceOwing = Field ["Line48400_48500", "Line48500", "Line_48500_Amount"] Amount,
   telephone = Field ["Certification", "Telephone"] Amount,
   date = Field ["Certification", "Date"] Amount,
   taxPreparer = within "Line_49000_IfFeeWasCharged" Rank2.<$> taxPreparerFields,
   line_1_ONOpportunitiesFund = Field ["ONOpportunitiesFund2", "Line_1", "Amount"] Amount,
   line_46500 = Field ["ONOpportunitiesFund2", "Line_2", "Amount"] Amount,
   line_46600 = Field ["ONOpportunitiesFund2", "Line_3", "Amount"] Amount}

page8step6Fields = Page8Step6 {
   line_43500_totalpayable = Field ["Line_43500_totalpayable", "Line_42000_Amount"] Amount,
   line_43700_Total_income_tax_ded = Field ["LIne_43700_Total_income_tax_ded", "Line_43700_Amount"] Amount,
   line_43800_TaxTransferQC = NoField,
   line_43850_diff = NoField,
   line_43850_cont = NoField,
   line_42900_copy = NoField,
   line_44000 = Field ["Line_44000Sub", "Line_44000_Amount"] Amount,
   line_44100 = NoField,
   line_44800_CPPOverpayment = Field ["Line_44800_CPPOverpayment", "Line_44800_Amount"] Amount,
   line_45000_EIOverpayment = Field ["Line_45000_EIOverpayment", "Line_45000_Amount"] Amount,
   line_31210_copy = NoField,
   line_45100_diff = NoField,
   line_45100_cont = NoField,
   line_45200_MedicalExpense = Field ["Line_45200_MedicalExpense", "Line_45200_Amount"] Amount,
   line_45300_CWB = Field ["Line_45300_CWB", "Line_45300_Amount"] Amount,
   line_45350_CTC = Field ["Line_45350_CTC", "Line_45300_Amount"] Amount,
   line_45400_InvestmentTaxCredit = Field ["Line_45400_InvestmentTaxCredit", "Line_45400_Amount"] Amount,
   line_45600_TrustTaxCredit = Field ["Line_45600_TrustTaxCredit", "Line_45600_Amount"] Amount,
   line_45700_GST_HST_Rebate = Field ["Line_45700_GST_HST_Rebate", "Line_45700_Amount"] Amount,
   line_46800 = Field ["EligibleEducatorSchoolSypplyTaxCredit", "F46800", "Amount"] Amount,
   line_46900 = Field ["EligibleEducatorSchoolSypplyTaxCredit", "Amount"] Amount,
   line_47555_TaxPaid = Field ["Line_47555_TaxPaid", "Line_47600_Amount"] Amount,
   line_47556 = Field ["Line159", "Amount"] Amount,
   line_47557 = Field ["Line160", "Amount"] Amount,
   line_47600_TaxPaid = Field ["Line_47600_TaxPaid", "Line_47600_Amount"] Amount,
   line_47900_ProvTerrCredits = Field ["Line_47900_ProvTerrCredits", "Line_47900_Amount"] Amount,
   line_48200_sum = Field ["Line_48200_AddLines", "Line_48200_Amount1"] Amount,
   line_48200_cont = Field ["Line_48200_AddLines", "Line_48200_Amount2"] Amount,
   line164_Refund_or_BalanceOwing = Field ["Line_162", "Refund_or_BalanceOwing_Amount"] Amount}

taxPreparerFields = TaxPreparer {
   eFileNumber = Field ["EFileNumber_Comb", "EFile"] Textual,
   nameOfPreparer = Field ["NameOfPreparer"] Textual,
   telephoneOfPreparer = Field ["TelephoneOfPreparer"] Textual,
   line49000_WasAFeeCharged = Field ["Line49000_CheckBoxGroup"] $ Switch' "Line49000_CheckBox_EN"}
