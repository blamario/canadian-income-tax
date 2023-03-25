{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.T1.FieldNames where

import Data.Fixed (Centi)
import Data.Text
import qualified Rank2
import Tax.Canada.T1.Types

data FieldConst x = Field [Text]
                  | Checkbox [Text]
                  | RadioButton [Text] Text
                  | RadioButtons [Text] Text
                  | Switch [Text] Text Text Text
                  | Switch' [Text] Text
                  deriving (Eq, Functor, Show)

within :: Text -> FieldConst x -> FieldConst x
within root (Checkbox path) = Checkbox (root:path)
within root (Field path) = Field (root:path)
within root (RadioButton path leaf) = RadioButton (root:path) leaf
within root (RadioButtons path leaf) = RadioButtons (root:path) leaf
within root (Switch path yes no leaf) = Switch (root:path) yes no leaf
within root (Switch' path leaf) = Switch' (root:path) leaf

t1Fields = T1 {
   page1 = within "Page1" Rank2.<$> page1Fields,
   page2 = within "Page2" Rank2.<$> page2Fields,
   page3 = within "Page3" Rank2.<$> page3Fields,
   page4 = within "Page4" Rank2.<$> page4Fields,
   page5 = within "Page5" Rank2.<$> page5Fields,
   page6 = within "Page6" Rank2.<$> page6Fields,
   page7 = within "Page7" Rank2.<$> page7Fields,
   page8 = within "Page8" Rank2.<$> page8Fields}

page1Fields = Page1 {
   identification = within "Identification" Rank2.<$> page1IdentificationFields,
   residence = within "Residence_Info" Rank2.<$> page1ResidenceFields,
   spouse = within "Info_Spouse_CLP" Rank2.<$> page1SpouseFields}

page1IdentificationFields = Identification {
   emailAddress = Field ["EmailAddress"],
   dateDeath_Comb = Field ["DateDeath_Comb_BordersAll", "DateDeath_Comb"],
   postalCode = Field ["PostalCode_Comb_BordersAll", "PostalCode"],
   your_Language = RadioButton ["Your_Language"] "RadioButtonlanguaget",
   iD_City = Field ["ID_City"],
   sIN_Comb = Field ["SIN_Comb_BordersAll", "SIN_Comb"],
   iD_LastName = Field ["ID_LastName"],
   dateBirth_Comb = Field ["DateBirth_Comb_BordersAll", "DateBirth_Comb"],
   iD_FirstNameInitial = Field ["ID_FirstNameInitial"],
   iD_MailingAddress = Field ["ID_MailingAddress"],
   maritalStatus = RadioButtons ["MaritalStatus_Checkbox"] "MaritalStatus",
   iD_RuralRoute = Field ["ID_RuralRoute"],
   iD_POBox = Field ["ID_POBox"],
   prov_DropDown = Field ["Prov_DropDown"]}

page1ResidenceFields = Residence {
   prov_DropDown_Business = Field ["Prov_DropDown-Business"],
   prov_DropDown_Residence = Field ["Prov_DropDown-Residence"],
   date_Departure = Field ["Date_Departure", "DateMMDD_Comb_BordersAll_Std", "DateMMDD_Comb"],
   date_Entry = Field ["Date_Entry", "DateMMDD_Comb_BordersAll_Std", "DateMMDD_Comb"],
   prov_DropDown = Field ["Prov_DropDown"]}
  
page1SpouseFields = Spouse {
   line23600 = Field ["Line23600", "Amount"],
   self_employment = Checkbox ["Self-employment", "Checkbox"],
   spouse_First_Name = Field ["Spouse_First_Name"],
   line11700 = Field ["Line11700", "Amount"],
   line21300 = Field ["Line21300", "Amount"],
   sIN_Comb = Field ["SIN_Comb"]}

page2Fields = Page2 {
   foreign_property = Switch ["Line26600"] "Option1" "Option2" "ForeignProperty_CheckBox",
   tax_exempt = Checkbox ["Tax_exempt", "Exempt", "Spouse_SelfEmployed"],
   electionsCanada = page2ElectionsCanadaFields,
   cai = Checkbox ["CAI", "CAI_ON", "Tick_box"],
   organ_donor = Switch ["Organ_donor", "Question"] "Option1" "Option2" "OrganDonor_CheckBox"
}

page2ElectionsCanadaFields = ElectionsCanada {
   citizenship = Switch ["LineA"] "Option1" "Option2" "A_CheckBox",
   authorization = Switch ["LineB"] "Option1" "Option2" "B_Authorize_CheckBox"}

page3Fields = Page3 {
   line_10400_OtherEmploymentIncome = Field ["Line_10400_OtherEmploymentIncome", "Line_10400_Amount"],
   line_12100_InvestmentIncome = Field ["Line_12100_InvestmentIncome", "Line_12100_Amount"],
   line_11300_OldAgeSecurityPension = Field ["Line_11300_OldAgeSecurityPension", "Line_11300_Amount"],
   line_11500_OtherPensions = Field ["Line_11500_OtherPensions", "Line_11500_Amount"],
   line_13000_OtherIncome = Field ["Line_13000_OtherIncome", "Line_13000_Amount"],
   line_13000_OtherIncomeSource = Field ["Line_13000_Specify"],
   line_11410_DisabilityBenefits = Field ["Line_11410_DisabilityBenefits", "Line_11410_Amount"],
   line_14400_WorkersCompBen = Field ["Line_14400_WorkersCompBen", "Line_14400_Amount"],
   line_11400_CPP_QPP = Field ["Line_11400_CPP_QPP", "Line_11400_Amount"],
   line_25_sum = Field ["Line_25", "Amount1"],
   line_25_cont = Field ["Line_25", "Amount2"],
   line_11700_UCCB = Field ["Line_11700_UCCB", "Line_11700_Amount"],
   line_10100_EmploymentIncome = Field ["Line_10100_EmploymentIncome", "Line_10100_Amount"],
   line_12700_TaxableCapitalGains = Field ["Line_12700_TaxableCapitalGains", "Line_12700_Amount"],
   line_13700_Amount = Field ["Line21", "Line_13700_Amount"],
   line_13699_Amount = Field ["Line21", "Line_13699_Amount"],
   line_11900_EmploymentInsurance = Field ["Line_11900_EmploymentInsurance", "Line_11900_Amount"],
   line_14100_Amount = Field ["Line23", "Line_14100_Amount"],
   line_14099_Amount = Field ["Line23", "Line_14099_Amount"],
   line_10120_Commissions = Field ["Line_10120_Commissions", "Line_10120_Amount"],
   line_11600_ElectedSplitPension = Field ["Line_11600_ElectedSplitPension", "Line_11600_Amount"],
   line_14700_EqualsAmount = Field ["Line_14700_AddLines", "Line_14700_EqualsAmount"],
   line_14700_PlusAmount = Field ["Line_14700_AddLines", "Line_14700_PlusAmount"],
   line_13010_Taxablescholarship = Field ["Line_13010_Taxablescholarship", "Amount"],
   line_19 = Field ["Line_19", "Amount"],
   line_12599_12600_RentalIncome = Field ["Line_12599_12600_RentalIncome", "Line_12599_Amount"],
   line_12600_Amount = Field ["Line_12599_12600_RentalIncome", "Line_12600_Amount"],
   line_12200_PartnershipIncome = Field ["Line_12200_PartnershipIncome", "Line_12200_Amount"],
   line_11905_Employmentmaternity = Field ["Line_11905_Employmentmaternity", "Line_11905_Amount"],
   line_14500_SocialAssistPay = Field ["Line_14500_SocialAssistPay", "Line_14500_Amount"],
   line_15000_TotalIncome = Field ["Line_15000_TotalIncome", "Line_15000_Amount"],
   line_10105_Taxexemptamount = Field ["Line_10105_Taxexemptamount", "Line_10105_Amount"],
   line_12900_RRSPIncome = Field ["Line_12900_RRSPIncome", "Line_12900_Amount"],
   line_12500_RDSP = Field ["Line_12500_RDSP", "Line_12500_Amount"],
   line_26 = Field ["Line_26", "Amount1"],
   line_12000_TaxableDividends = Field ["Line_12000_TaxableDividends", "Amount"],
   line_14600_NetFedSupplements = Field ["Line_14600_NetFedSupplements", "Line_14600_Amount"],
   line_10130_sf = Field ["Line_10130_sf", "Line_10130_Amount"],
   line_12010_OtherTaxableDividends = Field ["Line_12010_OtherTaxableDividends", "Line_12010_Amount"],
   line_11701_UCCBDesignated = Field ["Line_11701_UCCBDesignated", "Line_11701_Amount"],
   line_13499_Amount = Field ["Line20", "Line_13499_Amount"],
   line_13500_Amount = Field ["Line20", "Line_13500_Amount"],
   line_12800_Amount = Field ["Line_12799_12800_SupportPayReceived", "Line_12800_Amount"],
   line_12799_Amount = Field ["Line_12799_12800_SupportPayReceived", "Line_12799_Amount"],
   line_14299_Amount = Field ["Line24", "Line_14299_Amount"],
   line_14300_Amount = Field ["Line24", "Line_14300_Amount"],
   line_13900_Amount = Field ["Line22", "Line_13900_Amount"],
   line_13899_Amount = Field ["Line22", "Line_13899_Amount"]}
                                                                                           
page4Fields = Page4 {
   line_21000_SplitPensionDeduction = Field ["Line_21000_SplitPensionDeduction", "Line_21000_Amount"],
   line_23500_SocialBenefits = Field ["Line_23500_SocialBenefits", "Line_23500_Amount"],
   line_23400_NetBeforeAdjust = Field ["Line_23400_NetBeforeAdjust", "Line_23400_Amount"],
   line_22100_CarryingChargesInterest = Field ["Line_22100_CarryingChargesInterest", "Line_22100_Amount"],
   line_21500_DisabilityDeduction = Field ["Line_21500_DisabilityDeduction", "Line_21500_Amount"],
   line_15000_TotalIncome_2 = Field ["Line_15000_TotalIncome_2", "Line_15000_Amount"],
   line_22900_OtherEmployExpenses = Field ["Line_22900_OtherEmployExpenses", "Line_22900_Amount"],
   line_20800_RRSPDeduction = Field ["Line_20800_RRSPDeduction", "Line_20800_Amount"],
   line_22000_Amount = Field ["Line42", "Line_22000_Amount"],
   line_21999_Amount = Field ["Line42", "Line_21999_Amount"],
   line_21699_Amount = Field ["Line40", "Line_21699_Amount"],
   line_21700_Amount = Field ["Line40", "Line_21700_Amount"],
   line_23210 = Field ["Line23210", "Amount"],
   line_20810_PRPP = Field ["Line_20810_PRPP", "Line_20810_Amount"],
   line_20700_RPPDeduction = Field ["Line_20700_RPPDeduction", "Line_20700_Amount"],
   line_22215_DeductionCPP_QPP = Field ["Line_22215_DeductionCPP_QPP", "Line_22215_Amount"],
   line_23300_sum = Field ["Line_23300_AddLines", "Line_23300_Amount1"],
   line_23300_cont = Field ["Line_23300_AddLines", "Line_23300_Amount2"],
   line_23600_NetIncome = Field ["Line_23600_NetIncome", "Line_23600_Amount"],
   line_21200_Dues = Field ["Line_21200_Dues", "Line_21200_Amount"],
   line_20600_PensionAdjustment = Field ["Line_20600_PensionAdjustment", "Line_20600_Amount"],
   line_22400_XplorationDevExpenses = Field ["Line_22400_XplorationDevExpenses", "Line_22400_Amount"],
   line_23200_OtherDeductions = Field ["Line_23200_OtherDeductions", "Line_23200_Amount"],
   line_23200_Specify = Field ["Line_23200_Specify"],
   line_21900_MovingExpenses = Field ["Line_21900_MovingExpenses", "Line_21900_Amount"],
   line_21400_ChildCareExpenses = Field ["Line_21400_ChildCareExpenses", "Line_21400_Amount"],
   line_22200_CPP_QPP_Contributions = Field ["Line_22200_CPP_QPP_Contributions", "Line_22200_Amount"],
   line_21300_UCCBRepayment = Field ["Line_21300_UCCBRepayment", "Line_21300_Amount"],
   line_23100_ClergyResDeduction = Field ["Line_23100_ClergyResDeduction", "Line_23100_Amount"]}
                                                                                           
page5Fields = Page5 {
   line_25000_OtherPayDeductions = Field ["Line_25000_OtherPayDeductions", "Line_25000_Amount"],
   line_25100_PartnershipLosses = Field ["Line_25100_PartnershipLosses", "Line_25100_Amount"],
   line_25600_AdditionalDeductions_Specify = Field ["Line_25600_AdditionalDeductions", "Line_25600_Specify"],
   line_25600_AdditionalDeductions_Amount = Field ["Line_25600_AdditionalDeductions", "Line_25600_Amount"],
   line_25300_NetCapitalLosses = Field ["Line_25300_NetCapitalLosses", "Line_25300_Amount"],
   line_25200_NoncapitalLosses = Field ["Line_25200_NoncapitalLosses", "Line_25200_Amount"],
   line_24400_MilitaryPoliceDeduction = Field ["Line_24400_MilitaryPoliceDeduction", "Line_24400_Amount"],
   line_26000_TaxableIncome = Field ["Line_26000_TaxableIncome", "Line_26000_Amount"],
   line_23600_NetIncome_2 = Field ["Line_23600_NetIncome_2", "Line_15000_Amount"],
   line_25400_CapitalGainsDeduction = Field ["Line_25400_CapitalGainsDeduction", "Line_25400_Amount"],
   line_24900_SecurityDeductions = Field ["Line_24900_SecurityDeductions", "Line_24900_Amount"],
   line_25500_NorthernDeductions = Field ["Line_25500_NorthernDeductions", "Line_25500_Amount"],
   line_25700_AddLines_sum = Field ["Line_25700_AddLines", "Line_25700_Amount1"],
   line_25700_AddLines_cont = Field ["Line_25700_AddLines", "Line_25700_Amount2"],
   -- Part A
   column1 = TaxIncomeBracket {
       line67_income = Field ["Column1", "Line36Amount1"],
       line69_overThreshold = Field ["Column1", "Line38Amount1"],
       line71_timesRate = Field ["Column1", "Line40Amount1"],
       line73_equalsTax = Field ["Column1", "Line42Amount1"]
       },
   column2 = TaxIncomeBracket {
       line67_income = Field ["Column2", "Line36Amount2"],
       line69_overThreshold = Field ["Column2", "Line38Amount2"],
       line71_timesRate = Field ["Column2", "Line40Amount2"],
       line73_equalsTax = Field ["Column2", "Line42Amount2"]
       },
   column3 = TaxIncomeBracket {
       line67_income = Field ["Column3", "Line36Amount3"],
       line69_overThreshold = Field ["Column3", "Line38Amount3"],
       line71_timesRate = Field ["Column3", "Line40Amount3"],
       line73_equalsTax = Field ["Column3", "Line42Amount3"]
       },
   column4 = TaxIncomeBracket {
       line67_income = Field ["Column4", "Line36Amount4"],
       line69_overThreshold = Field ["Column4", "Line38Amount4"],
       line71_timesRate = Field ["Column4", "Line40Amount4"],
       line73_equalsTax = Field ["Column4", "Line42Amount4"]
       },
   column5 = TaxIncomeBracket {
       line67_income = Field ["Column5", "Line36Amount5"],
       line69_overThreshold = Field ["Column5", "Line38Amount5"],
       line71_timesRate = Field ["Column5", "Line40Amount5"],
       line73_equalsTax = Field ["Column5", "Line42Amount5"]},
   -- Part B
   line30000 = Field ["Line30000_Sub", "Line1_Amount"],
   line30100 = Field ["Line30100_Sub", "Line2_Amount"],
   line30300 = Field ["Line30300_Sub", "Line3_Amount"],
   line30400 = Field ["Line30400_Sub", "Line4_Amount"],
   line30425 = Field ["Line30425_sub", "Line4_Amount"],
   line30450 = Field ["Line30450_Sub", "Line6_Amount"],
   line30499_ChildrenNum = Field ["Line30500", "Line7_ChildrenNum"],
   line30500 = Field ["Line30500", "Line7_Amount"],
   line_81 = Field ["Line_81", "Line30_Amount"]}
                                                                                           
page6Fields = Page6 {
   -- CPP_QPP
   line30800 = Field ["Line30800_Sub", "Line8_Amount"],
   line31000 = Field ["Line31000_Sub", "Line9_Amount"],
   -- EI
   line31200 = Field ["Line31200_Sub", "Line10_Amount"],
   line31217 = Field ["Line31217_Sub", "Line11_Amount"],
   line31220 = Field ["Line31220_Sub", "Line12_Amount"],
   line31240 = Field ["Line31240_Sub", "Line13_Amount"],
   line31260 = Field ["Line31260_Sub", "Line14_Amount"],
   line31270 = Field ["Line31270_Sub", "Line15_Amount"],
   line31285 = Field ["Line31285_sub", "Line16_Amount"],
   line31300 = Field ["Line31300_Sub", "Line17_Amount"],
   line31350 = Field ["Line31350_Sub", "Amount"],
   line94_sum = Field ["Line93", "Amount1"],
   line94_cont = Field ["Line93", "Amount2"],
   line31400 = Field ["Line31400_Sub", "Line18_Amount"],
   line96 = Field ["Line95", "Amount1"],
   line31600 = Field ["Line31600_Sub", "Line19_Amount"],
   line31800 = Field ["Line31800_Sub", "Line20_Amount"],
   line99 = Field ["Line98", "Amount1"],
   line31900 = Field ["Line31900_Sub", "Line21_Amount"],
   line32300 = Field ["Line32300_Sub", "Line22_Amount"],
   line32400 = Field ["Line32400_Sub", "Line23_Amount"],
   line32600 = Field ["Line32600_Sub", "Line24_Amount"],
   line104 = Field ["Line103", "Amount1"],
   medical_expenses = page6MedicalExpensesFields,
   line33200_sum = Field ["Line33200_Sub", "Line29_Amount1"],
   line33200_cont = Field ["Line33200_Sub", "Line29_Amount2"],
   line33500 = Field ["Line33500_Sub", "Line30_Amount"],
   line33800 = Field ["Line33800_Sub", "Line32_Amount"],
   line_79 = Field ["Line_79", "Line43Amount"],
   line34900 = Field ["Line34900_Sub", "Line33_Amount"],
   line35000 = Field ["Line35000_Sub", "Line34_Amount"]}

page6MedicalExpensesFields = MedicalExpenses {
   familyExpenses = Field ["Line104", "Amount"],
   taxableIncome = Field ["Line105", "Amount1"],
   taxableIncomeFraction = Field ["Line105", "Amount2"],
   threshold = Field ["Line26_Sub", "Line26_Amount"],
   difference = Field ["Line27_Sub", "Line27_Amount"],
   otherDependants = Field ["Line33199_Sub", "Line28_Amount"]}

page7Fields = Page7 {
   -- Part C
   line116 = Field ["Line108_sub", "Line43Amount"],
   line40424 = Field ["Line40424_Sub", "Line44Amount"],
   line40400 = Field ["Line40400_Sub", "Line45Amount1"],
   line119 = Field ["Line35000_sub", "Line46Amount"],
   line40425 = Field ["Line40425_Sub", "Line47Amount"],
   line40427 = Field ["Line40427_Sub", "Line48Amount"],
   line122_sum = Field ["Line114_sub", "Line49Amount1"],
   line122_cont = Field ["Line114_sub", "Line49Amount1"],
   line123 = Field ["Line122", "Amount"],
   line124 = Field ["Line123", "Amount"],
   line125 = Field ["Line124", "Amount"],
   line40500 = Field ["Line40500_sub", "Line51Amount"],
   line128 = Field ["Line127", "Amount"],
   line129 = Field ["Line128", "Amount"],
   line130 = Field ["Line129", "Amount"],
   line40600 = Field ["Line40600_sub", "Line52Amount"],
   line40900 = Field ["Line131", "F40900", "Amount"],
   line41000 = Field ["Line131", "Amount"],
   line41200 = Field ["Line41200_sub", "Line55Amount"],
   line41300 = Field ["Line41400_sub", "Line56Amount1"],
   line41400 = Field ["Line41400_sub", "Line56Amount2"],
   line41600_sum = Field ["Line41600_sub", "Line57Amount1"],
   line41600_cont = Field ["Line41600_sub", "Line57Amount2"],
   line41700 = Field ["Line41700_sub", "Line58Amount"],
   line41500 = Field ["Line41500_sub", "Line59Amount"],
   line41800 = Field ["Line41800_sub", "Line61Amount"],
   line42000 = Field ["Line42000_sub", "Line60Amount"],
   -- Step 6
   line140 = Field ["Line_Y", "Line_42120_Amount"],
   line_42100_CPPContributions = Field ["Line_42100_CPPContributions", "Line_42100_Amount"],
   line_42120_EIPremiums = Field ["Line_42120_EIPremiums", "Line_42120_Amount"],
   line_42200_SocialBenefits = Field ["Line_42200_SocialBenefits", "Line_42200_Amount"],
   line_42800_ProvTerrTax = Field ["Line_42800_ProvTerrTax", "Line_42800_Amount"],
   line_43500_TotalPayable = Field ["Line_43500_TotalPayable", "Line_43500_Amount"]}
                                                                                           
page8Fields = Page8 {
   line_43500_totalpayable = Field ["Line_43500_totalpayable", "Line_42000_Amount"],
   line_43700_Total_income_tax_ded = Field ["LIne_43700_Total_income_tax_ded", "Line_43700_Amount"],
   line_44000Sub = Field ["Line_44000Sub", "Line_44000_Amount"],
   line_44800_CPPOverpayment = Field ["Line_44800_CPPOverpayment", "Line_44800_Amount"],
   line_45000_EIOverpayment = Field ["Line_45000_EIOverpayment", "Line_45000_Amount"],
   line_45200_MedicalExpense = Field ["Line_45200_MedicalExpense", "Line_45200_Amount"],
   line_45300_CWB = Field ["Line_45300_CWB", "Line_45300_Amount"],
   line_45350_CTC = Field ["Line_45350_CTC", "Line_45300_Amount"],
   line_45400_InvestmentTaxCredit = Field ["Line_45400_InvestmentTaxCredit", "Line_45400_Amount"],
   line_45600_TrustTaxCredit = Field ["Line_45600_TrustTaxCredit", "Line_45600_Amount"],
   line_45700_GST_HST_Rebate = Field ["Line_45700_GST_HST_Rebate", "Line_45700_Amount"],
   line_46800 = Field ["EligibleEducatorSchoolSypplyTaxCredit", "F46800", "Amount"],
   line_46900 = Field ["EligibleEducatorSchoolSypplyTaxCredit", "Amount"],
   line_47555_TaxPaid = Field ["Line_47555_TaxPaid", "Line_47600_Amount"],
   line_47556 = Field ["Line159", "Amount"],
   line_47557 = Field ["Line160", "Amount"],
   line_47600_TaxPaid = Field ["Line_47600_TaxPaid", "Line_47600_Amount"],
   line_47900_ProvTerrCredits = Field ["Line_47900_ProvTerrCredits", "Line_47900_Amount"],
   line_48200_sum = Field ["Line_48200_AddLines", "Line_48200_Amount1"],
   line_48200_cont = Field ["Line_48200_AddLines", "Line_48200_Amount2"],
   line164_Refund_or_BalanceOwing = Field ["Line_162", "Refund_or_BalanceOwing_Amount"],
   line48400_Refund = Field ["Line48400_48500", "Line48400", "Line_48400_Amount"],
   line48500_BalanceOwing = Field ["Line48400_48500", "Line48500", "Line_48500_Amount"],

   telephone = Field ["Certification", "Telephone"],
   date = Field ["Certification", "Date"],
   taxPreparer = within "Line_49000_IfFeeWasCharged" Rank2.<$> taxPreparerFields,
   line_1_ONOpportunitiesFund = Field ["ONOpportunitiesFund2", "Line_1", "Amount"],
   line_46500 = Field ["ONOpportunitiesFund2", "Line_2", "Amount"],
   line_46600 = Field ["ONOpportunitiesFund2", "Line_3", "Amount"]}

taxPreparerFields = TaxPreparer {
   eFileNumber = Field ["EFileNumber_Comb", "EFile"],
   nameOfPreparer = Field ["NameOfPreparer"],
   telephoneOfPreparer = Field ["TelephoneOfPreparer"],
   line49000_WasAFeeCharged = Switch' ["Line49000_CheckBoxGroup"] "Line49000_CheckBox_EN"}
