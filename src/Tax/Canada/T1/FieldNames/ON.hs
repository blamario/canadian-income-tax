{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.T1.FieldNames.ON where

import Rank2 qualified

import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (fromText, toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Tax.FDF (FieldConst (Field, NoField), Entry (..), within)
import Tax.Canada.Shared (TaxIncomeBracket (..), subCalculationFields)
import Tax.Canada.T1.Types

t1Fields :: T1 FieldConst
t1Fields = within "form1" Rank2.<$> T1 {
   page1 = within "Page1" . within "Return-pg1" Rank2.<$> page1Fields,
   page2 = within "Page2" Rank2.<$> page2Fields,
   page3 = within "Page3" . within "Return-pg3" Rank2.<$> page3Fields,
   page4 = within "Page4" . within "Return-pg4" Rank2.<$> page4Fields,
   page5 = within "Page5" . within "Return-pg5" Rank2.<$> page5Fields,
   page6 = within "Page6" . within "Return-pg6" . within "PartB" Rank2.<$> page6Fields,
   page7 = within "Page7" . within "Return-pg7" Rank2.<$> page7Fields,
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
   line_23600 = Field ["Line23600", "Amount"] Amount,
   self_employment = Field ["Self-employment", "Checkbox"] Checkbox,
   spouse_First_Name = Field ["Spouse_First_Name"] Textual,
   line_11700 = Field ["Line11700", "Amount"] Amount,
   line_21300 = Field ["Line21300", "Amount"] Amount,
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
   line_10100_EmploymentIncome = Field ["Line10100", "Line_10100_Amount"] Amount,
   line_10105_Taxexemptamount = Field ["Line10105", "Line_10105_Amount"] Amount,
   line_10120_Commissions = Field ["Line10120", "Line_10120_Amount"] Amount,
   line_10130_sf = Field ["Line10130", "Line_10130_Amount"] Amount,
   line_10400_OtherEmploymentIncome = Field ["Line10400", "Line_10400_Amount"] Amount,
   line_11300_OldAgeSecurityPension = Field ["Line11300", "Line_11300_Amount"] Amount,
   line_11400_CPP_QPP = Field ["Line11400", "Line_11400_Amount"] Amount,
   line_11410_DisabilityBenefits = Field ["Line11410", "Line_11410_Amount"] Amount,
   line_11500_OtherPensions = Field ["Line11500", "Line_11500_Amount"] Amount,
   line_11600_ElectedSplitPension = Field ["Line11600", "Line_11600_Amount"] Amount,
   line_11700_UCCB = Field ["Line11700", "Line_11700_Amount"] Amount,
   line_11701_UCCBDesignated = Field ["Line11701", "Line_11701_Amount"] Amount,
   line_11900_EmploymentInsurance = Field ["Line11900", "Line_11900_Amount"] Amount,
   line_11905_Employmentmaternity = Field ["Line11905", "Line_11905_Amount"] Amount,
   line_12000_TaxableDividends = Field ["Line12000", "Line_12000_Amount"] Amount,
   line_12010_OtherTaxableDividends = Field ["Line12010", "Line_12010_Amount"] Amount,
   line_12100_InvestmentIncome = Field ["Line12100", "Line_12100_Amount"] Amount,
   line_12200_PartnershipIncome = Field ["Line12200", "Line_12200_Amount"] Amount,
   line_12500_RDSP = Field ["Line12500", "Line_12500_Amount"] Amount,
   line_12599_12600_RentalIncome = Field ["Line12600", "Line12599", "Line_12599_Amount"] Amount,
   line_12600_Amount = Field ["Line12600", "Line_12600_Amount"] Amount,
   line_12700_TaxableCapitalGains = Field ["Line12700", "Line_12700_Amount"] Amount,
   line_12799_Amount = Field ["Line12800", "Line_12799", "Line_12799_Amount"] Amount,
   line_12800_Amount = Field ["Line12800", "Line_12800_Amount"] Amount,
   line_12900_RRSPIncome = Field ["Line12900", "Line_12900_Amount"] Amount,
   line_12905_FHSAIncome = Field ["Line12905", "Line_12905_Amount"] Amount,
   line_12906_OtherFHSAIncome = Field ["Line12906", "Line_12906_Amount"] Amount,
   line_13000_OtherIncome = Field ["Line13000", "Line_13000_Amount"] Amount,
   line_13000_OtherIncomeSource = Field ["Line13000", "Line_13000_Specify"] Textual,
   line_13010_TaxableScholarship = Field ["Line13010", "Line_13010_Amount"] Amount,
   line21_sum = Field ["Line21", "Amount"] Amount,
   selfEmployment = selfEmploymentFields,
   line27_sum = subCalculationFields "Line27" ["Amount1"] ["Amount2"],
   line28_sum = Field ["Line28", "Amount"] Amount,
   line_14400_WorkersCompBen = Field ["Line14400", "Line_14400_Amount"] Amount,
   line_14500_SocialAssistPay = Field ["Line14500", "Line_14500_Amount"] Amount,
   line_14600_NetFedSupplements = Field ["Line14600", "Line_14600_Amount"] Amount,
   line_14700_sum = subCalculationFields "Line14700" ["Line_14700_Amount1"] ["Line_14700_Amount2"],
   line_15000_TotalIncome = Field ["Line15000", "Line_15000_Amount"] Amount}

selfEmploymentFields = SelfEmploymentIncome {
   line_13499_Amount = Field ["Line13500", "Line13499", "Line_13499_Amount"] Amount,
   line_13500_Amount = Field ["Line13500", "Line_13500_Amount"] Amount,
   line_13699_Amount = Field ["Line13700", "Line13699", "Line_13699_Amount"] Amount,
   line_13700_Amount = Field ["Line13700", "Line_13700_Amount"] Amount,
   line_13899_Amount = Field ["Line13900", "Line13899", "Line_13899_Amount"] Amount,
   line_13900_Amount = Field ["Line13900", "Line_13900_Amount"] Amount,
   line_14099_Amount = Field ["Line14100", "Line14099", "Line_14099_Amount"] Amount,
   line_14100_Amount = Field ["Line14100", "Line_14100_Amount"] Amount,
   line_14299_Amount = Field ["Line14300", "Line14299", "Line_14299_Amount"] Amount,
   line_14300_Amount = Field ["Line14300", "Line_14300_Amount"] Amount}

page4Fields = Page4 {
   line_15000_TotalIncome_2 = Field ["Line34", "Amount"] Amount,
   line_20600_PensionAdjustment = Field ["Line20600", "Line_20600_Amount"] Amount,
   line_20700_RPPDeduction = Field ["Line20700", "Line_20700_Amount"] Amount,
   line_20800_RRSPDeduction = Field ["Line20800", "Line_20800_Amount"] Amount,
   line_20805_FHSADeduction = Field ["Line20805", "Line_20805_Amount"] Amount,
   line_20810_PRPP = Field ["Line20810", "Line_20810_Amount"] Amount,
   line_21000_SplitPensionDeduction = Field ["Line21000", "Line_21000_Amount"] Amount,
   line_21200_Dues = Field ["Line21200", "Line_21200_Amount"] Amount,
   line_21300_UCCBRepayment = Field ["Line21300", "Line_21300_Amount"] Amount,
   line_21400_ChildCareExpenses = Field ["Line21400", "Line_21400_Amount"] Amount,
   line_21500_DisabilityDeduction = Field ["Line21500", "Line_21500_Amount"] Amount,
   line_21699_Amount = Field ["Line21700", "Line21699", "Line_21699_Amount"] Amount,
   line_21700_Amount = Field ["Line21700", "Line_21700_Amount"] Amount,
   line_21900_MovingExpenses = Field ["Line21900", "Line_21900_Amount"] Amount,
   line_21999_Amount = Field ["Line22000", "Line21999", "Line_21999_Amount"] Amount,
   line_22000_Amount = Field ["Line22000", "Line_22000_Amount"] Amount,
   line_22100_CarryingChargesInterest = Field ["Line22100", "Line_22100_Amount"] Amount,
   line_22200_CPP_QPP_Contributions = Field ["Line22200", "Line_22200_Amount"] Amount,
   line_22215_DeductionCPP_QPP = Field ["Line22215", "Line_22215_Amount"] Amount,
   line_22300_DeductionPPIP = NoField,
   line_22400_XplorationDevExpenses = Field ["Line22400", "Line_22400_Amount"] Amount,
   line_22900_OtherEmployExpenses = Field ["Line22900", "Line_22900_Amount"] Amount,
   line_23100_ClergyResDeduction = Field ["Line23100", "Line_23100_Amount"] Amount,
   line_23200_OtherDeductions = Field ["Line23200", "Line_23200_Amount"] Amount,
   line_23200_Specify = Field ["Line23200", "Line_23200_Specify"] Textual,
   line_23300_sum = subCalculationFields "Line23300" ["Line_23300_Amount1"] ["Line_23300_Amount2"],
   line_23400_NetBeforeAdjust = Field ["Line23400", "Line_23400_Amount"] Amount,
   line_23500_SocialBenefits = Field ["Line23500", "Line_23500_Amount"] Amount,
   line_23600_NetIncome = Field ["Line23600", "Line_23600_Amount"] Amount}

page5Fields = Page5 {
   step4_TaxableIncome = within "Step4" Rank2.<$> step4Fields,
   partA_FederalTax = within "PartA" Rank2.<$> partAFields "Column" 36,
   partB_FederalTaxCredits = within "PartB" Rank2.<$> partBFields}

step4Fields = Step4 {
   line_23600_NetIncome_2 = Field ["Line57", "Amount"] Amount,
   line_24400_MilitaryPoliceDeduction = Field ["Line24400", "Line_24400_Amount"] Amount,
   line_24900_SecurityDeductions = Field ["Line24900", "Line_24900_Amount"] Amount,
   line_25000_OtherPayDeductions = Field ["Line25000", "Line_25000_Amount"] Amount,
   line_25100_PartnershipLosses = Field ["Line25100", "Line_25100_Amount"] Amount,
   line_25200_NoncapitalLosses = Field ["Line25200", "Line_25200_Amount"] Amount,
   line_25300_NetCapitalLosses = Field ["Line25300", "Line_25300_Amount"] Amount,
   line_25400_CapitalGainsDeduction = Field ["Line25400", "Line_25400_Amount"] Amount,
   line_25500_NorthernDeductions = Field ["Line25500", "Line_25500_Amount"] Amount,
   line_25600_AdditionalDeductions_Amount = Field ["Line25600", "Line_25600_Amount"] Amount,
   line_25600_AdditionalDeductions_Specify = Field ["Line25600", "Line_25600_Specify"] Textual,
   line_25700_sum = subCalculationFields "Line25700" ["Line_25700_Amount1"] ["Line_25700_Amount2"],
   line_26000_TaxableIncome = Field ["Line26000", "Line_26000_Amount"] Amount}

partAFields :: Text -> Int -> Page5PartA FieldConst
partAFields = partAFieldsWith fieldNameAt
   where fieldNameAt line column isRate =
            toText $ "Line" <> decimal line <> (if isRate then "Rate" else "Amount") <> decimal column
         toText = toStrict . toLazyText

partAFieldsWith :: (Int -> Int -> Bool -> Text) -> Text -> Int -> Page5PartA FieldConst
partAFieldsWith fieldNameAt columnPrefix startLine = Page5PartA {
   column1 = column 1 0 0.15 0,
   column2 = column 2 53_359.00 0.205 8_003.85,
   column3 = column 3 106_717.00 0.26 18_942.24,
   column4 = column 4 165_430.00 0.29 34_207.62,
   column5 = column 5 235_675.00 0.33 54_578.67}
   where column n threshold rate baseTax = within (columnPrefix <> toText (decimal n)) Rank2.<$> TaxIncomeBracket {
            income = Field [fieldNameAt startLine n False] Amount,
            threshold = Field [fieldNameAt (startLine + 1) n False] $ Constant threshold Amount,
            overThreshold = Field [fieldNameAt (startLine + 2) n False] Amount,
            rate = Field [fieldNameAt (startLine + 3) n True] $ Constant rate Percent,
            timesRate = Field [fieldNameAt (startLine + 4) n False] Amount,
            baseTax = Field [fieldNameAt (startLine + 5) n False] $ Constant baseTax Amount,
            equalsTax = Field [fieldNameAt (startLine + 6) n False] Amount}
         toText = toStrict . toLazyText

partBFields = Page5PartB {
   line_30000 = Field ["Line30000", "Line_30000_Amount"] Amount,
   line_30100 = Field ["Line30100", "Line_30100_Amount"] Amount,
   line_30300 = Field ["Line30300", "Line_30300_Amount"] Amount,
   line_30400 = Field ["Line30400", "Line_30400_Amount"] Amount,
   line_30425 = Field ["Line30425", "Line_30425_Amount"] Amount,
   line_30450 = Field ["Line30450", "Line_30450_Amount"] Amount,
   line_30499_ChildrenNum = Field ["Line30500", "Line30499", "Line_30499_ChildrenNum"] Count,
   line_30500 = Field ["Line30500", "Line_30500_Amount"] Amount,
   line83_sum = Field ["Line83", "Amount"] Amount}
                                                                                           
page6Fields = Page6 {
   line84_copy = Field ["Line84", "Amount"] Amount,
   line_30800 = Field ["Line30800", "Line_30800_Amount"] Amount,
   line_31000 = Field ["Line31000", "Line_31000_Amount"] Amount,
   line_31200 = Field ["Line31200", "Line_31200_Amount"] Amount,
   line_31205 = NoField,
   line_31210 = NoField,
   line_31215 = NoField,
   line_31217 = Field ["Line31217", "Line_31217_Amount"] Amount,
   line_31220 = Field ["Line31220", "Line_31220_Amount"] Amount,
   line_31240 = Field ["Line31240", "Line_31240_Amount"] Amount,
   line_31260 = Field ["Line31260", "Line_31260_Amount"] Amount,
   line_31270 = Field ["Line31270", "Line_31270_Amount"] Amount,
   line_31285 = Field ["Line31285", "Line_31285_Amount"] Amount,
   line_31300 = Field ["Line31300", "Line_31300_Amount"] Amount,
   line_31350 = Field ["Line31350", "Line_31350_Amount"] Amount,
   line96_sum = subCalculationFields "Line96" ["Amount1"] ["Amount2"],
   line_31400 = Field ["Line31400", "Line_31400_Amount"] Amount,
   line98_sum = Field ["Line98", "Amount"] Amount,
   line_31600 = Field ["Line31600", "Line_31600_Amount"] Amount,
   line_31800 = Field ["Line31800", "Line_31800_Amount"] Amount,
   line101_sum = Field ["Line101", "Amount"] Amount,
   line_31900 = Field ["Line31900", "Line_31900_Amount"] Amount,
   line_32300 = Field ["Line32300", "Line_32300_Amount"] Amount,
   line_32400 = Field ["Line32400", "Line_32400_Amount"] Amount,
   line_32600 = Field ["Line32600", "Line_32600_Amount"] Amount,
   line106_sum = Field ["Line106", "Amount"] Amount,
   medical_expenses = page6MedicalExpensesFields,
   line_33200_sum = subCalculationFields "Line33200" ["Line_33200_Amount1"] ["Line_33200_Amount2"],
   line_33500 = Field ["Line33500", "Line_33500_Amount"] Amount,
   line114_taxCreditRate = Field ["Line114", "Percent"] $ Constant 0.15 Percent,
   line_33800 = Field ["Line33800", "Line_33800_Amount"] Amount,
   line_34900 = Field ["Line34900", "Line_34900_Amount"] Amount,
   line_35000 = Field ["Line35000", "Line_35000_Amount"] Amount}

page6MedicalExpensesFields = MedicalExpenses {
   familyExpenses = Field ["Line33099", "Line_33099_Amount"] Amount,
   taxableIncome = Field ["Line108", "Amount1"] Amount,
   taxableIncomeFraction = Field ["Line108", "Amount2"] Amount,
   threshold = Field ["Line109", "Amount"] Amount,
   difference = Field ["Line110", "Amount"] Amount,
   otherDependants = Field ["Line33199", "Line_33199_Amount"] Amount}

page7Fields = Page7 {
   partC_NetFederalTax = within "PartC" Rank2.<$> partCFields,
   step6_RefundOrBalanceOwing = within "Step6" Rank2.<$> page7step6Fields}

partCFields = Page7PartC {
   line118_copy = Field ["Line118", "Amount"] Amount,
   line_40424 = Field ["Line40424", "Line_40424_Amount"] Amount,
   line_40400 = Field ["Line40400", "Line_40400_Amount"] Amount,
   line121_copy = Field ["Line121", "Amount"] Amount,
   line_40425 = Field ["Line40425", "Line_40425_Amount"] Amount,
   line_40427 = Field ["Line40427", "Line_40427_Amount"] Amount,
   line124_sum = subCalculationFields "Line124" ["Amount1"] ["Amount2"],
   line_42900 = Field ["Line42900", "Line_42900_Amount"] Amount,
   line126_foreignSurtax = Field ["Line126", "Amount"] Amount,
   line127_sum = Field ["Line127", "Amount"] Amount,
   line_40500 = Field ["Line40500", "Line_40500_Amount"] Amount,
   line129_difference = Field ["Line129", "Amount"] Amount,
   line130_recapture = Field ["Line130", "Amount"] Amount,
   line131_sum = Field ["Line131", "Amount"] Amount,
   line132_logging = Field ["Line132", "Amount"] Amount,
   line_40600 = Field ["Line40600", "Line_40600_Amount"] Amount,
   line_40900 = Field ["Line41000", "Line40900", "Line_40900_Amount"] Amount,
   line_41000 = Field ["Line41000", "Line_41000_Amount"] Amount,
   line_41200 = Field ["Line41200", "Line_41200_Amount"] Amount,
   line_41300 = Field ["Line41400", "Line41300", "Line_41300_Amount"] Amount,
   line_41400 = Field ["Line41400", "Line_41400_Amount"] Amount,
   line_41600_sum = subCalculationFields "Line41600" ["Line_41600_Amount1"] ["Line_41600_Amount2"],
   line_41700 = Field ["Line41700", "Line_41700_Amount"] Amount,
   line_41500 = Field ["Line41500", "Line_41500_Amount"] Amount,
   line_41800 = Field ["Line41800", "Line_41800_Amount"] Amount,
   line_42000 = Field ["Line42000", "Line_42000_Amount"] Amount}

page7step6Fields = Page7Step6 {
   line142_copy = Field ["Line14", "Amount"] Amount,
   line_42100_CPPContributions = Field ["Line42100", "Line_42100_Amount"] Amount,
   line_42120_EIPremiums = Field ["Line42120", "Line_42120_Amount"] Amount,
   line_42200_SocialBenefits = Field ["Line42200", "Line_42200_Amount"] Amount,
   line_42800_ProvTerrTax = Field ["Line42800", "Line_42800_Amount"] Amount,
   line_43200_FirstNationsTax = NoField,
   line_43500_TotalPayable = Field ["Line43500", "Line_43500_Amount"] Amount}
                                                                                           
page8Fields = Page8 {
   step6_RefundOrBalanceOwing = within "Step6-Continued" Rank2.<$> page8step6Fields,
   line_48400_Refund = Field ["Refund_or_Balance-owing", "Line48400", "Line_48400_Amount"] Amount,
   line_48500_BalanceOwing = Field ["Refund_or_Balance-owing", "Line48500", "Line_48500_Amount"] Amount,
   telephone = Field ["Certification", "Telephone"] Amount,
   date = Field ["Certification", "Date"] Amount,
   taxPreparer = within "Line_49000_IfFeeWasCharged" Rank2.<$> taxPreparerFields,
   line1_ONOpportunitiesFund = Field ["ONOpportunitiesFund2", "Line_1", "Amount"] Amount,
   line_46500 = Field ["ONOpportunitiesFund2", "Line_2", "Amount"] Amount,
   line_46600 = Field ["ONOpportunitiesFund2", "Line_3", "Amount"] Amount}

page8step6Fields = Page8Step6 {
   line_43500_totalpayable = Field ["Line148", "Amount"] Amount,
   line_43700_Total_income_tax_ded = Field ["Line43700", "Line_43700_Amount"] Amount,
   line_43800_TaxTransferQC = NoField,
   line_43850_diff = Rank2.pure NoField,
   line_42900_copy = NoField,
   line_44000 = Field ["Line44000", "Line_44000_Amount"] Amount,
   line_44100 = NoField,
   line_44800_CPPOverpayment = Field ["Line44800", "Line_44800_Amount"] Amount,
   line_45000_EIOverpayment = Field ["Line45000", "Line_45000_Amount"] Amount,
   line_31210_copy = NoField,
   line_45100_diff = Rank2.pure NoField,
   line_45200_MedicalExpense = Field ["Line45200", "Line_45200_Amount"] Amount,
   line_45300_CWB = Field ["Line45300", "Line_45300_Amount"] Amount,
   line_45350_CTC = Field ["Line45350", "Line_45350_Amount"] Amount,
   line_45355_MHRTC = Field ["Line45355", "Line_45355_Amount"] Amount,
   line_45400_InvestmentTaxCredit = Field ["Line45400", "Line_45400_Amount"] Amount,
   line_45600_TrustTaxCredit = Field ["Line45600", "Line_45600_Amount"] Amount,
   line_45700_GST_HST_Rebate = Field ["Line45700", "Line_45700_Amount"] Amount,
   line_46800 = Field ["Line46900", "Line46800", "Line_46800_Amount"] Amount,
   line_46900 = Field ["Line46900", "Line_46900_Amount"] Amount,
   line_47555_TaxPaid = Field ["Line47555", "Line_47600_Amount"] Amount,
   line_47556 = Field ["Line47556", "Line_47556_Amount"] Amount,
   line_47557 = Field ["Line47557", "Line_47557_Amount"] Amount,
   line_47600_TaxPaid = Field ["Line47600", "Line_47600_Amount"] Amount,
   line_47900_ProvTerrCredits = Field ["Line47900", "Line_47900_Amount"] Amount,
   line_48200_sum = subCalculationFields "Line48200" ["Line_48200_Amount1"] ["Line_48200_Amount2"],
   line164_Refund_or_BalanceOwing = Field ["Line167", "Amount"] Amount}

taxPreparerFields = TaxPreparer {
   eFileNumber = Field ["EFileNumber_Comb", "EFile"] Textual,
   nameOfPreparer = Field ["NameOfPreparer"] Textual,
   telephoneOfPreparer = Field ["TelephoneOfPreparer"] Textual,
   line_49000_WasAFeeCharged = Field ["Line49000_CheckBoxGroup"] $ Switch' "Line49000_CheckBox_EN"}
