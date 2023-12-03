{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.Province.AB.AB428.FieldNames (ab428Fields) where

import Data.Fixed (Centi)
import Rank2 qualified

import Tax.Canada.Province.AB.AB428.Types
import Tax.Canada.Shared (MedicalExpenses(..), TaxIncomeBracket (..))
import Tax.FDF (Entry (Count, Constant, Amount, Percent), FieldConst (Field, NoField), within)

ab428Fields = within "form1" Rank2.<$> AB428 {
   page1 = within "Page1" Rank2.<$> page1Fields,
   page2 = within "Page2" Rank2.<$> page2Fields,
   page3 = within "Page3" Rank2.<$> page3Fields}


page1Fields = Page1 {
   income = Field ["Line1", "Amount"] Amount,
   partA = within "Chart" Rank2.<$> page1PartAFields,
   partB = page1PartBFields}

page1PartAFields = Page1PartA {
   column1 = within "Column1" Rank2.<$> taxIncomeBracketFields       0 0.10      0,
   column2 = within "Column2" Rank2.<$> taxIncomeBracketFields 134_238 0.12 13_423.80,
   column3 = within "Column3" Rank2.<$> (taxIncomeBracketFields 161_086 0.13 16_645.56){rate = Field ["LIne5", "Percent"] $ Constant 0.13 Percent},
   column4 = within "Column4" Rank2.<$> taxIncomeBracketFields 214_781 0.14 23_625.91,
   column5 = within "Column5" Rank2.<$> taxIncomeBracketFields 322_171 0.15 38_660.51}

taxIncomeBracketFields :: Centi -> Rational -> Centi -> TaxIncomeBracket FieldConst
taxIncomeBracketFields threshold rate baseTax = TaxIncomeBracket {
   income = Field ["Line2", "Amount"] Amount,
   threshold = Field ["Line3", "Amount_Fixed"] $ Constant threshold Amount,
   overThreshold = Field ["Line4", "Amount"] Amount,
   rate = Field ["Line5", "Percent"] $ Constant rate Percent,
   timesRate = Field ["Line6", "Amount"] Amount,
   baseTax = Field ["Line7", "Amount_Fixed"] $ Constant baseTax Amount,
   equalsTax = Field ["Line8", "Amount"] Amount}

page1PartBFields = Page1PartB {
   line9_basic = Field ["Line9", "Amount"] Amount,
   line10_age = Field ["Line10", "Amount"] Amount,
   line11_base = Field ["Line11to13", "Line11", "Amount_Fixed"] $ Constant 19_814 Amount,
   line12_spouseIncome = Field ["Line11to13", "Line12", "Amount"] Amount,
   line13_difference = Field ["Line11to13", "Line13", "Amount1"] Amount,
   line13_cont = Field ["Line11to13", "Line13", "Amount2"] Amount,
   line14_base = Field ["Line14to16", "Line14", "Amount_Fixed"] $ Constant 19_814 Amount,
   line15_dependentIncome = Field ["Line14to16", "Line15", "Amount"] Amount,
   line16_difference = Field ["Line14to16", "Line16", "Amount1"] Amount,
   line16_cont = Field ["Line14to16", "Line16", "Amount2"] Amount,
   line17_infirm = Field ["Line17", "Amount"] Amount,
   line18 = Field ["Line18", "Amount"] Amount,
   line19_cppQpp = Field ["Line19to20", "Line19", "Amount"] Amount,
   line20_cppQpp = Field ["Line19to20", "Line20", "Amount"] Amount,
   line21_employmentInsurance = Field ["Line21to22", "Line21", "Amount"] Amount,
   line22_employmentInsurance = Field ["Line21to22", "Line22", "Amount"] Amount,
   line23_adoption = Field ["Line23", "Amount"] Amount,
   line24_sum = Field ["Line24", "Amount1"] Amount,
   line24_cont = Field ["Line24", "Amount2"] Amount,
   line25 = Field ["Line25", "Amount"] Amount}

page2Fields = Page2 {
  partB = within "PartB_Page2" Rank2.<$> page2PartBFields}

page2PartBFields = Page2PartB {
   line26 = Field ["Line26", "Amount"] Amount,
   line27_pension = Field ["Line27", "Amount"] Amount,
   line28_caregiver = Field ["Line28", "Amount"] Amount,
   line29 = Field ["Line29", "Amount"] Amount,
   line30_disability = Field ["Line30", "Amount"] Amount,
   line31 = Field ["Line31", "Amount"] Amount,
   line32 = Field ["Line32", "Amount"] Amount,
   line33_interest = Field ["Line33", "Amount"] Amount,
   line34_education = Field ["Line34", "Amount"] Amount,
   line35_transferredSpouse = Field ["Line35", "Amount"] Amount,
   line36 = Field ["Line36", "Amount"] Amount,
   medicalExpenses = within "MedicalExp" Rank2.<$> medicalExpensesFields,
   line43 = Field ["Line43", "Amount"] Amount,
   line44_sum = Field ["Line44", "Amount1"] Amount,
   line44_cont = Field ["Line44", "Amount2"] Amount,
   line45 = Field ["Line45", "Amount"] Amount,
   line46_rate = Field ["Line46", "Percent"] $ Constant 0.10 Percent,
   line47_fraction = Field ["Line47", "Amount"] Amount,
   donations = within "DonationGift" Rank2.<$> donationFields,
   line50_sum = Field ["Line50", "Amount1"] Amount,
   line50_cont = Field ["Line50", "Amount2"] Amount,
   line51 = Field ["Line51", "Amount"] Amount}

medicalExpensesFields = MedicalExpenses {
   expenses = Field ["Line37", "Amount"] Amount,
   netIncome = Field ["Line38", "Amount"] Amount,
   incomeRate = Field ["Line39", "PercentAmount"] $ Constant 0.03 Percent,
   fraction = Field ["Line40", "Amount1"] Amount,
   lesser = Field ["Line41", "Amount"] Amount,
   difference = Field ["Line42", "Amount"] Amount}

donationFields = Donations {
   line48_base = Field ["Line48", "Amount1"] Amount,
   line48_fraction = Field ["Line48", "Amount2"] Amount,
   line49_base = Field ["Line49", "Amount1"] Amount,
   line49_fraction = Field ["Line49", "Amount2"] Amount}

page3Fields = Page3 {
   partC = within "PartC" Rank2.<$> partCFields,
   partD = within "PartC" . within "UAITC" Rank2.<$> partDFields}

partCFields = PartC {
   line52_tax = Field ["Line52", "Amount"] Amount,
   line53_splitIncomeTax = Field ["Line53", "Amount"] Amount,
   line54 = Field ["Line54", "Amount"] Amount,
   line55_copy = Field ["Line55", "Amount"] Amount,
   line56_dividendCredits = Field ["Line56", "Amount"] Amount,
   line57_copy = Field ["Line57", "Amount1"] Amount,
   line57_fraction = Field ["Line57", "Amount2"] Amount,
   line58_sum = Field ["Line58", "Amount1"] Amount,
   line58_cont = Field ["Line58", "Amount2"] Amount,
   line59_difference = Field ["Line59", "Amount"] Amount,
   line60_fromT691 = Field ["Line60", "Amount1"] Amount,
   line60_fraction = Field ["Line60", "Amount2"] Amount,
   line61 = Field ["Line61", "Amount"] Amount,
   line62_foreignCredit = Field ["Line62", "Amount"] Amount,
   line63_difference = Field ["Line63", "Amount"] Amount,
   line64_political = Field ["PoliticalCont", "Line64", "Amount"] Amount,
   line65_political = Field ["PoliticalCont", "Line65", "Amount"] Amount,
   line66_tax = Field ["PoliticalCont", "Line66", "Amount"] Amount}

partDFields = PartD {
   line67_investorCredit = Field ["Line67", "Amount"] Amount,
   line68_stockCredit = Field ["Line68", "Amount"] Amount,
   line69_credits = Field ["Line69", "Amount"] Amount}
