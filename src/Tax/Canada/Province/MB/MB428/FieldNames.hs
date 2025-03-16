{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.Province.MB.MB428.FieldNames (mb428Fields) where

import Data.Fixed (Centi)
import Rank2 qualified

import Tax.Canada.Province.MB.MB428.Types
import Tax.Canada.Shared (BaseCredit(..), MedicalExpenses(..), TaxIncomeBracket (..), subCalculationFields)
import Tax.FDF (Entry (Constant, Amount, Percent), FieldConst (Field), within)

mb428Fields :: MB428 FieldConst
mb428Fields = within "form1" Rank2.<$> MB428 {
   page1 = within "Page1" Rank2.<$> page1Fields,
   page2 = within "Page2" Rank2.<$> page2Fields,
   page3 = within "Page3" Rank2.<$> page3Fields}


page1Fields :: Page1 FieldConst
page1Fields = Page1 {
   income = Field ["Line1", "Amount"] Amount,
   partA = within "Table" Rank2.<$> page1PartAFields,
   partB = page1PartBFields}

page1PartAFields :: Page1PartA FieldConst
page1PartAFields = Page1PartA {
   column1 = within "Column1" Rank2.<$> taxIncomeBracketFields       0  0.108       0,
   column2 = within "Column2" Rank2.<$> taxIncomeBracketFields   47_000 0.1275   5076,
   column3 = within "Column3" Rank2.<$> (taxIncomeBracketFields 100_000 0.174  11_833.5)}

taxIncomeBracketFields :: Centi -> Rational -> Centi -> TaxIncomeBracket FieldConst
taxIncomeBracketFields threshold rate baseTax = TaxIncomeBracket {
   income = Field ["Line2", "Amount"] Amount,
   threshold = Field ["Line3", "Amount"] $ Constant threshold Amount,
   overThreshold = Field ["Line4", "Amount"] Amount,
   rate = Field ["Line5", "Percent"] $ Constant rate Percent,
   timesRate = Field ["Line6", "Amount"] Amount,
   baseTax = Field ["Line7", "Amount"] $ Constant baseTax Amount,
   equalsTax = Field ["Line8", "Amount"] Amount}

page1PartBFields :: Page1PartB FieldConst
page1PartBFields = Page1PartB {
   line9_basic = Field ["Line9", "Amount"] Amount,
   line10_age = Field ["Line10", "Amount"] Amount,
   spouseAmount = BaseCredit{
       baseAmount = Field ["Line11", "Amount"] $ Constant 9_134 Amount,
       reduction = Field ["Line12", "Amount"] Amount,
       difference = Field ["line13", "Amount1"] Amount,
       cont = Field ["line13", "Amount2"] Amount},
   dependantAmount = BaseCredit{
       baseAmount = Field ["Line14", "Amount"] $ Constant 9_134 Amount,
       reduction = Field ["Line15", "Amount"] Amount,
       difference = Field ["Line16", "Amount1"] Amount,
       cont = Field ["Line16", "Amount2"] Amount},
   line17_infirm = Field ["Line17", "Amount"] Amount,
   line18 = Field ["Line18", "Amount"] Amount,
   line19_cppQpp = Field ["Line19", "Amount"] Amount,
   line20_cppQpp = Field ["Line20", "Amount"] Amount,
   line21_employmentInsurance = Field ["Line21", "Amount"] Amount,
   line22_employmentInsurance = Field ["Line22", "Amount"] Amount,
   line23_firefighters = Field ["Line23", "Amount"] Amount,
   line24_rescue = Field ["Line24", "Amount"] Amount,
   line25_fitness = Field ["Line25", "Amount"] Amount,
   line26_arts = Field ["Line26", "Amount"] Amount,
   line27_adoption = Field ["Line27", "Amount"] Amount,
   line28_sum = subCalculationFields "Line28" ["Amount1"] ["Amount2"],
   line29 = Field ["Line29", "Amount"] Amount}

page2Fields :: Page2 FieldConst
page2Fields = Page2 {
  partB = within "PartB" Rank2.<$> page2PartBFields}

page2PartBFields :: Page2PartB FieldConst
page2PartBFields = Page2PartB {
   line30 = Field ["Line30", "Amount"] Amount,
   line31_pension = Field ["Line31", "Amount"] Amount,
   line32_caregiver = Field ["Line32", "Amount"] Amount,
   line33 = Field ["Line33", "Amount"] Amount,
   line34_disability = Field ["Line34", "Amount"] Amount,
   line35 = Field ["Line35", "Amount"] Amount,
   line36 = Field ["Line36", "Amount"] Amount,
   line37_interest = Field ["Line37", "Amount"] Amount,
   line38_education = Field ["Line38", "Amount"] Amount,
   line39_transferredChild = Field ["Line39", "Amount"] Amount,
   line40_transferredSpouse = Field ["Line40", "Amount"] Amount,
   line41_family = Field ["Line41", "Amount"] Amount,
   line42_sum = Field ["Line42", "Amount"] Amount,
   medicalExpenses = within "MedicalExpenses" Rank2.<$> medicalExpensesFields,
   line49 = Field ["Line49", "Amount"] Amount,
   line50_sum = subCalculationFields "Line50" ["Amount1"] ["Amount2"],
   line51 = Field ["Line51", "Amount"] Amount,
   line52_rate = Field ["Line52", "Percent_ReadOnly"] $ Constant 0.108 Percent,
   line53_fraction = Field ["Line53", "Amount"] Amount,
   donations = donationFields,
   line56_sum = subCalculationFields "Line56" ["Amount1"] ["Amount2"],
   line57 = Field ["Line57", "Amount"] Amount}

medicalExpensesFields :: MedicalExpenses FieldConst
medicalExpensesFields = MedicalExpenses {
   expenses = Field ["Line43", "Amount"] Amount,
   netIncome = Field ["Line44", "Amount"] Amount,
   incomeRate = Field ["Line45", "Percent_ReadOnly"] $ Constant 0.03 Percent,
   fraction = Field ["Line46", "Amount"] Amount,
   lesser = Field ["Line47", "Amount"] Amount,
   difference = Field ["Line48", "Amount"] Amount}

donationFields :: Donations FieldConst
donationFields = Donations {
   line54_base = Field ["Line54", "Amount1"] Amount,
   line54_fraction = Field ["Line54", "Amount2"] Amount,
   line55_base = Field ["Line55", "Amount1"] Amount,
   line55_fraction = Field ["Line55", "Amount2"] Amount}

page3Fields :: Page3 FieldConst
page3Fields = Page3 {
   partC = within "PartC" Rank2.<$> partCFields}

partCFields :: PartC FieldConst
partCFields = PartC {
   line58_tax = Field ["Line58", "Amount"] Amount,
   line59_splitIncomeTax = Field ["Line59", "Amount"] Amount,
   line60 = Field ["Line60", "Amount"] Amount,
   line61_copy = Field ["Line61", "Amount"] Amount,
   line62_dividendCredits = Field ["Line62", "Amount"] Amount,
   line63_copy = Field ["Line63", "Amount1"] Amount,
   line63_fraction = Field ["Line63", "Amount2"] Amount,
   line64_sum = subCalculationFields "Line64" ["Amount1"] ["Amount2"],
   line65_difference = Field ["Line65", "Amount"] Amount,
   line66_fromT691 = Field ["Line66", "Amount1"] Amount,
   line66_fraction = Field ["Line66", "Amount2"] Amount,
   line67 = Field ["Line67", "Amount"] Amount,
   line68_political = Field ["Line68", "Amount"] Amount,
   line69_political = Field ["Line69", "Amount"] Amount,
   line70_difference = Field ["Line70", "Amount"] Amount,
   line71_labour = Field ["Line71", "Amount"] Amount,
   line72_difference = Field ["Line72", "Amount"] Amount,
   line73_foreignCredit = Field ["Line73", "Amount"] Amount,
   line74_difference = Field ["Line74", "Amount"] Amount,
   line75_community = Field ["Line75", "Amount"] Amount,
   line76_difference = Field ["Line76", "Amount"] Amount,
   line77_venture = Field ["Line77", "Amount"] Amount,
   line78_difference = Field ["Line78", "Amount"] Amount,
   line79_sharePurchase = Field ["Line79", "Amount"] Amount,
   line80_difference = Field ["Line80", "Amount"] Amount,
   line81_mineral = Field ["Line81", "Amount"] Amount,
   line82_tax = Field ["Line82", "Amount"] Amount}
