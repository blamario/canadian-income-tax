{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.Province.BC.BC428.FieldNames (bc428Fields) where

import Data.Fixed (Centi)
import Rank2 qualified

import Tax.Canada.Province.BC.BC428.Types
import Tax.Canada.Shared (TaxIncomeBracket (..))
import Tax.FDF (Entry (Count, Constant, Amount, Percent), FieldConst (Field, NoField), within)

bc428Fields = within "form1" Rank2.<$> BC428 {
   page1 = within "Page1" Rank2.<$> page1Fields,
   page2 = within "Page2" Rank2.<$> page2Fields,
   page3 = within "Page3" Rank2.<$> page3Fields}


page1Fields = Page1 {
   partA = within "PartA" Rank2.<$> page1PartAFields,
   partB = within "PartB" Rank2.<$> page1PartBFields}

page1PartAFields = Page1PartA {
   income = Field ["Line1", "Amount"] Amount,
   column1 = within "Column1" Rank2.<$>  taxIncomeBracketFields        0 0.0506      0,
   column2 = within "Column2" Rank2.<$>  taxIncomeBracketFields   43_070 0.077   2_179.34,
   column3 = within "Column3" Rank2.<$>  taxIncomeBracketFields   86_141 0.105   5_495.81,
   column4 = within "Column4" Rank2.<$>  taxIncomeBracketFields   98_901 0.1229  6_835.61,
   column5 = within "Column5" Rank2.<$> (taxIncomeBracketFields' 120_094 0.147   9_440.23){equalsTax = Field ["LIne15", "Amount"] Amount},
   column6 = within "Column6" Rank2.<$> (taxIncomeBracketFields' 162_832 0.168  15_722.71){rate = Field ["Line12", "Percent_amount"] $ Constant 0.168 Percent},
   column7 = within "Column7" Rank2.<$>  taxIncomeBracketFields' 227_091 0.205  26_518.23}

taxIncomeBracketFields :: Centi -> Rational -> Centi -> TaxIncomeBracket FieldConst
taxIncomeBracketFields threshold rate baseTax = TaxIncomeBracket {
   income = Field ["Line2", "Amount"] Amount,
   threshold = Field ["Line3", "Amount_ReadOnly"] $ Constant threshold Amount,
   overThreshold = Field ["Line4", "Amount"] Amount,
   rate = Field ["Line5", "Percent_Amount"] $ Constant rate Percent,
   timesRate = Field ["Line6", "Amount"] Amount,
   baseTax = Field ["Line7", "Amount_ReadOnly"] $ Constant baseTax Amount,
   equalsTax = Field ["Line8", "Amount"] Amount}

taxIncomeBracketFields' :: Centi -> Rational -> Centi -> TaxIncomeBracket FieldConst
taxIncomeBracketFields' threshold rate baseTax = TaxIncomeBracket {
   income = Field ["Line9", "Amount"] Amount,
   threshold = Field ["Line10", "Amount_ReadOnly"] $ Constant threshold Amount,
   overThreshold = Field ["Line11", "Amount"] Amount,
   rate = Field ["Line12", "Percent_Amount"] $ Constant rate Percent,
   timesRate = Field ["Line13", "Amount"] Amount,
   baseTax = Field ["Line14", "Amount_ReadOnly"] $ Constant baseTax Amount,
   equalsTax = Field ["Line15", "Amount"] Amount}

page1PartBFields = Page1PartB {
   line16_basic = Field ["Line16", "Amount"] Amount,
   line17_age = Field ["Line17", "Amount"] Amount,
   line18_base = Field ["Spouse_CPL_Amount", "Line18", "Amount"] $ Constant 10_646 Amount,
   line19_spouseIncome = Field ["Spouse_CPL_Amount", "Line19", "Amount"] Amount,
   line20_difference = Field ["Spouse_CPL_Amount", "Line20", "Amount1"] Amount,
   line20_cont = Field ["Spouse_CPL_Amount", "Line20", "Amount2"] Amount,
   line21_base = Field ["Amount_Eligible_Dependant", "Line21", "Amount"] $ Constant 10_646 Amount,
   line22_dependentIncome = Field ["Amount_Eligible_Dependant", "Line22", "Amount"] Amount,
   line23_difference = Field ["Amount_Eligible_Dependant", "Line23", "Amount1"] Amount,
   line23_cont = Field ["Amount_Eligible_Dependant", "Line23", "Amount2"] Amount,
   line24_caregiver = Field ["Line24", "Amount"] Amount,
   line25 = Field ["Line25", "Amount"] Amount}

page2Fields = Page2 {
  partB = page2PartBFields}

page2PartBFields = Page2PartB {
   line26 = Field ["Line26", "Amount"] Amount,
   line27_cppQpp = Field ["Line27", "Amount"] Amount,
   line28_cppQpp = Field ["Line28", "Amount"] Amount,
   line29_employmentInsurance = Field ["Line29", "Amount"] Amount,
   line30_employmentInsurance = Field ["Line30", "Amount"] Amount,
   line31_firefighters = Field ["Line31", "Amount"] Amount,
   line32_rescue = Field ["Line32", "Amount"] Amount,
   line33_sum = Field ["Line33", "I1", "Amount"] Amount,
   line33_cont = Field ["Line33", "I2", "Amount"] Amount,
   line34_adoption = Field ["Line34", "Amount"] Amount,
   line35 = Field ["Line35", "Amount"] Amount,
   line36_pension = Field ["Line36", "Amount"] Amount,
   line37 = Field ["Line37", "Amount"] Amount,
   line38_disability = Field ["Line38", "Amount"] Amount,
   line39 = Field ["Line39", "Amount"] Amount,
   line40 = Field ["Line40", "Amount"] Amount,
   line41_interest = Field ["Line41", "Amount"] Amount,
   line42_education = Field ["Line42", "Amount"] Amount,
   line43_transferredChild = Field ["Line43", "Amount"] Amount,
   line44_transferredSpouse = Field ["Line44", "Amount"] Amount,
   line45 = Field ["Line45", "Amount"] Amount,
   medicalExpenses = within "MedicalExp" Rank2.<$> medicalExpensesFields,
   line52 = Field ["Line52", "Amount"] Amount,
   line53_sum = Field ["Line53", "Amount1"] Amount,
   line53_cont = Field ["Line53", "Amount2"] Amount,
   line54 = Field ["Line54", "Amount"] Amount,
   line55_rate = Field ["Line55", "PercentAmount_ReadOnly"] $ Constant 0.0506 Percent,
   line56_fraction = Field ["Line56", "Amount"] Amount,
   line57_donations = Field ["Line57", "Amount"] Amount,
   line58 = Field ["Line58", "Amount"] Amount,
   line59_food = Field ["Line59", "Gifts_from_57", "Amount"] Amount,
   line59_fraction = Field ["Line59", "Amount"] Amount,
   line60 = Field ["Line60", "Amount"] Amount}

medicalExpensesFields = MedicalExpenses {
   line46_expenses = Field ["Line46", "Amount"] Amount,
   line47_income = Field ["Line47", "Amount"] Amount,
   line48_rate = Field ["Line48", "PercentAmount_ReadOnly"] $ Constant 0.03 Percent,
   line49_fraction = Field ["Line49", "Amount"] Amount,
   line50_lesser = Field ["Line50", "Amount"] Amount,
   line51_difference = Field ["Line51", "Amount"] Amount}

partCFields = PartC {
   line61_tax = Field ["Line61", "Amount"] Amount,
   line62_splitIncomeTax = Field ["Line62", "Amount"] Amount,
   line63 = Field ["Line63", "Amount"] Amount,
   line64_copy = Field ["Line64", "Amount"] Amount,
   line65_dividendCredits = Field ["Line65", "Amount"] Amount,
   line66_copy = Field ["Line66", "Line40427", "Amount"] Amount,
   line66_fraction = Field ["Line66", "Amount"] Amount,
   line67_sum = Field ["Line67", "Amount1"] Amount,
   line67_cont = Field ["Line67", "Amount2"] Amount,
   line68 = Field ["Line68", "Amount"] Amount,
   line69_copy = Field ["Line69", "Line120", "Amount"] Amount,
   line69_fraction = Field ["Line69", "Amount"] Amount,
   line70 = Field ["Line70", "Amount"] Amount,
   line71_foreignCredit = Field ["Line71", "Amount"] Amount,
   line72 = Field ["Line72", "Amount"] Amount}

page3Fields = Page3 {
   partC = partCFields,
   line73_basicReduction = Field ["Line73", "Amount"] Amount,
   line74_copy = Field ["Line74", "Amount"] Amount,
   line75_base = Field ["Line75", "Amount_ReadOnly"] $ Constant 21_867 Amount,
   line76_difference = Field ["Line76", "Amount"] Amount,
   line77_rate = Field ["Line77", "PercentAmount_ReadOnly"] $ Constant 0.0356 Percent,
   line78_fraction = Field ["Line78", "Amount1"] Amount,
   line78_cont = Field ["Line78", "Amount2"] Amount,
   line79_difference = Field ["Line79", "Amount1"] Amount,
   line79_cont = Field ["Line79", "Amount2"] Amount,
   line80_difference = Field ["Line80", "Amount"] Amount,
   line81_logging = Field ["Line81", "Amount"] Amount,
   line82_difference = Field ["Line82", "Amount"] Amount,
   line83_political = Field ["Line83", "Amount"] Amount,
   line84_political = Field ["Line84", "Amount"] Amount,
   line85_difference = Field ["Line85", "Amount"] Amount,
   line86_esop20 = Field ["Line86", "Amount"] Amount,
   line87_evcc30 = Field ["Line87", "Amount"] Amount,
   line88_sum = Field ["Line88", "Amount1"] Amount,
   line88_cont = Field ["Line88", "Amount2"] Amount,
   line89_difference = Field ["Line89", "Amount"] Amount,
   line90_mining = Field ["Line90", "Amount"] Amount,
   line91_tax = Field ["Line91", "Amount"] Amount}
