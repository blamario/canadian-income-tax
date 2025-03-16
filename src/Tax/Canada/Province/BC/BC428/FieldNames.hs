{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.Province.BC.BC428.FieldNames (bc428Fields) where

import Data.Fixed (Centi)
import Rank2 qualified

import Tax.Canada.Province.BC.BC428.Types
import Tax.Canada.Shared (BaseCredit(..), MedicalExpenses(..), TaxIncomeBracket (..), subCalculationFields)
import Tax.FDF (Entry (Constant, Amount, Percent, Textual), FieldConst (Field), within)

bc428Fields :: BC428 FieldConst
bc428Fields = within "form1" Rank2.<$> BC428 {
   page1 = within "Page1" Rank2.<$> page1Fields,
   page2 = within "Page2" Rank2.<$> page2Fields,
   page3 = within "Page3" Rank2.<$> page3Fields}


page1Fields :: Page1 FieldConst
page1Fields = Page1 {
   partA = within "PartA" Rank2.<$> page1PartAFields,
   partB = within "PartB" Rank2.<$> page1PartBFields}

page1PartAFields :: Page1PartA FieldConst
page1PartAFields = Page1PartA {
   income = Field ["Line1", "Amount"] Amount,
   column1 = within "Column1" Rank2.<$>  taxIncomeBracketFields        0 0.0506      0,
   column2 = within "Column2" Rank2.<$>  taxIncomeBracketFields   47_937 0.077   2_425.61,
   column3 = within "Column3" Rank2.<$>  taxIncomeBracketFields   95_875 0.105   6_116.84,
   column4 = within "Column4" Rank2.<$>  taxIncomeBracketFields  110_076 0.1229  7_607.94,
   column5 = within "Column5" Rank2.<$> (taxIncomeBracketFields' 133_664 0.147  10_506.91){equalsTax = Field ["LIne15", "Amount"] Amount},
   column6 = within "Column6" Rank2.<$> (taxIncomeBracketFields' 181_232 0.168  17_499.40){rate = Field ["Line12", "Percent_ReadOnly"] $ Constant 0.168 Percent},
   column7 = within "Column7" Rank2.<$>  taxIncomeBracketFields' 252_752 0.205  29_514.76}

taxIncomeBracketFields :: Centi -> Rational -> Centi -> TaxIncomeBracket FieldConst
taxIncomeBracketFields threshold rate baseTax = TaxIncomeBracket {
   income = Field ["Line2", "Amount"] Amount,
   threshold = Field ["Line3", "Amount_ReadOnly"] $ Constant threshold Amount,
   overThreshold = Field ["Line4", "Amount"] Amount,
   rate = Field ["Line5", "Percent_ReadOnly"] $ Constant rate Percent,
   timesRate = Field ["Line6", "Amount"] Amount,
   baseTax = Field ["Line7", "Amount_ReadOnly"] $ Constant baseTax Amount,
   equalsTax = Field ["Line8", "Amount"] Amount}

taxIncomeBracketFields' :: Centi -> Rational -> Centi -> TaxIncomeBracket FieldConst
taxIncomeBracketFields' threshold rate baseTax = TaxIncomeBracket {
   income = Field ["Line9", "Amount"] Amount,
   threshold = Field ["Line10", "Amount_ReadOnly"] $ Constant threshold Amount,
   overThreshold = Field ["Line11", "Amount"] Amount,
   rate = Field ["Line12", "Percent_ReadOnly"] $ Constant rate Percent,
   timesRate = Field ["Line13", "Amount"] Amount,
   baseTax = Field ["Line14", "Amount_ReadOnly"] $ Constant baseTax Amount,
   equalsTax = Field ["Line15", "Amount"] Amount}

page1PartBFields :: Page1PartB FieldConst
page1PartBFields = Page1PartB {
   line16_basic = Field ["Line16", "Amount"] Amount,
   line17_age = Field ["Line17", "Amount"] Amount,
   spouseAmount = within "Spouse_CPL_Amount" Rank2.<$> BaseCredit{
       baseAmount = Field ["Line18", "Amount"] $ Constant 11_850 Amount,
       reduction = Field ["Line19", "Amount"] Amount,
       difference = Field ["Line20", "Amount1"] Amount,
       cont = Field ["Line20", "Amount2"] Amount},
   dependantAmount = within "Amount_Eligible_Dependant" Rank2.<$> BaseCredit{
       baseAmount = Field ["Line21", "Amount"] $ Constant 11_850 Amount,
       reduction = Field ["Line22", "Amount"] Amount,
       difference = Field ["Line23", "Amount1"] Amount,
       cont = Field ["Line23", "Amount2"] Amount},
   line24_caregiver = Field ["Line24", "Amount"] Amount,
   line25 = Field ["Line25", "Amount"] Amount}

page2Fields :: Page2 FieldConst
page2Fields = Page2 {
  partB = page2PartBFields}

page2PartBFields :: Page2PartB FieldConst
page2PartBFields = Page2PartB {
   line26 = Field ["Line26", "Amount"] Amount,
   line27_cppQpp = Field ["Line27", "Amount"] Amount,
   line28_cppQpp = Field ["Line28", "Amount"] Amount,
   line29_employmentInsurance = Field ["Line29", "Amount"] Amount,
   line30_employmentInsurance = Field ["Line30", "Amount"] Amount,
   line31_firefighters = Field ["Line31", "Amount"] Amount,
   line32_rescue = Field ["Line32", "Amount"] Amount,
   line33_sum = subCalculationFields "Line33" ["I1", "Amount"] ["I2", "Amount"],
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
   line53_sum = subCalculationFields "Line53" ["Amount1"] ["Amount2"],
   line54 = Field ["Line54", "Amount"] Amount,
   line55_rate = Field ["Line55", "PercentAmount_ReadOnly"] $ Constant 0.0506 Percent,
   line56_fraction = Field ["Line56", "Amount"] Amount,
   line57_donations = Field ["Line57", "Amount"] Amount,
   line58 = Field ["Line58", "Amount"] Amount,
   line59_food = Field ["Line59", "Gifts_from_57", "Amount"] Amount,
   line59_fraction = Field ["Line59", "Amount"] Amount,
   line60 = Field ["Line60", "Amount"] Amount}

medicalExpensesFields :: MedicalExpenses FieldConst
medicalExpensesFields = MedicalExpenses {
   expenses = Field ["Line46", "Amount"] Amount,
   netIncome = Field ["Line47", "Amount"] Amount,
   incomeRate = Field ["Line48", "PercentAmount_ReadOnly"] $ Constant 0.03 Percent,
   fraction = Field ["Line49", "Amount"] Amount,
   lesser = Field ["Line50", "Amount"] Amount,
   difference = Field ["Line51", "Amount"] Amount}

partCFields :: PartC FieldConst
partCFields = PartC {
   line61_tax = Field ["Line61", "Amount"] Amount,
   line62_splitIncomeTax = Field ["Line62", "Amount"] Amount,
   line63 = Field ["Line63", "Amount"] Amount,
   line64_copy = Field ["Line64", "Amount"] Amount,
   line65_dividendCredits = Field ["Line65", "Amount"] Amount,
   line66_copy = Field ["Line66", "Line40427", "Amount"] Amount,
   line66_fraction = Field ["Line66", "Amount"] Amount,
   line67_sum = subCalculationFields "Line67" ["Amount1"] ["Amount2"],
   line68 = Field ["Line68", "Amount"] Amount,
   line69_copy = Field ["Line69", "Line120", "Amount"] Amount,
   line69_fraction = Field ["Line69", "Amount"] Amount,
   line70 = Field ["Line70", "Amount"] Amount,
   line71_foreignCredit = Field ["Line71", "Amount"] Amount,
   line72 = Field ["Line72", "Amount"] Amount}

page3Fields :: Page3 FieldConst
page3Fields = Page3 {
   partC = partCFields,
   line73_basicReduction = Field ["Line73", "Amount"] Amount,
   line74_copy = Field ["Line74", "Amount"] Amount,
   line75_base = Field ["Line75", "Amount_ReadOnly"] $ Constant 24_338 Amount,
   line76_difference = Field ["Line76", "Amount"] Amount,
   line77_rate = Field ["Line77", "PercentAmount_ReadOnly"] $ Constant 0.0356 Percent,
   line78_fraction = subCalculationFields "Line78" ["Amount1"] ["Amount2"],
   line79_difference = subCalculationFields "Line79" ["Amount1"] ["Amount2"],
   line80_difference = Field ["Line80", "Amount"] Amount,
   line81_logging = Field ["Line81", "Amount"] Amount,
   line82_difference = Field ["Line82", "Amount"] Amount,
   line83_political = Field ["Line83", "Amount"] Amount,
   line84_political = Field ["Line84", "Amount"] Amount,
   line85_difference = Field ["Line85", "Amount"] Amount,
   line86_esop20 = Field ["Line86", "Certificatenumber", "Certificat-no"] Textual,
   line_60450_esop20 = Field ["Line86", "Amount"] Amount,
   line87_evcc30 = Field ["Line87", "Certificatenumber", "Certificat-no"] Textual,
   line_60470_evcc30 = Field ["Line87", "Amount"] Amount,
   line88_sum = subCalculationFields "Line88" ["Amount1"] ["Amount2"],
   line89_difference = Field ["Line89", "Amount"] Amount,
   line90_mining = Field ["Line90", "Amount"] Amount,
   line91_tax = Field ["Line91", "Amount"] Amount}
