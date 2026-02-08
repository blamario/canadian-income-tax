{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.Province.AB.AB428.FieldNames (ab428Fields) where

import Data.Fixed (Centi)
import Data.Text (Text)
import Rank2 qualified

import Tax.Canada.Province.AB.AB428.Types
import Tax.Canada.Shared (BaseCredit(..), MedicalExpenses(..), TaxIncomeBracket (..), subCalculationFields)
import Tax.FDF (Entry (Constant, Amount, Percent), FieldConst (Field), within)

ab428Fields :: AB428 FieldConst
ab428Fields = within "form1" Rank2.<$> AB428 {
   page1 = within "Page1" Rank2.<$> page1Fields,
   page2 = within "Page2" Rank2.<$> page2Fields,
   page3 = within "Page3" Rank2.<$> page3Fields}


page1Fields :: Page1 FieldConst
page1Fields = Page1 {
   income = Field ["Line1", "Amount"] Amount,
   partA = page1PartAFields,
   partB = page1PartBFields}

page1PartAFields :: Page1PartA FieldConst
page1PartAFields = Page1PartA {
   column1 = within "Chart1" . within "Column1" Rank2.<$>  taxIncomeBracketFields "1"       0 0.08      0,
   column2 = within "Chart1" . within "Column2" Rank2.<$>  taxIncomeBracketFields "2"  60_000 0.10  4_800,
   column3 = within "Chart1" . within "Column3" Rank2.<$> (taxIncomeBracketFields "3" 151_234 0.12 13_923.40){
                                                             rate = Field ["LIne5-C3", "Percent"] $ Constant 0.12 Percent},
   column4 = within "Chart2" . within "Column4" Rank2.<$> (taxIncomeBracketFields "4" 181_481 0.13 17_553.04){
                                                             rate = Field ["LIne5-C4", "Percent"] $ Constant 0.13 Percent},
   column5 = within "Chart2" . within "Column5" Rank2.<$>  taxIncomeBracketFields "5" 241_974 0.14 25_417.13,
   column6 = within "Chart2" . within "Column6" Rank2.<$>  taxIncomeBracketFields "6" 362_961 0.15 42_355.31}

taxIncomeBracketFields :: Text -> Centi -> Rational -> Centi -> TaxIncomeBracket FieldConst
taxIncomeBracketFields column threshold rate baseTax = TaxIncomeBracket {
   income = Field ["Line2-C" <> column , "Amount"] Amount,
   threshold = Field ["Line3-C" <> column, "Amount"] $ Constant threshold Amount,
   overThreshold = Field ["Line4-C" <> column, "Amount"] Amount,
   rate = Field ["Line5-C" <> column, "Percent"] $ Constant rate Percent,
   timesRate = Field ["Line6-C" <> column, "Amount"] Amount,
   baseTax = Field ["Line7-C" <> column, "Amount"] $ Constant baseTax Amount,
   equalsTax = Field ["Line8-C" <> column, "Amount"] Amount}

page1PartBFields :: Page1PartB FieldConst
page1PartBFields = Page1PartB {
   line9_basic = Field ["Line16", "Amount"] Amount,
   line10_age = Field ["Line17", "Amount"] Amount,
   spouseAmount = within "Spouse-Net-Income" Rank2.<$> BaseCredit{
       baseAmount = Field ["Line18", "Amount"] $ Constant 22_323 Amount,
       reduction = Field ["Line19", "Amount"] Amount,
       difference = Field ["Line20", "Amount1"] Amount,
       cont = Field ["Line20", "Amount2"] Amount},
   dependantAmount = within "Eligible-Dependant" Rank2.<$> BaseCredit{
       baseAmount = Field ["Line21", "Amount"] $ Constant 22_323 Amount,
       reduction = Field ["Line22", "Amount"] Amount,
       difference = Field ["Line23", "Amount1"] Amount,
       cont = Field ["Line23", "Amount2"] Amount},
   line17_infirm = Field ["Line24", "Amount"] Amount,
   line18 = Field ["Line25", "Amount"] Amount}

page2Fields :: Page2 FieldConst
page2Fields = Page2 {
  partB = page2PartBFields}

page2PartBFields :: Page2PartB FieldConst
page2PartBFields = Page2PartB {
   line26_copy = Field ["Line26", "Amount"] Amount,
   line19_cppQpp = Field ["CPP-QPP", "Line27", "Amount"] Amount,
   line20_cppQpp = Field ["CPP-QPP", "Line28", "Amount"] Amount,
   line21_employmentInsurance = Field ["EI", "Line29", "Amount"] Amount,
   line22_employmentInsurance = Field ["EI", "Line30", "Amount"] Amount,
   line23_adoption = Field ["Line31", "Amount"] Amount,
   line24_sum = subCalculationFields "Line32" ["Amount1"] ["Amount2"],
   line25 = Field ["Line33", "Amount"] Amount,
   line27_pension = Field ["Line34", "Amount"] Amount,
   line28_caregiver = Field ["Line35", "Amount"] Amount,
   line29 = Field ["Line36", "Amount"] Amount,
   line30_disability = Field ["Line37", "Amount"] Amount,
   line31 = Field ["Line38", "Amount"] Amount,
   line32 = Field ["Line39", "Amount"] Amount,
   line33_interest = Field ["Line40", "Amount"] Amount,
   line34_education = Field ["Line41", "Amount"] Amount,
   line35_transferredSpouse = Field ["Line42", "Amount"] Amount,
   line36 = Field ["Line43", "Amount"] Amount,
   medicalExpenses = within "Medical-Expenses" Rank2.<$> medicalExpensesFields,
   line43 = Field ["Line50", "Amount"] Amount,
   line44_sum = subCalculationFields "Line51" ["Amount1"] ["Amount2"],
   line45 = Field ["Line52", "Amount"] Amount,
   line46_rate = Field ["Line53", "Percent"] $ Constant 0.08 Percent,
   line47_fraction = Field ["Line54", "Amount"] Amount,
   donations = within "Donation-Gift" Rank2.<$> donationFields,
   line50_sum = subCalculationFields "Line57" ["Amount1"] ["Amount2"],
   line51 = Field ["Line58", "Amount"] Amount}

medicalExpensesFields :: MedicalExpenses FieldConst
medicalExpensesFields = MedicalExpenses {
   expenses = Field ["Line44", "Amount"] Amount,
   netIncome = Field ["Line45", "Amount"] Amount,
   incomeRate = Field ["Line46", "Percent"] $ Constant 0.03 Percent,
   fraction = Field ["Line47", "Amount"] Amount,
   lesser = Field ["Line48", "Amount"] Amount,
   difference = Field ["Line49", "Amount"] Amount}

donationFields :: Donations FieldConst
donationFields = Donations {
   line48_base = Field ["Line55", "Amount1"] Amount,
   line48_fraction = Field ["Line55", "Amount2"] Amount,
   line49_base = Field ["Line56", "Amount1"] Amount,
   line49_fraction = Field ["Line56", "Amount2"] Amount}

page3Fields :: Page3 FieldConst
page3Fields = Page3 {
   partC = within "PartC" Rank2.<$> partCFields}

partCFields :: PartC FieldConst
partCFields = PartC {
   line52_tax = Field ["Line59", "Amount"] Amount,
   line53_splitIncomeTax = Field ["Line60", "Amount"] Amount,
   line54 = Field ["Line61", "Amount"] Amount,
   line55_copy = Field ["Line62", "Amount"] Amount,
   line56_dividendCredits = Field ["Line63", "Amount"] Amount,
   line57_copy = Field ["Line64", "Amount1"] Amount,
   line57_fraction = Field ["Line64", "Amount2"] Amount,
   line58_sum = subCalculationFields "Line65" ["Amount1"] ["Amount2"],
   line59_difference = Field ["Line66", "Amount"] Amount,
   line60_fromT691 = Field ["Line67", "Amount1"] Amount,
   line60_fraction = Field ["Line67", "Amount2"] Amount,
   line61 = Field ["Line68", "Amount"] Amount,
   line62_foreignCredit = Field ["Line69", "Amount"] Amount,
   line63_difference = Field ["Line70", "Amount"] Amount,
   line64_political = Field ["Line71", "Amount"] Amount,
   line65_political = Field ["Line72", "Amount"] Amount,
   line73_difference = Field ["Line73", "Amount"] Amount,
   line_61545_supplemental = Field ["Line74", "Amount"] Amount,
   line66_tax = Field ["Line75", "Amount"] Amount}
