{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Tax.Canada.ON428.FieldNames where

import Data.Fixed (Centi)
import Rank2 qualified

import Tax.Canada.ON428.Types
import Tax.FDF (Entry (Count, Amount, Percent), FieldConst (Field), within)

on428Fields = within "form1" Rank2.<$> ON428 {
   page1 = within "Page1" Rank2.<$> page1Fields,
   page2 = within "Page2" Rank2.<$> page2Fields,
   page3 = within "Page3" Rank2.<$> page3Fields,
   page4 = within "Page4" Rank2.<$> page4Fields}


page1Fields = Page1 {
   line1 = Field ["Line1", "Amount"] Amount,
   partA = within "Chart" Rank2.<$> page1PartAFields,
   partB = within "Part_B" Rank2.<$> page1PartBFields}

page1PartAFields = Page1PartA {
   column1 = within "Column1" Rank2.<$> taxIncomeBracketFields,
   column2 = within "Column2" Rank2.<$> taxIncomeBracketFields,
   column3 = within "Column3" Rank2.<$> taxIncomeBracketFields,
   column4 = within "Column4" Rank2.<$> taxIncomeBracketFields,
   column5 = within "Column5" Rank2.<$> taxIncomeBracketFields}

taxIncomeBracketFields = TaxIncomeBracket {
   line2_income = Field ["Line2", "Amount"] Amount,
   line3_threshold = Field ["Line3", "Amount"] Amount,
   line4_overThreshold = Field ["Line4", "Amount"] Amount,
   line5_rate = Field ["Line5", "Percent"] Percent,
   line6_timesRate = Field ["Line6", "Amount"] Amount,
   line7_baseTax = Field ["Line7", "Amount"] Amount,
   line8_equalsTax = Field ["Line8", "Amount"] Amount}

page1PartBFields = Page1PartB {
   line9_basic = Field ["Line9", "Amount"] Amount,
   line10_age = Field ["Line10", "Amount"] Amount,
   line11_base = Field ["Spouse-Amount", "Line11", "Amount"] Amount,
   line12_spouseIncome = Field ["Spouse-Amount", "Line12", "Amount"] Amount,
   line13_difference = Field ["Spouse-Amount", "Line13", "Amount1"] Amount,
   line13_cont = Field ["Spouse-Amount", "Line13", "Amount2"] Amount,
   line14_base = Field ["Eligible-Dependant", "Line14", "Amount"] Amount,
   line15_dependentIncome = Field ["Eligible-Dependant", "Line15", "Amount"] Amount,
   line16_difference = Field ["Eligible-Dependant", "Line16", "Amount1"] Amount,
   line16_cont = Field ["Eligible-Dependant", "Line16", "Amount2"] Amount,
   line17_caregiver = Field ["Line17", "Amount"] Amount,
   line18 = Field ["Line18", "Amount"] Amount,
   line19_cppQpp = Field ["CPP-QPP", "Line19", "Amount"] Amount,
   line20_cppQpp = Field ["CPP-QPP", "Line20", "Amount"] Amount,
   line21_employmentInsurance = Field ["Employment-Insurance", "Line21", "Amount"] Amount,
   line22_employmentInsurance = Field ["Employment-Insurance", "Line22", "Amount"] Amount,
   line23_adoption = Field ["Line23", "Amount"] Amount,
   line24_sum = Field ["Line24", "Amount1"] Amount,
   line24_cont = Field ["Line24", "Amount2"] Amount,
   line25 = Field ["Line25", "Amount"] Amount}

page2Fields = Page2 {
  partB = page2PartBFields,
  partC = page2PartCFields}

page2PartBFields = Page2PartB {
   line26 = Field ["Line26", "Amount"] Amount,
   line27_pension = Field ["Line27", "Amount"] Amount,
   line28 = Field ["Line28", "Amount"] Amount,
   line29_disability = Field ["Line29", "Amount"] Amount,
   line30 = Field ["Line30", "Amount"] Amount,
   line31 = Field ["Line31", "Amount"] Amount,
   line32_interest = Field ["Line32", "Amount"] Amount,
   line33_education = Field ["Line33", "Amount"] Amount,
   line34_transferred = Field ["Line34", "Amount"] Amount,
   line35 = Field ["Line35", "Amount"] Amount,
   medicalExpenses = within "Medical-Expenses" Rank2.<$> medicalExpensesFields,
   line42 = Field ["Line42", "Amount"] Amount,
   line43_sum = Field ["Line43", "Amount1"] Amount,
   line43_cont = Field ["Line43", "Amount2"] Amount,
   line44 = Field ["Line44", "Amount"] Amount,
   line45_rate = Field ["Line45", "Percent_ReadOnly"] Percent,
   line46_fraction = Field ["Line46", "Amount"] Amount,
   donations = within "Donations" Rank2.<$> donationsFields,
   line50 = Field ["Line50", "Amount"] Amount}

medicalExpensesFields = MedicalExpenses {
   line36_expenses = Field ["Line36", "Amount"] Amount,
   line37_income = Field ["Line37", "Amount"] Amount,
   line38_rate = Field ["Line38", "Percent_ReadOnly"] Percent,
   line39_fraction = Field ["Line39", "Amount"] Amount,
   line40_lesser = Field ["Line40", "Amount"] Amount,
   line41_difference = Field ["Line41", "Amount"] Amount}

donationsFields = Donations {
   line47_base = Field ["Line47", "Amount1"] Amount,
   line47_fraction = Field ["Line47", "Amount2"] Amount,
   line48_base = Field ["Line48", "Amount1"] Amount,
   line48_fraction = Field ["Line48", "Amount2"] Amount,
   line49_sum = Field ["Line49", "Amount1"] Amount,
   line49_cont = Field ["Line49", "Amount2"] Amount}

page2PartCFields = Page2PartC {
   line51_tax = Field ["Line51", "Amount"] Amount,
   line52_credits = Field ["Line52", "Amount"] Amount,
   line53 = Field ["Line53", "Amount"] Amount,
   line54 = Field ["Line54", "Amount"] Amount,
   line55 = Field ["Line55", "Amount"] Amount,
   line56 = Field ["Min-Tax-Carryover", "Line56", "Amount"] Amount,
   line57 = Field ["Min-Tax-Carryover", "Line57", "Amount"] Amount,
   line58 = Field ["Min-Tax-Carryover", "Line58", "Amount"] Amount,
   line59_copy = Field ["Min-Tax-Carryover", "Line59", "Amount1"] Amount,
   line59_product = Field ["Min-Tax-Carryover", "Line59", "Amount2"] Amount,
   line60_lesser = Field ["Min-Tax-Carryover", "Line60", "Amount"] Amount,
   line61 = Field ["Line61", "Amount"] Amount}

page3Fields = Page3 {
   line62 = Field ["Line62", "Amount"] Amount,
   line63 = Field ["ON-Surtax", "Line63", "Amount"] Amount,
   line64 = Field ["ON-Surtax", "Line64", "Amount"] Amount,
   line65 = Field ["ON-Surtax", "Line65", "Amount"] Amount,
   line66_copy = Field ["Line66", "Amount1"] Amount,
   line66_surtax = Field ["Line66", "Amount2"] Amount,
   line67_copy = Field ["Line67", "Amount1"] Amount,
   line67_surtax = Field ["Line67", "Amount2"] Amount,
   line68_sum = Field ["Line68", "Amount1"] Amount,
   line68_cont = Field ["Line68", "Amount2"] Amount,
   line69 = Field ["Line69", "Amount2"] Amount,
   line70 = Field ["Line70", "Amount"] Amount,
   line71 = Field ["Line71", "Amount"] Amount,
   line72 = Field ["Line72", "Amount"] Amount,
   line73 = Field ["Line73", "Amount"] Amount,
   line74_basicReduction = Field ["Line74", "Amount"] Amount,
   line75_childrenNum = Field ["Line_60969", "Line75", "Number"] Count,
   line75_amount = Field ["Line75", "Amount"] Amount,
   line76_childrenNum = Field ["Line76", "Line_60970", "Number"] Count,
   line76_amount = Field ["Line76", "Amount"] Amount,
   line77 = Field ["Line77", "Amount"] Amount,
   line78_copy = Field ["Line78", "Amount1"] Amount,
   line78_product = Field ["Line78", "Amount2"] Amount,
   line79 = Field ["Line79", "Amount"] Amount,
   line80_difference = Field ["Line80", "Amount1"] Amount,
   line80_cont = Field ["Line80", "Amount2"] Amount,
   line81 = Field ["Line81", "Amount"] Amount,
   line82 = Field ["Line82", "Amount"] Amount,
   line83 = Field ["Line83", "Amount"] Amount}

page4Fields = Page4 {
   line84 = Field ["Line84", "Amount"] Amount,
   line85_lift = Field ["Line85", "Amount"] Amount,
   line86 = Field ["Line86", "Amount"] Amount,
   line87_foodDonations = Field ["Line62150", "Line87", "Amount"] Amount,
   line87_fraction = Field ["Line87", "Amount"] Amount,
   line88 = Field ["Line88", "Amount"] Amount,
   line89_health = Field ["Line89", "Amount"] Amount,
   line90 = Field ["Line90", "Amount"] Amount,
   healthPremium = within "ON_Health_Prenium-worksheet" . within "Chart_ON_Health_Prenium" Rank2.<$> healthPremiumFields}

healthPremiumFields = HealthPremium {
   row1 = within "Taxable_Line2" Rank2.<$> healthPremiumBracketFields,
   row2 = within "Taxable_Line4" Rank2.<$> healthPremiumBracketFields,
   row3 = within "Taxable_Line6" Rank2.<$> healthPremiumBracketFields,
   row4 = within "Taxable_Line8" Rank2.<$> healthPremiumBracketFields,
   row5 = within "Taxable_Line10" Rank2.<$> healthPremiumBracketFields}

healthPremiumBracketFields = HealthPremiumBracket {
   taxableIncome = Field ["Amount1"] Amount,
   overThreshold = Field ["Amount2"] Amount,
   timesRate     = Field ["Amount3"] Amount,
   equalsTax     = Field ["Amount4"] Amount}
