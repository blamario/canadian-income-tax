{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.Province.BC.BC479.FieldNames (bc479Fields) where

import Rank2 qualified

import Tax.Canada.Province.BC.BC479.Types
import Tax.Canada.Shared (subCalculationFields)
import Tax.FDF (Entry (Amount, Checkbox, Constant, Count, Percent, Textual), FieldConst (Field), within)

bc479Fields :: BC479 FieldConst
bc479Fields = within "form1" Rank2.<$> BC479 {
   page1 = within "Page1" Rank2.<$> page1Fields,
   page2 = within "Page2" Rank2.<$> page2Fields,
   page3 = within "Page3" Rank2.<$> page3Fields}

page1Fields :: Page1 FieldConst
page1Fields = Page1 {
   line1_netIncome_self = Field ["Chart", "Line1", "NumWithoutCurrency"] Amount,
   line1_netIncome_spouse = Field ["Chart", "Line1", "NumWithoutCurrency2"] Amount,
   line2_uccb_rdsp_repayment_self = Field ["Chart", "Line2", "Col1_Amount"] Amount,
   line2_uccb_rdsp_repayment_spouse = Field ["Chart", "Line2", "Col2_Amount"] Amount,
   line3_sum_self = Field ["Chart", "Line3", "Col1_Amount"] Amount,
   line3_sum_spouse = Field ["Chart", "Line3", "Col2_Amount"] Amount,
   line4_uccb_rdsp_income_self = Field ["Chart", "Line4", "Col1_Amount"] Amount,
   line4_uccb_rdsp_income_spouse = Field ["Chart", "Line4", "Col2_Amount"] Amount,
   line5_difference_self = Field ["Chart", "Line5", "Col1_Amount"] Amount,
   line5_difference_spouse = Field ["Chart", "Line5", "Col2_Amount"] Amount,
   line6_sum = Field ["Chart", "Line6", "Amount"] Amount,
   line7_threshold = Field ["Line7", "Amount"] Amount,
   line8_difference = Field ["Line8", "Amount"] Amount,
   line_60330_sales = Field ["Line9", "Amount"] Amount,
   line_60350_spouse = Field ["Line10", "Amount"] Amount,
   line11_sum = Field ["Line11", "Amount"] Amount,
   line12_fraction = subCalculationFields "Line12" ["Amount1"] ["Amount2"],
   line13_difference = Field ["Line13", "Amount"] Amount,
   line_60890_separate = Field ["BCSHRTC", "Line60890", "CheckBox"] Checkbox,
   line_60480_renovation = Field ["BCSHRTC", "Line14", "Line60480", "Amount"] Amount,
   line14_fraction = Field ["BCSHRTC", "Line14", "Amount"] Amount,
   line15_sum = Field ["BCSHRTC", "Line15", "Amount"] Amount}

page2Fields :: Page2 FieldConst
page2Fields = Page2 {
   line16_copy = Field ["Line16", "Amount"] Amount,
   line17_venture = Field ["BCVCTC", "Line17", "Amount"] Amount,
   line_60490_shares = Field ["BCVCTC", "Line18", "Amount"] Amount,
   line_60491_certificate = Field ["BCVCTC", "Line19", "Account_Number_Comb_EN", "Account_Number"] Textual,
   line_60495_shares = Field ["BCVCTC", "Line20", "Amount"] Amount,
   line_60496_certificate = Field ["BCVCTC", "Line21", "Account_Number"] Textual,
   line22_sum = subCalculationFields "BCVCTC" ["Line22", "I1", "Amount1"] ["Line22", "I2", "Amount2"],
   line_60510_fromT88 = Field ["BCMETC", "Line23", "Amount"] Amount,
   line_60530_fromT88 = Field ["BCMETC", "Line24", "Amount"] Amount,
   line_60545_buildings = Field ["BCCBTC", "Line25", "I1", "Amount1"] Amount,
   line_60546_partnership = Field ["BCCBTC", "Line26", "I1", "Amount1"] Amount,
   line27_sum = subCalculationFields "BCCBTC" ["Line27", "I1", "Amount1"] ["Line27", "I2", "Amount2"],
   line_60550_training = Field ["BCTTC", "Line28", "Amount"] Amount,
   line_60560_training = Field ["BCTTC", "Line29", "Amount"] Amount,
   line_60570_ships = Field ["BCTTC", "Line30", "Amount"] Amount,
   line31_sum = subCalculationFields "BCTTC" ["Line31", "I1", "Amount1"] ["Line31", "I2", "Amount2"],
   line32_credits = Field ["BCTTC", "Line32", "Amount"] Amount}

page3Fields :: Page3 FieldConst
page3Fields = Page3 {
   line33_copy = Field ["Line33", "Amount"] Amount,
   tenancy_months1 = Field ["Declaration-For-Renters", "Table", "Row1", "Numberofmonths"] Count,
   tenancy_months2 = Field ["Declaration-For-Renters", "Table", "Row2", "Numberofmonths"] Count,
   rent_paid1 = Field ["Declaration-For-Renters", "Table", "Row1", "Rentpaid"] Amount,
   rent_paid2 = Field ["Declaration-For-Renters", "Table", "Row2", "Rentpaid"] Amount,
   line_60575_sum = Field ["Line34", "Amount"] Count,
   line35_ceiling = Field ["Line35", "Amount_ReadOnly"] $ Constant 400 Amount,
   line36_income_copy = Field ["Line36", "Amount"] Amount,
   line37_threshold = Field ["Line37", "AmountRead_Only"] $ Constant 63_000 Amount,
   line38_difference = Field ["Line38", "Amount"] Amount,
   line39_rate = Field ["Line39", "PercentAmount_ReadOnly"] $ Constant 0.02 Percent,
   line40_fraction = subCalculationFields "Line40" ["L1", "Amount"] ["L2", "Amount2"],
   line_60576_difference = subCalculationFields "Line41" ["I1", "Amount1"] ["I2", "Amount2"],
   line42_credits = Field ["Line42", "Amount"] Amount}
