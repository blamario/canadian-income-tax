{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.Province.BC.BC479.FieldNames (bc479Fields) where

import Data.Fixed (Centi)
import Rank2 qualified

import Tax.Canada.Province.BC.BC479.Types
import Tax.FDF (Entry (Amount, Checkbox, Textual), FieldConst (Field), within)

bc479Fields = within "form1" Rank2.<$> BC479 {
   page1 = within "Page1" Rank2.<$> page1Fields,
   page2 = within "Page2" Rank2.<$> page2Fields}

page1Fields = Page1 {
   line1_netIncome_self = Field ["Chart", "Column1", "Line1", "Col1_Amount"] Amount,
   line1_netIncome_spouse = Field ["Chart", "Column2", "Line1", "Col2_Amount"] Amount,
   line2_uccb_rdsp_repayment_self = Field ["Chart", "Column1", "Line2", "Col1_Amount"] Amount,
   line2_uccb_rdsp_repayment_spouse = Field ["Chart", "Column2", "Line2", "Col2_Amount"] Amount,
   line3_sum_self = Field ["Chart", "Column1", "Line3", "Col1_Amount"] Amount,
   line3_sum_spouse = Field ["Chart", "Column2", "Line3", "Col2_Amount"] Amount,
   line4_uccb_rdsp_income_self = Field ["Chart", "Column1", "Line4", "Col1_Amount"] Amount,
   line4_uccb_rdsp_income_spouse = Field ["Chart", "Column2", "Line4", "Col2_Amount"] Amount,
   line5_difference_self = Field ["Chart", "Column1", "Line5", "Col1_Amount"] Amount,
   line5_difference_spouse = Field ["Chart", "Column2", "Line5", "Col2_Amount"] Amount,
   line6_sum = Field ["Chart", "Line6", "Amount"] Amount,
   line7_threshold = Field ["Line7", "Amount"] Amount,
   line8_difference = Field ["Line8", "Amount"] Amount,
   line_60330_sales = Field ["Line9", "Amount"] Amount,
   line_60350_spouse = Field ["Line10", "Amount"] Amount,
   line11_sum = Field ["Line11", "Amount"] Amount,
   line12_copy = Field ["Line12", "Amount1"] Amount,
   line12_fraction = Field ["Line12", "Amount2"] Amount,
   line13_difference = Field ["Line13", "Amount"] Amount,
   line_60890_separate = Field ["BCSHRTC", "Line60890", "CheckBox"] Checkbox,
   line_60480_renovation = Field ["BCSHRTC", "Line14", "Line60480", "Amount"] Amount,
   line14_fraction = Field ["BCSHRTC", "Line14", "Amount"] Amount,
   line15_sum = Field ["BCSHRTC", "Line15", "Amount"] Amount}

page2Fields = Page2 {
   line16_copy = Field ["Line16", "Amount"] Amount,
   line17_venture = Field ["BCVCTC", "Line17", "Amount"] Amount,
   line_60490_shares = Field ["BCVCTC", "Line18", "Amount"] Amount,
   line_60491_certificate = Field ["BCVCTC", "Line19", "Account_Number_Comb_EN", "Account_Number"] Textual,
   line_60495_shares = Field ["BCVCTC", "Line20", "Amount"] Amount,
   line_60496_certificate = Field ["BCVCTC", "Line21", "Account_Number"] Textual,
   line22_sum = Field ["BCVCTC", "Line22", "I1", "Amount1"] Amount,
   line22_cont = Field ["BCVCTC", "Line22", "I2", "Amount2"] Amount,
   line_60510_fromT88 = Field ["BCMETC", "Line23", "Amount"] Amount,
   line_60530_fromT88 = Field ["BCMETC", "Line24", "Amount"] Amount,
   line_60550_training = Field ["BCTTC", "Line25", "Amount"] Amount,
   line_60560_training = Field ["BCTTC", "Line26", "Amount"] Amount,
   line_60570_ships = Field ["BCTTC", "Line27", "Amount"] Amount,
   line28_sum = Field ["BCTTC", "Line28", "I1", "Amount1"] Amount,
   line28_cont = Field ["BCTTC", "Line28", "I2", "Amount2"] Amount,
   line29_credits = Field ["BCTTC", "Line29", "Amount"] Amount}
