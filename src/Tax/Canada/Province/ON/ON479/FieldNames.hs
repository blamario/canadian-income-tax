{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.Province.ON.ON479.FieldNames (on479Fields) where

import Rank2 qualified

import Tax.Canada.Province.ON.ON479.Types
import Tax.Canada.Shared (subCalculationFields)
import Tax.FDF (Entry (Count, Constant, Amount, Percent, RadioButton, Textual), FieldConst (Field), within)

on479Fields :: ON479 FieldConst
on479Fields = within "form1" Rank2.<$> ON479 {
   page1 = within "Page1" Rank2.<$> page1Fields,
   page2 = within "Page2" Rank2.<$> page2Fields}


page1Fields :: Page1 FieldConst
page1Fields = Page1 {
   line_63050_childcare = Field ["Line1", "Amount"] Amount,
   line4_homecare_copy = Field ["Line2", "Amount"] Amount,
   line5_allowable = Field ["Line3", "Percent_ReadOnly"] $ Constant 0.25 Percent,
   line6_fraction = Field ["Line4", "Amount"] Amount,
   line7_netIncome_copy = Field ["Line5", "Amount"] Amount,
   line8_spouse_copy = Field ["Line6", "Amount"] Amount,
   line9_sum = Field ["Line7", "Amount1"] Amount,
   line10_base = Field ["Line8", "Amount"] $ Constant 35_000 Amount,
   line11_difference = Field ["Line9", "Amount"] Amount,
   line12_rate = Field ["Line10", "Percent_ReadOnly"] $ Constant 0.05 Percent,
   line13_fraction = subCalculationFields "Line11" ["Amount1"] ["Amount2"],
   line_63095_difference = subCalculationFields "Line12" ["Amount1"] ["Amount2"],
   line_63100_transit = Field ["Line13", "Line63100", "Amount"] Amount,
   line_63100_fraction = Field ["Line13", "Amount"] Amount,
   line16_sum = Field ["Line14", "Amount"] Amount}

page2Fields :: Page2 FieldConst
page2Fields = Page2 {
   line17_copy = Field ["Line15", "Amount"] Amount,
   line_63110_contributions = Field ["ON-Political-Contribution", "Line16", "Amount"] Amount,
   line_63110_credit = Field ["ON-Political-Contribution", "Line17", "Amount"] Amount,
   line_63220_fromT1221 = Field ["Line18", "Line63220", "Amount1"] Amount,
   line_63220_fraction = Field ["Line18", "Amount2"] Amount,
   line_63260_placements = Field ["ON-Coop-Education", "Line63260", "NmbrApprentices"] Count,
   line_63265_partnership = Field ["ON-Coop-Education", "Line63265", "RadioButtonGroup"] $ RadioButton [True, False],
   line_63270_business = Field ["ON-Coop-Education", "Line63270", "BusinessNumber_9_Comb_Bottom_Adv", "BusinessNumber"] Textual,
   line_63300_total = Field ["Line19", "Amount"] Amount,
   line23_credits = Field ["Line20", "Amount"] Amount}
