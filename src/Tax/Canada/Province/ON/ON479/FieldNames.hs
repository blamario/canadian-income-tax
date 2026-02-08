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
   line_61268_fertility = Field ["Line1", "Line61268", "Amount1"] Amount,
   line_61268_fraction = Field ["Line1", "Amount2"] Amount,
   line_63050_childcare = Field ["Line2", "Amount"] Amount,
   line4_homecare_copy = Field ["Line3", "Amount"] Amount,
   line5_allowable = Field ["Line4", "Percent_ReadOnly"] $ Constant 0.25 Percent,
   line6_fraction = Field ["Line5", "Amount"] Amount,
   line7_netIncome_copy = Field ["Line6", "Amount"] Amount,
   line8_spouse_copy = Field ["Line7", "Amount"] Amount,
   line9_sum = Field ["Line8", "Amount1"] Amount,
   line10_base = Field ["Line9", "Amount"] $ Constant 35_000 Amount,
   line11_difference = Field ["Line10", "Amount"] Amount,
   line12_rate = Field ["Line11", "Percent_ReadOnly"] $ Constant 0.05 Percent,
   line13_fraction = subCalculationFields "Line12" ["Amount1"] ["Amount2"],
   line_63095_difference = subCalculationFields "Line13" ["Amount1"] ["Amount2"],
   line_63100_transit = Field ["Line14", "Line63100", "Amount"] Amount,
   line_63100_fraction = Field ["Line14", "Amount"] Amount,
   line_63110_contributions = Field ["Line15", "Amount"] Amount,
   line_63110_credit = Field ["Line16", "Amount"] Amount,
   line_63220_fromT1221 = Field ["Line17", "Line63220", "Amount1"] Amount,
   line_63220_fraction = Field ["Line17", "Amount2"] Amount,
   line18_sum = Field ["Line18", "Amount"] Amount}

page2Fields :: Page2 FieldConst
page2Fields = Page2 {
   line19_copy = Field ["Line19", "Amount"] Amount,
   line_63260_placements = Field ["Line63260", "NmbrApprentices"] Count,
--   line_63265_partnership = Field ["Line63265", "RadioButtonGroup"] $ RadioButton [True, False],
   line_63270_business = Field ["Line63270", "BusinessNumber_9_Comb_Bottom_Adv", "BusinessNumber"] Textual,
   line_63300_total = Field ["Line20", "Amount"] Amount,
   line23_credits = Field ["Line21", "Amount"] Amount}
