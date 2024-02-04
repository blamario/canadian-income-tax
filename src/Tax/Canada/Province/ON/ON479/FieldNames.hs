{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.Province.ON.ON479.FieldNames (on479Fields) where

import Data.Fixed (Centi)
import Rank2 qualified

import Tax.Canada.Province.ON.ON479.Types
import Tax.Canada.Shared (SubCalculation (SubCalculation, calculation, result))
import Tax.FDF (Entry (Count, Constant, Amount, Percent, RadioButton, Textual), FieldConst (Field), within)

on479Fields = within "form1" Rank2.<$> ON479 {
   page1 = within "Page1" Rank2.<$> page1Fields,
   page2 = within "Page2" Rank2.<$> page2Fields}


page1Fields = Page1 {
   line_63050_childcare = Field ["Line1", "Amount"] Amount,
   line_63051_staycationBox = Field ["Line_63051", "RadioButtonGroup"] $ RadioButton [True, False],
   line_63052_staycationCost = Field ["Line2", "Line63052", "Amount1"] Amount,
   line_63052_fraction = Field ["Line2", "Amount2"] Amount,
   line_63055_copy = Field ["Line63055_Line3", "Amount1"] Amount,
   line_63055_fraction = Field ["Line63055_Line3", "Amount2"] Amount,
   line4_homecare_copy = Field ["Line4", "Amount"] Amount,
   line5_allowable = Field ["Line5", "Percent_ReadOnly"] $ Constant 0.25 Percent,
   line6_fraction = Field ["Line6", "Amount"] Amount,
   line7_netIncome_copy = Field ["Line7", "Amount"] Amount,
   line8_spouse_copy = Field ["Line8", "Amount"] Amount,
   line9_sum = Field ["Line9", "Amount1"] Amount,
   line10_base = Field ["Line10", "Amount"] Amount,
   line11_difference = Field ["Line11", "Amount"] Amount,
   line12_rate = Field ["Line12", "Percent_ReadOnly"] $ Constant 0.05 Percent,
   line13_fraction = SubCalculation{calculation = Field ["Line13", "Amount1"] Amount,
                                    result = Field ["Line13", "Amount2"] Amount},
   line_63095_difference = SubCalculation{calculation = Field ["Line63095_Line14", "Amount1"] Amount,
                                          result = Field ["Line63095_Line14", "Amount2"] Amount},
   line_63100_transit = Field ["Line15", "Line63100", "Amount1"] Amount,
   line_63100_fraction = Field ["Line15", "Amount2"] Amount,
   line16_sum = Field ["Line17", "Amount"] Amount}

page2Fields = Page2 {
   line17_copy = Field ["Line17", "Amount"] Amount,
   line_63105_renovation = Field ["Line18", "Line63105", "Amount1"] Amount,
   line_63105_fraction = Field ["Line18", "Amount2"] Amount,
   line_63110_contributions = Field ["Line19", "Amount"] Amount,
   line_63110_credit = Field ["Line20", "Amount"] Amount,
   line_63220_fromT1221 = Field ["Line21", "Line63220", "Amount1"] Amount,
   line_63220_fraction = Field ["Line21", "Amount2"] Amount,
   line_63260_placements = Field ["Line63260", "NmbrApprentices"] Count,
   line_63265_partnership = Field ["Line63265", "RadioButtonGroup"] $ RadioButton [True, False],
   line_63270_business = Field ["Line63270", "BusinessNumber_9_Comb_Bottom_Adv", "BusinessNumber"] Textual,
   line_63300_total = Field ["Line22", "Amount"] Amount,
   line23_credits = Field ["Line23", "Amount"] Amount}
