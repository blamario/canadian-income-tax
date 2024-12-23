{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Tax.Canada.Federal.Schedule8 where

import Data.Fixed (Centi)
import Data.Text (Text)
import Data.Time.Calendar (MonthOfYear)
import Language.Haskell.TH qualified as TH
import Rank2 qualified
import Rank2.TH qualified
import Transformation.Shallow.TH qualified

import Tax.Canada.Shared (SubCalculation(SubCalculation, calculation, result), fixSubCalculation, subCalculationFields)
import Tax.Canada.T1.Types (T1)
import Tax.Canada.T1.Types qualified as T1
import Tax.FDF (Entry (Amount, Constant, Count, Month, Percent), FieldConst (Field), within)
import Tax.Util (fixEq, fractionOf, difference, nonNegativeDifference, totalOf)

data Schedule8 line = Schedule8{
   page2 :: Page2 line,
   page3 :: Page3 line,
   page4 :: Page4 line,
   page5 :: Page5 line,
   page6 :: Page6 line}

data Page2 line = Page2{
   line_50372_stopMonth :: line MonthOfYear,
   line_50374_revokeMonth :: line MonthOfYear,
   lineA_months :: line Word}

data Page3 line = Page3{
   line1_maxPensionableEarnings :: line Centi,
   line_50339_totalPensionableEarnings :: line Centi,
   line3_least :: line Centi,
   line4_maxBasicExemption :: line Centi,
   line5_difference :: line Centi,
   line_50340_totalContributions :: line Centi,
   line7_fraction :: SubCalculation line,
   line8_difference :: line Centi,
   line9_fraction :: SubCalculation line,
   line10_fraction :: SubCalculation line,
   line11_sum :: line Centi,
   line12_copy :: line Centi,
   line13_copy :: line Centi,
   line14_difference :: line Centi}

data Page4 line = Page4{
   part4 :: Part4 line,
   part5 :: Page4Part5 line}

data Part4 line = Part4{
   line1_netSelfEmploymentEarnings :: line Centi,
   line2_additionalEmploymentEarningsOffT4 :: line Centi,
   line3_sum :: line Centi,
   line4_basicExemption :: line Centi,
   line5_difference :: line Centi,
   line6_contributionRate :: line Rational,
   line7_fraction :: line Centi,
   line8_copy :: line Centi,
   line8_fraction :: line Centi,
   line9_difference :: line Centi,
   line10_copy :: line Centi,
   line10_fraction :: line Centi,
   line11_sum :: line Centi}

data Page4Part5 line = Page4Part5{
   line1_netSelfEmploymentEarnings :: line Centi,
   line2_additionalEmploymentEarningsOffT4 :: line Centi,
   line3_additionalEmploymentEarningsOnT4 :: line Centi,
   line4_sum :: line Centi,
   line5_copy :: line Centi,
   line6_copyIfPositive :: line Centi,
   line7_difference :: line Centi,
   line8_copy :: line Centi,
   line8_fraction :: line Centi}

data Page5 line = Page5{
   line9_copy :: line Centi,
   line10_copy :: line Centi,
   line11_difference :: line Centi,
   line12_copy :: line Centi,
   line13_difference :: line Centi,
   line14_least :: line Centi,
   line15_copy :: line Centi,
   line16_copy :: line Centi,
   line17_difference :: line Centi,
   line18_copy :: line Centi,
   line19_copy :: line Centi,
   line20_difference :: SubCalculation line,
   line21_difference :: SubCalculation line,
   line22_difference :: line Centi,
   line23_copy :: line Centi,
   line23_fraction :: line Centi,
   line24_copy :: line Centi,
   line24_double :: line Centi,
   line25_difference :: line Centi,
   line26_abs :: line Centi,
   line27_copy :: line Centi,
   line28_copy :: line Centi,
   line29_difference :: line Centi,
   line30_least :: line Centi,
   line31_copy :: line Centi,
   line32_copy :: line Centi,
   line33_difference :: line Centi,
   line34_least :: line Centi}

data Page6 line = Page6{
   line35_fraction :: SubCalculation line,
   line36_fraction :: SubCalculation line,
   line37_difference :: line Centi,
   line38_copy :: line Centi,
   line39_copy :: line Centi,
   line40_difference :: line Centi,
   line41_copy :: line Centi,
   line42_copy :: line Centi,
   line43_difference :: line Centi,
   line44_copy :: line Centi,
   line45_fraction :: SubCalculation line,
   line46_difference :: line Centi,
   line47_fraction :: SubCalculation line,
   line48_sum :: line Centi,
   line49_copy :: line Centi,
   line50_copy :: line Centi,
   line51_sum :: line Centi,
   line52_copy :: line Centi,
   line53_copy :: line Centi,
   line54_sum :: line Centi}

$(foldMap
   (\t-> concat <$> sequenceA [
       [d|
           deriving instance (Show (line Centi), Show (line MonthOfYear),
                              Show (line Rational), Show (line Word)) => Show ($(TH.conT t) line)
           deriving instance (Eq (line Centi), Eq (line MonthOfYear),
                              Eq (line Rational), Eq (line Word)) => Eq ($(TH.conT t) line)
       |],
       Rank2.TH.deriveAll t,
       Transformation.Shallow.TH.deriveAll t])
   [''Schedule8, ''Page2, ''Page3, ''Page4, ''Page5, ''Page6, ''Part4, ''Page4Part5])

schedule8Fields :: Schedule8 FieldConst
schedule8Fields = within "form1" Rank2.<$> Schedule8{
   page2 = within "Page2" Rank2.<$> Page2{
      line_50372_stopMonth = Field ["Part1-Continued", "Election-Evocation",
                                    "Line50372", "Month_grp", "Stop-Month"] Month,
      line_50374_revokeMonth = Field ["Part1-Continued", "Election-Evocation",
                                      "Line50374", "Month_grp", "Revoke-Month"] Month,
      lineA_months = Field ["Part2", "LineA", "Number"] Count},
   page3 = within "Page3" . within "Part3" Rank2.<$> Page3{
      line1_maxPensionableEarnings = Field ["Line1", "Amount"] Amount,
      line_50339_totalPensionableEarnings = Field ["Line2", "Amount"] Amount,
      line3_least = Field ["Line3", "Amount"] Amount,
      line4_maxBasicExemption = Field ["Line4", "Amount"] Amount,
      line5_difference = Field ["Line5", "Amount"] Amount,
      line_50340_totalContributions = Field ["Line6", "Amount"] Amount,
      line7_fraction = subCalculationFields "Line7" ["Amount1"] ["Amount2"],
      line8_difference = Field ["Line8", "Amount"] Amount,
      line9_fraction = subCalculationFields "Line9" ["Amount1"] ["Amount2"],
      line10_fraction = subCalculationFields "Line10" ["Amount1"] ["Amount2"],
      line11_sum = Field ["Line11", "Amount"] Amount,
      line12_copy = Field ["Line12", "Amount"] Amount,
      line13_copy = Field ["Line13", "Amount"] Amount,
      line14_difference = Field ["Line14", "Amount"] Amount},
   page4 = within "Page4" Rank2.<$> Page4{
      part4 = within "Part4" Rank2.<$> Part4{
         line1_netSelfEmploymentEarnings = Field ["Line1", "Amount"] Amount,
         line2_additionalEmploymentEarningsOffT4 = Field ["Line2", "Amount"] Amount,
         line3_sum = Field ["Line3", "Amount"] Amount,
         line4_basicExemption = Field ["Line4", "Amount"] Amount,
         line5_difference = Field ["Line5", "Amount"] Amount,
         line6_contributionRate = Field ["Line6", "Percent"] $ Constant 0.119 Percent,
         line7_fraction = Field ["Line7", "Amount"] Amount,
         line8_copy = Field ["Line8", "Amount1"] Amount,
         line8_fraction = Field ["Line8", "Amount2"] Amount,
         line9_difference = Field ["Line9", "Amount"] Amount,
         line10_copy = Field ["Line10", "Amount1"] Amount,
         line10_fraction = Field ["Line10", "Amount2"] Amount,
         line11_sum = Field ["Line11", "amount"] Amount},
      part5 = within "part5" Rank2.<$> Page4Part5{
         line1_netSelfEmploymentEarnings = Field ["Line1", "Amount"] Amount,
         line2_additionalEmploymentEarningsOffT4 = Field ["Line2", "Amount"] Amount,
         line3_additionalEmploymentEarningsOnT4 = Field ["Line3", "Amount"] Amount,
         line4_sum = Field ["Line4", "Amount"] Amount,
         line5_copy = Field ["Line5", "Amount"] Amount,
         line6_copyIfPositive = Field ["Line6", "Amount"] Amount,
         line7_difference = Field ["Line7", "Amount"] Amount,
         line8_copy = Field ["Line8", "Amount1"] Amount,
         line8_fraction = Field ["Line8", "Amount2"] Amount}},
      page5 = within "Page5" . within "Part5_continued" Rank2.<$> Page5{
         line9_copy = Field ["Line9", "Amount"] Amount,
         line10_copy = Field ["Line10", "Amount"] Amount,
         line11_difference = Field ["Line11", "Amount"] Amount,
         line12_copy = Field ["Line12", "Amount"] Amount,
         line13_difference = Field ["Line13", "Amount"] Amount,
         line14_least = Field ["Line14", "Amount"] Amount,
         line15_copy = Field ["Line15", "Amount"] Amount,
         line16_copy = Field ["Line16", "Amount"] Amount,
         line17_difference = Field ["Line17", "Amount"] Amount,
         line18_copy = Field ["Line18", "Amount"] Amount,
         line19_copy = Field ["Line19", "Amount"] Amount,
         line20_difference = subCalculationFields "Line20" ["Amount1"] ["Amount2"],
         line21_difference = subCalculationFields "Line21" ["Amount1"] ["Amount2"],
         line22_difference = Field ["Line22", "Amount"] Amount,
         line23_copy = Field ["Line23", "Amount1"] Amount,
         line23_fraction = Field ["Line23", "Amount2"] Amount,
         line24_copy = Field ["Line24", "Amount1"] Amount,
         line24_double = Field ["Line24", "Amount2"] Amount,
         line25_difference = Field ["Line25", "Amount"] Amount,
         line26_abs = Field ["Line26", "Amount"] Amount,
         line27_copy = Field ["Line27", "Amount"] Amount,
         line28_copy = Field ["Line28", "Amount"] Amount,
         line29_difference = Field ["Line29", "Amount"] Amount,
         line30_least = Field ["Line30", "Amount"] Amount,
         line31_copy = Field ["Line31", "Amount"] Amount,
         line32_copy = Field ["Line32", "Amount"] Amount,
         line33_difference = Field ["Line33", "Amount"] Amount,
         line34_least = Field ["Line34", "Amount"] Amount},
      page6 = within "Page6" . within "Part5" Rank2.<$> Page6{
         line35_fraction = subCalculationFields "Line35" ["Amount1"] ["Amount2"],
         line36_fraction = subCalculationFields "Line36" ["Amount1"] ["Amount2"],
         line37_difference = Field ["Line37", "Amount"] Amount,
         line38_copy = Field ["Line38", "Amount"] Amount,
         line39_copy = Field ["Line39", "Amount"] Amount,
         line40_difference = Field ["Line40", "Amount"] Amount,
         line41_copy = Field ["Line41", "Amount"] Amount,
         line42_copy = Field ["Line42", "Amount"] Amount,
         line43_difference = Field ["Line43", "Amount"] Amount,
         line44_copy = Field ["Line44", "Amount"] Amount,
         line45_fraction = subCalculationFields "Line45" ["Amount1"] ["Amount2"],
         line46_difference = Field ["Line46", "Amount"] Amount,
         line47_fraction = subCalculationFields "Line47" ["Amount1"] ["Amount2"],
         line48_sum = Field ["Line48", "Amount"] Amount,
         line49_copy = Field ["Line49", "Amount"] Amount,
         line50_copy = Field ["Line50", "Amount"] Amount,
         line51_sum = Field ["Line51", "Amount"] Amount,
         line52_copy = Field ["Line52", "Amount"] Amount,
         line53_copy = Field ["Line53", "Amount"] Amount,
         line54_sum = Field ["Line54", "Amount"] Amount}
   }
