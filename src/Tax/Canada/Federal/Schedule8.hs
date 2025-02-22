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

import Control.Applicative ((<|>))
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
   page6 :: Page6 line,
   page7 :: Page7 line,
   page8 :: Page8 line,
   page9 :: Page9 line,
   page10 :: Page10 line}

data Page2 line = Page2{
   line_50372_stopMonth :: line MonthOfYear,
   line_50374_revokeMonth :: line MonthOfYear}

data Page3 line = Page3{
   lineA_months :: line Word,
   lineB_additionalMaxPensionableEarnings :: line Centi,
   lineC_maxPensionableEarnings :: line Centi,
   lineD_maxBasicExemption :: line Centi,
   lineE_maxSubjectToSecondAdditionalContributions :: line Centi}

data Page4 line = Page4{
   line_50339_totalPensionableEarnings :: line Centi,
   line2_least :: SubCalculation line,
   line3_copyC :: line Centi,
   line4_difference :: SubCalculation line,
   line5_difference :: line Centi,
   line6_copyD :: line Centi,
   line7_difference :: line Centi,
   line_50340_totalContributions :: line Centi,
   line9_fraction :: SubCalculation line,
   line10_difference :: line Centi,
   line11_fraction :: SubCalculation line,
   line12_fraction :: SubCalculation line,
   line13_sum :: line Centi,
   line_50341_totalSecondContributions :: line Centi,
   line15_fraction :: SubCalculation line,
   line16_copy :: line Centi,
   line17_copy :: line Centi,
   line18_difference :: line Centi,
   line19_copy :: line Centi,
   line20_copy :: line Centi,
   line21_difference :: SubCalculation line,
   line22_sum :: line Centi,
   line23_copy :: line Centi,
   line24_copy :: line Centi,
   line25_difference :: SubCalculation line,
   line26_sum :: line Centi}

data Page5 line = Page5{
   part3a :: Page5Part3a line,
   part3b :: Page5Part3b line}

data Page5Part3a line = Page5Part3a{
   line27_copy :: line Centi,
   line28_copy :: line Centi,
   line29_copy :: line Centi,
   line30_sum :: line Centi,
   line31_copy :: line Centi}

data Page5Part3b line = Page5Part3b{
   line32_join :: line Centi,
   line33_conditionalCopy :: line Centi,
   line34to37 :: Page5Part3bCond1 line,
   line38_conditionalCopy :: line Centi,
   line39to42 :: Page5Part3bCond2 line,
   line43_sum :: line Centi}

data Page5Part3bCond1 line = Page5Part3bCond1{
   line34_abs :: line Centi,
   line35_copy :: line Centi,
   line36_least :: line Centi,
   line37_sum :: SubCalculation line}

data Page5Part3bCond2 line = Page5Part3bCond2{
   line39_abs :: line Centi,
   line40_copy :: line Centi,
   line41_least :: line Centi,
   line42_sum :: SubCalculation line}

data Page6 line = Page6{
   line1_netSelfEmploymentEarnings :: line Centi,
   line_50373_additionalEmploymentEarningsOffT4 :: line Centi,
   line3_sum :: line Centi,
   line4_least :: SubCalculation line,
   line5_copyC :: line Centi,
   line6_difference :: SubCalculation line,
   line7_difference :: line Centi,
   line8_copyD :: line Centi,
   line9_difference :: line Centi,
   line10_fraction :: SubCalculation line,
   line11_fraction :: SubCalculation line,
   line12_fraction :: SubCalculation line,
   line13_sum :: SubCalculation line,
   line14_sum :: line Centi,
   line15_half :: SubCalculation line,
   line16_copy :: line Centi,
   line17_sum :: line Centi}

data Page7 line = Page7{
   line1_netSelfEmploymentEarnings :: line Centi,
   line_50373_additionalEmploymentEarningsOffT4 :: line Centi,
   line_50399_additionalEmploymentEarningsOnT4 :: line Centi,
   line4_sum :: line Centi,
   line5_copy :: line Centi,
   line6to8 :: Page7Cond1 line,
   line9_difference :: line Centi,
   line10_fraction :: SubCalculation line,
   line11_copy :: line Centi,
   line12to14 :: Page7Cond2 line,
   line15_difference :: line Centi,
   line16_fraction :: SubCalculation line,
   line17_sum :: line Centi,
   line18to20 :: Page7Cond3 line,
   line21_copy :: line Centi,
   line22_copy :: line Centi,
   line23_sum :: line Centi,
   line24_copyC :: line Centi,
   line25_copyD :: line Centi,
   line26_difference :: line Centi,
   line27_copy :: line Centi,
   line28_difference :: line Centi}

data Page7Cond1 line = Page7Cond1{
   line6_copy :: line Centi,
   line7_copy :: line Centi,
   line8_difference :: SubCalculation line}

data Page7Cond2 line = Page7Cond2{
   line12_copy :: line Centi,
   line13_copy :: line Centi,
   line14_difference :: SubCalculation line}

data Page7Cond3 line = Page7Cond3{
   line18_abs :: line Centi,
   line19_least :: line Centi,
   line20_fraction :: SubCalculation line}

data Page8 line = Page8{
   line29_least :: line Centi,
   line30_copy :: line Centi,
   line31_copy :: line Centi,
   line32_difference :: line Centi,
   line33_copy :: line Centi,
   line34_copy :: line Centi,
   line35_difference :: SubCalculation line,
   line36_difference :: SubCalculation line,
   line37_difference :: line Centi,
   line38to48 :: Page8Cond1 line,
   line49_fraction :: SubCalculation line,
   line50_fraction :: SubCalculation line,
   line51_fraction :: SubCalculation line,
   line52_sum :: SubCalculation line,
   line53_sum :: line Centi,
   line54_double :: SubCalculation line,
   line55_difference :: line Centi}

data Page8Cond1 line = Page8Cond1{
   line38_copyE :: line Centi,
   line39_copy :: line Centi,
   line40_copy :: line Centi,
   line41_sum :: SubCalculation line,
   line42_difference :: line Centi,
   line43_copy :: line Centi,
   line44_copy :: line Centi,
   line45_difference :: line Centi,
   line46_copy :: line Centi,
   line47_difference :: line Centi,
   line48_least :: line Centi}

data Page9 line = Page9{
   line56_half :: SubCalculation line,
   line57_copy :: line Centi,
   line58_copy :: line Centi,
   line59_difference :: line Centi,
   line60_least :: line Centi,
   line61_copy :: line Centi,
   line62_copy :: line Centi,
   line63_difference :: line Centi,
   line64_conditionalCopy :: line Centi,
   line65_abs :: line Centi,
   line66_copy :: line Centi,
   line67_least :: line Centi,
   line68_sum :: SubCalculation line,
   line69_copy :: line Centi,
   line70_copy :: line Centi,
   line71_difference :: line Centi,
   line72_conditionalCopy :: line Centi,
   line73_abs :: line Centi,
   line74_copy :: line Centi,
   line75_least :: line Centi,
   line76_sum :: SubCalculation line,
   line77_sum :: line Centi,
   line78_half :: SubCalculation line,
   line79_half :: SubCalculation line,
   line80_sum :: line Centi,
   line81_positiveCopy :: line Centi,
   line82_difference :: line Centi,
   line83_fraction :: SubCalculation line,
   line84_copy :: line Centi,
   line85_difference :: line Centi}

data Page10 line = Page10{
   line86_copy :: line Centi,
   line87_copy :: line Centi,
   line88_copy :: line Centi,
   line89_difference :: SubCalculation line,
   line90_copy :: line Centi,
   line91_copy :: line Centi,
   line92_difference :: SubCalculation line,
   line93_half :: SubCalculation line,
   line94_copy :: line Centi,
   line95_join :: line Centi,
   line96_difference :: SubCalculation line,
   line97_sum :: line Centi}


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
   [''Schedule8, ''Page2, ''Page3, ''Page4,
    ''Page5, ''Page5Part3a, ''Page5Part3b, ''Page5Part3bCond1, ''Page5Part3bCond2, ''Page6,
    ''Page7, ''Page7Cond1, ''Page7Cond2, ''Page7Cond3, ''Page8, ''Page8Cond1, ''Page9, ''Page10])

fixSchedule8 :: Schedule8 Maybe -> Schedule8 Maybe
fixSchedule8 = fixEq $ \Schedule8{page2, page3, page4, page5, page6, page7, page8, page9, page10}-> Schedule8{
   page2 = page2,
   page3 = page3{
      lineA_months = page3.lineA_months <|> Just 12,
      lineB_additionalMaxPensionableEarnings = ((6100 *) . fromIntegral . max 12) <$> page3.lineA_months,
      lineC_maxPensionableEarnings = ((/ 12) . (68_500 *) . fromIntegral . max 12) <$> page3.lineA_months,
      lineD_maxBasicExemption = ((/ 12) . (3500 *) . fromIntegral . min 12) <$> page3.lineA_months,
      lineE_maxSubjectToSecondAdditionalContributions = ((/ 12) . (4700 *) . fromIntegral . min 12) <$> page3.lineA_months},
   page4 = let Page4{..} = page4 in case line_50339_totalPensionableEarnings of
         Nothing -> Rank2.pure Nothing
         Just{} -> page4{
      line2_least = fixSubCalculation id $ min page3.lineB_additionalMaxPensionableEarnings line_50339_totalPensionableEarnings,
      line3_copyC = page3.lineC_maxPensionableEarnings,
      line4_difference = fixSubCalculation id $ nonNegativeDifference line2_least.result line3_copyC,
      line5_difference = nonNegativeDifference line2_least.result line4_difference.result,
      line6_copyD = page3.lineD_maxBasicExemption,
      line7_difference = nonNegativeDifference line5_difference line6_copyD,
      line9_fraction = fixSubCalculation (0.831933 *) line_50340_totalContributions,
      line10_difference = difference line_50340_totalContributions line9_fraction.result,
      line11_fraction = fixSubCalculation (0.0495 *) line7_difference,
      line12_fraction = fixSubCalculation (0.01 *) line7_difference,
      line13_sum = totalOf [line11_fraction.result, line12_fraction.result],
      line15_fraction = fixSubCalculation (0.04 *) line4_difference.result,
      line16_copy = line9_fraction.result,
      line17_copy = line11_fraction.result,
      line18_difference = difference line16_copy line17_copy,
      line19_copy = line10_difference,
      line20_copy = line12_fraction.result,
      line21_difference = fixSubCalculation id $ difference line19_copy line20_copy,
      line22_sum = totalOf [line18_difference, line21_difference.result],
      line23_copy = line_50341_totalSecondContributions,
      line24_copy = line15_fraction.result,
      line25_difference = fixSubCalculation id $ difference line23_copy line24_copy,
      line26_sum = totalOf [line22_sum, line25_difference.result]},
   page5 = case page4.line_50339_totalPensionableEarnings of
         Nothing -> Rank2.pure Nothing
         Just{}
           | any (> 0) page4.line26_sum -> page5{
               part3a = let Page5Part3a{..} = page5.part3a in page5.part3a{
                  line27_copy = page4.line17_copy,
                  line28_copy = page4.line20_copy,
                  line29_copy = page4.line24_copy,
                  line30_sum = totalOf [line28_copy, line29_copy],
                  line31_copy = page4.line26_sum},
               part3b = Rank2.pure Nothing}
           | otherwise -> page5{
               part3a = Rank2.pure Nothing,
               part3b = let Page5Part3b{..} = page5.part3b in page5.part3b{
                  line32_join = if any (< 0) page4.line18_difference then page4.line16_copy else page4.line17_copy,
                  line33_conditionalCopy = if any (< 0) page4.line21_difference.result then Nothing else page4.line20_copy,
                  line34to37 = if all (>= 0) page4.line21_difference.result then Rank2.pure Nothing
                               else flip fixEq line34to37 $ \Page5Part3bCond1{..}-> Page5Part3bCond1{
                     line34_abs = abs <$> page4.line21_difference.result,
                     line35_copy = page4.line19_copy,
                     line36_least = maximum [Just 0, minimum [page4.line18_difference, line34_abs]],
                     line37_sum = fixSubCalculation id $ totalOf [line35_copy, line36_least]},
                  line38_conditionalCopy = if all (== 0) page4.line25_difference.result then page4.line24_copy else Nothing,
                  line39to42 = if all (>= 0) page4.line25_difference.result then Rank2.pure Nothing
                               else flip fixEq line39to42 $ \Page5Part3bCond2{..}-> Page5Part3bCond2{
                     line39_abs = abs <$> page4.line25_difference.result,
                     line40_copy = page4.line23_copy,
                     line41_least = maximum [Just 0, minimum [page4.line22_sum, line39_abs]],
                     line42_sum = fixSubCalculation id $ totalOf [line40_copy, line41_least]},
                  line43_sum = totalOf [line33_conditionalCopy, line34to37.line37_sum.result,
                                        line38_conditionalCopy, line39to42.line42_sum.result]}},
   page6 = case page4.line_50339_totalPensionableEarnings of
             Just{} -> Rank2.pure Nothing
             Nothing -> let Page6{..} = page6 in page6{
               line3_sum = max 0 <$> totalOf [line1_netSelfEmploymentEarnings,
                                              line_50373_additionalEmploymentEarningsOffT4],
               line4_least = fixSubCalculation id $ minimum [line3_sum, page3.lineB_additionalMaxPensionableEarnings],
               line5_copyC = page3.lineC_maxPensionableEarnings,
               line6_difference = fixSubCalculation id $ nonNegativeDifference line4_least.result line5_copyC,
               line7_difference = nonNegativeDifference line4_least.result line6_difference.result,
               line8_copyD = page3.lineD_maxBasicExemption,
               line9_difference = nonNegativeDifference line7_difference line8_copyD,
               line10_fraction = fixSubCalculation (0.099 *) line9_difference,
               line11_fraction = fixSubCalculation (0.02 *) line9_difference,
               line12_fraction = fixSubCalculation (0.08 *) line6_difference.result,
               line13_sum = fixSubCalculation id $ totalOf [line11_fraction.result, line12_fraction.result],
               line14_sum = totalOf [line10_fraction.result, line13_sum.result],
               line15_half = fixSubCalculation (/ 2) line10_fraction.result,
               line16_copy = line13_sum.result,
               line17_sum = totalOf [line15_half.result, line16_copy]},
   page7 = case page4.line_50339_totalPensionableEarnings of
             Nothing -> Rank2.pure Nothing
             Just{} -> let Page7{..} = page7 in page7{
               line4_sum = totalOf [line1_netSelfEmploymentEarnings,
                                    line_50373_additionalEmploymentEarningsOffT4,
                                    line_50399_additionalEmploymentEarningsOnT4],
               line5_copy = page4.line_50340_totalContributions,
               line6to8 = if any (<= 0) page4.line22_sum then Rank2.pure Nothing
                          else let Page7Cond1{..} = line6to8 in Page7Cond1{
                   line6_copy = line5_copy,
                   line7_copy = page4.line13_sum,
                   line8_difference = fixSubCalculation id $ nonNegativeDifference line6_copy line7_copy},
               line9_difference = nonNegativeDifference line5_copy line6to8.line8_difference.result,
               line10_fraction = fixSubCalculation (16.80672 *) line9_difference,
               line11_copy = page4.line_50341_totalSecondContributions,
               line12to14 = if any (<= 0) page4.line26_sum then Rank2.pure Nothing
                            else let Page7Cond2{..} = line12to14 in Page7Cond2{
                   line12_copy = line11_copy,
                   line13_copy = page4.line15_fraction.result,
                   line14_difference = fixSubCalculation id $ difference line12_copy line13_copy},
               line15_difference = if any (< 0) line12to14.line14_difference.result then line12to14.line13_copy
                                   else nonNegativeDifference line11_copy line12to14.line14_difference.result,
               line16_fraction = fixSubCalculation (25 *) line15_difference,
               line17_sum = totalOf [line10_fraction.result, line16_fraction.result],
               line18to20 = if any (> 0) page4.line26_sum then Rank2.pure Nothing
                            else let Page7Cond3{..} = line18to20 in Page7Cond3{
                   line18_abs = abs <$> page4.line25_difference.result,
                   line19_least = if any (> 0) page4.line22_sum then minimum [page4.line22_sum, line18_abs] else Just 0,
                   line20_fraction = fixSubCalculation (25 *) line19_least},
               line21_copy = line4_sum,
               line22_copy = page4.line2_least.result,
               line23_sum = totalOf [line21_copy, line22_copy],
               line24_copyC = page3.lineC_maxPensionableEarnings,
               line25_copyD = page3.lineD_maxBasicExemption,
               line26_difference = nonNegativeDifference line24_copyC line25_copyD,
               line27_copy = line10_fraction.result,
               line28_difference = nonNegativeDifference line26_difference line27_copy},
   page8 = case page4.line_50339_totalPensionableEarnings of
             Nothing -> Rank2.pure Nothing
             Just{} -> let Page8{..} = page8 in page8{
               line29_least = minimum [page7.line4_sum, page7.line28_difference],
               line30_copy = page7.line25_copyD,
               line31_copy = page4.line_50339_totalPensionableEarnings,
               line32_difference = nonNegativeDifference line30_copy line31_copy,
               line33_copy = page7.line4_sum,
               line34_copy = page7.line26_difference,
               line35_difference = fixSubCalculation id $ nonNegativeDifference line33_copy line34_copy,
               line36_difference = fixSubCalculation id
                                   $ nonNegativeDifference line32_difference line35_difference.result,
               line37_difference = nonNegativeDifference line29_least line36_difference.result,
               line38to48 = if page7.line23_sum <= page7.line24_copyC then (Rank2.pure Nothing){line48_least = Just 0}
                            else let Page8Cond1{..} = line38to48 in Page8Cond1{
                   line38_copyE = page3.lineE_maxSubjectToSecondAdditionalContributions,
                   line39_copy = page7.line16_fraction.result,
                   line40_copy = page7.line18to20.line20_fraction.result,
                   line41_sum = fixSubCalculation id $ totalOf [line39_copy, line40_copy],
                   line42_difference = difference line38_copyE line41_sum.result,
                   line43_copy = page7.line4_sum,
                   line44_copy = line32_difference,
                   line45_difference = difference line43_copy line44_copy,
                   line46_copy = line37_difference,
                   line47_difference = difference line45_difference line46_copy,
                   line48_least = minimum [line42_difference, line47_difference]},
               line49_fraction = fixSubCalculation (0.099 *) line37_difference,
               line50_fraction = fixSubCalculation (0.02 *) line37_difference,
               line51_fraction = fixSubCalculation (0.08 *) line38to48.line48_least,
               line52_sum = fixSubCalculation id $ totalOf [line50_fraction.result, line51_fraction.result],
               line53_sum = totalOf [line49_fraction.result, line52_sum.result],
               line54_double = fixSubCalculation (2 *)
                               $ if any (> 0) page4.line26_sum then page4.line26_sum else Nothing,
               line55_difference = difference line53_sum line54_double.result},
   page9 = case page4.line_50339_totalPensionableEarnings of
             Nothing -> Rank2.pure Nothing
             Just{} -> let Page9{..} = page9 in page9{
               line56_half = fixSubCalculation (/ 2)
                             $ if any (< 0) page8.line55_difference then abs <$> page8.line55_difference else Nothing,
               line57_copy = page4.line9_fraction.result,
               line58_copy = page4.line11_fraction.result,
               line59_difference = difference line57_copy line58_copy,
               line60_least = minimum [line57_copy, line58_copy],
               line61_copy = page4.line10_difference,
               line62_copy = page4.line12_fraction.result,
               line63_difference = difference line61_copy line62_copy,
               line64_conditionalCopy = if all (>= 0) line63_difference then line62_copy else Nothing,
               line65_abs = abs <$> line63_difference,
               line66_copy = line61_copy,
               line67_least = if any (> 0) line59_difference then minimum [line59_difference, line65_abs] else Just 0,
               line68_sum = fixSubCalculation id $ totalOf [line66_copy, line67_least],
               line69_copy = page4.line_50341_totalSecondContributions,
               line70_copy = page4.line15_fraction.result,
               line71_difference = difference line69_copy line70_copy,
               line72_conditionalCopy = if any (> 0) line71_difference then line70_copy else Nothing,
               line73_abs = abs <$> line71_difference,
               line74_copy = line69_copy,
               line75_least = if any (> 0) page4.line22_sum then minimum [page4.line22_sum, line73_abs] else Just 0,
               line76_sum = fixSubCalculation id $ totalOf [line74_copy, line75_least],
               line77_sum = totalOf [line64_conditionalCopy <|> line68_sum.result,
                                     line72_conditionalCopy <|> line76_sum.result],
               line78_half = fixSubCalculation (/ 2) page8.line49_fraction.result,
               line79_half = fixSubCalculation (/ 2) page8.line50_fraction.result,
               line80_sum = totalOf [line78_half.result, line79_half.result],
               line81_positiveCopy = maximum [page4.line26_sum, Just 0],
               line82_difference = difference line80_sum line81_positiveCopy,
               line83_fraction = fixSubCalculation (0.831933 *) $ minimum [line80_sum, line81_positiveCopy],
               line84_copy = line83_fraction.result,
               line85_difference = difference line83_fraction.result line84_copy},
   page10 = case page4.line_50339_totalPensionableEarnings of
              Nothing -> Rank2.pure Nothing
              Just{} -> let Page10{..} = page10 in page10{
                line86_copy = page9.line79_half.result,
                line87_copy = page9.line78_half.result,
                line88_copy = page9.line83_fraction.result,
                line89_difference = fixSubCalculation id $ difference line87_copy line88_copy,
                line90_copy = page9.line79_half.result,
                line91_copy = page9.line85_difference,
                line92_difference = fixSubCalculation id $ difference line90_copy line91_copy,
                line93_half = fixSubCalculation (/ 2) page8.line51_fraction.result,
                line94_copy = line93_half.result,
                line95_join = if any (< 0) page9.line82_difference then abs <$> page9.line82_difference
                              else Just 0,
                line96_difference = fixSubCalculation id $ nonNegativeDifference line94_copy line95_join,
                line97_sum = totalOf [line86_copy, line89_difference.result,
                                      line92_difference.result, line96_difference.result]}}
schedule8Fields :: Schedule8 FieldConst
schedule8Fields = within "form1" Rank2.<$> Schedule8{
   page2 = within "Page2" . within "Part1" . within "Election-Evocation" Rank2.<$> Page2{
      line_50372_stopMonth = Field ["Line50372", "Month_grp", "Stop-Month"] Month,
      line_50374_revokeMonth = Field ["Line50374", "Month_grp", "Revoke-Month"] Month},
   page3 = within "Page3" . within "Part2" Rank2.<$> Page3{
      lineA_months = Field ["LineA", "Number"] Count,
      lineB_additionalMaxPensionableEarnings = Field ["LineB", "Amount"] Amount,
      lineC_maxPensionableEarnings = Field ["LineC", "Amount"] Amount,
      lineD_maxBasicExemption = Field ["LineD", "Amount"] Amount,
      lineE_maxSubjectToSecondAdditionalContributions = Field ["LineE", "Amount"] Amount},
   page4 = within "Page4" . within "Part3" Rank2.<$> Page4{
      line_50339_totalPensionableEarnings = Field ["Line1", "Amount"] Amount,
      line2_least = subCalculationFields "Line2" ["Amount1"] ["Amount"],
      line3_copyC = Field ["Line3", "Amount"] Amount,
      line4_difference = subCalculationFields "Line4" ["Amount1"] ["Amount"],
      line5_difference = Field ["Line5", "Amount"] Amount,
      line6_copyD = Field ["Line6", "Amount"] Amount,
      line7_difference = Field ["Line7", "Amount"] Amount,
      line_50340_totalContributions = Field ["Line8", "Amount"] Amount,
      line9_fraction = subCalculationFields "Line9" ["Amount1"] ["Amount2"],
      line10_difference = Field ["Line10", "Amount"] Amount,
      line11_fraction = subCalculationFields "Line11" ["Amount1"] ["Amount2"],
      line12_fraction = subCalculationFields "Line12" ["Amount1"] ["Amount2"],
      line13_sum = Field ["Line13", "Amount"] Amount,
      line_50341_totalSecondContributions = Field ["Line14", "Amount"] Amount,
      line15_fraction = subCalculationFields "Line15" ["Amount1"] ["Amount2"],
      line16_copy = Field ["Line16", "Amount"] Amount,
      line17_copy = Field ["Line17", "Amount"] Amount,
      line18_difference = Field ["Line18", "Amount"] Amount,
      line19_copy = Field ["Line19", "Amount"] Amount,
      line20_copy = Field ["Line20", "Amount"] Amount,
      line21_difference = subCalculationFields "Line21" ["Amount1"] ["Amount2"],
      line22_sum = Field ["Line22", "Amount"] Amount,
      line23_copy = Field ["Line23", "Amount"] Amount,
      line24_copy = Field ["Line24", "Amount"] Amount,
      line25_difference = subCalculationFields "Line25" ["Amount1"] ["Amount2"],
      line26_sum = Field ["Line26", "Amount"] Amount},
   page5 = within "Page5" . within "Part3_Cont" Rank2.<$> Page5{
      part3a = Page5Part3a{
         line27_copy = Field ["Line27", "Amount"] Amount,
         line28_copy = Field ["Line28", "Amount"] Amount,
         line29_copy = Field ["Line29", "Amount"] Amount,
         line30_sum = Field ["Line30", "Amount"] Amount,
         line31_copy = Field ["Line31", "Amount"] Amount},
      part3b = Page5Part3b{
         line32_join = Field ["Line32", "Amount"] Amount,
         line33_conditionalCopy = Field ["Line33", "Amount"] Amount,
         line34to37 = Page5Part3bCond1{
            line34_abs = Field ["Line34", "Amount"] Amount,
            line35_copy = Field ["Line35", "Amount"] Amount,
            line36_least = Field ["Line36", "Amount"] Amount,
            line37_sum = subCalculationFields "Line37" ["Amount1"] ["Amount"]},
         line38_conditionalCopy = Field ["Line38", "Amount"] Amount,
         line39to42 = Page5Part3bCond2{
            line39_abs = Field ["Line39", "Amount"] Amount,
            line40_copy = Field ["Line40", "Amount"] Amount,
            line41_least = Field ["Line41", "Amount"] Amount,
            line42_sum = subCalculationFields "Line42" ["Amount1"] ["Amount"]},
         line43_sum = Field ["Line43", "Amount"] Amount}},
   page6 = within "Page6" . within "Part4" Rank2.<$> Page6{
      line1_netSelfEmploymentEarnings = Field ["Line1", "Amount"] Amount,
      line_50373_additionalEmploymentEarningsOffT4 = Field ["Line2", "Amount"] Amount,
      line3_sum = Field ["Line3", "Amount"] Amount,
      line4_least = subCalculationFields "Line4" ["Amount1"] ["Amount"],
      line5_copyC = Field ["Line5", "Amount"] Amount,
      line6_difference = subCalculationFields "Line6" ["Amount1"] ["Amount"],
      line7_difference = Field ["Line7", "Amount"] Amount,
      line8_copyD = Field ["Line8", "Amount"] Amount,
      line9_difference = Field ["Line9", "Amount"] Amount,
      line10_fraction = subCalculationFields "Line10" ["Amount1"] ["Amount2"],
      line11_fraction = subCalculationFields "Line11" ["Amount1"] ["Amount2"],
      line12_fraction = subCalculationFields "Line12" ["Amount1"] ["Amount2"],
      line13_sum = subCalculationFields "Line13" ["Amount1"] ["Amount"],
      line14_sum = Field ["Line14", "Amount"] Amount,
      line15_half = subCalculationFields "Line15" ["Amount1"] ["Amount2"],
      line16_copy = Field ["Line16", "Amount"] Amount,
      line17_sum = Field ["Line17", "amount"] Amount},
   page7 = within "Page7" . within "Part5" Rank2.<$> Page7{
      line1_netSelfEmploymentEarnings = Field ["Line1", "Amount"] Amount,
      line_50373_additionalEmploymentEarningsOffT4 = Field ["Line2", "Amount"] Amount,
      line_50399_additionalEmploymentEarningsOnT4 = Field ["Line3", "Amount"] Amount,
      line4_sum = Field ["Line4", "Amount"] Amount,
      line5_copy = Field ["Line5", "Amount"] Amount,
      line6to8 = Page7Cond1{
         line6_copy = Field ["Line6", "Amount"] Amount,
         line7_copy = Field ["Line7", "Amount"] Amount,
         line8_difference = subCalculationFields "Line8" ["Amount1"] ["Amount"]},
      line9_difference = Field ["Line9", "Amount"] Amount,
      line10_fraction = subCalculationFields "Line10" ["Amount1"] ["Amount2"],
      line11_copy = Field ["Line11", "Amount"] Amount,
      line12to14 = Page7Cond2{
         line12_copy = Field ["Line12", "Amount"] Amount,
         line13_copy = Field ["Line13", "Amount"] Amount,
         line14_difference = subCalculationFields "Line14" ["Amount1"] ["Amount"]},
      line15_difference = Field ["Line15", "Amount"] Amount,
      line16_fraction = subCalculationFields "Line16" ["Amount1"] ["Amount2"],
      line17_sum = Field ["Line17", "Amount"] Amount,
      line18to20 = Page7Cond3{
         line18_abs = Field ["Line18", "Amount"] Amount,
         line19_least = Field ["Line19", "Amount"] Amount,
         line20_fraction = subCalculationFields "Line20" ["Amount1"] ["Amount2"]},
      line21_copy = Field ["Line21", "Amount"] Amount,
      line22_copy = Field ["Line22", "Amount"] Amount,
      line23_sum = Field ["Line23", "Amount"] Amount,
      line24_copyC = Field ["Line24", "Amount"] Amount,
      line25_copyD = Field ["Line25", "Amount"] Amount,
      line26_difference = Field ["Line26", "Amount"] Amount,
      line27_copy = Field ["Line27", "Amount"] Amount,
      line28_difference = Field ["Line28", "Amount"] Amount},
   page8 = within "Page8" . within "Part5_Cont" Rank2.<$> Page8{
      line29_least = Field ["Line29", "Amount"] Amount,
      line30_copy = Field ["Line30", "Amount"] Amount,
      line31_copy = Field ["Line31", "Amount"] Amount,
      line32_difference = Field ["Line32", "Amount"] Amount,
      line33_copy = Field ["Line33", "Amount"] Amount,
      line34_copy = Field ["Line34", "Amount"] Amount,
      line35_difference = subCalculationFields "Line35" ["Amount1"] ["Amount2"],
      line36_difference = subCalculationFields "Line36" ["Amount1"] ["Amount2"],
      line37_difference = Field ["Line37", "Amount"] Amount,
      line38to48 = Page8Cond1{
         line38_copyE = Field ["Line38", "Amount"] Amount,
         line39_copy = Field ["Line39", "Amount"] Amount,
         line40_copy = Field ["Line40", "Amount"] Amount,
         line41_sum = subCalculationFields "Line41" ["Amount"] ["Amount2"],
         line42_difference = Field ["Line42", "Amount"] Amount,
         line43_copy = Field ["Line43", "Amount"] Amount,
         line44_copy = Field ["Line44", "Amount"] Amount,
         line45_difference = Field ["Line45", "Amount"] Amount,
         line46_copy = Field ["Line46", "Amount"] Amount,
         line47_difference = Field ["Line47", "Amount"] Amount,
         line48_least = Field ["Line48", "Amount"] Amount},
      line49_fraction = subCalculationFields "Line49" ["Amount1"] ["Amount2"],
      line50_fraction = subCalculationFields "Line50" ["Amount1"] ["Amount2"],
      line51_fraction = subCalculationFields "Line51" ["Amount1"] ["Amount2"],
      line52_sum = subCalculationFields "Line52" ["Amount"] ["Amount2"],
      line53_sum = Field ["Line53", "Amount"] Amount,
      line54_double = subCalculationFields "Line54" ["Amount1"] ["Amount2"],
      line55_difference = Field ["Line55", "Amount"] Amount},
   page9 = within "Page9" . within "Part5_Cont" Rank2.<$> Page9{
      line56_half = subCalculationFields "Line56" ["Amount1"] ["Amount"],
      line57_copy = Field ["Line57", "Amount"] Amount,
      line58_copy = Field ["Line58", "Amount"] Amount,
      line59_difference = Field ["Line59", "Amount"] Amount,
      line60_least = Field ["Line60", "Amount"] Amount,
      line61_copy = Field ["Line61", "Amount"] Amount,
      line62_copy = Field ["Line62", "Amount"] Amount,
      line63_difference = Field ["Line63", "Amount"] Amount,
      line64_conditionalCopy = Field ["Line64", "Amount"] Amount,
      line65_abs = Field ["Line65", "Amount"] Amount,
      line66_copy = Field ["Line66", "Amount"] Amount,
      line67_least = Field ["Line67", "Amount"] Amount,
      line68_sum = subCalculationFields "Line68" ["Amount"] ["Amount2"],
      line69_copy = Field ["Line69", "Amount"] Amount,
      line70_copy = Field ["Line70", "Amount"] Amount,
      line71_difference = Field ["Line71", "Amount"] Amount,
      line72_conditionalCopy = Field ["Line72", "Amount"] Amount,
      line73_abs = Field ["Line73", "Amount"] Amount,
      line74_copy = Field ["Line74", "Amount"] Amount,
      line75_least = Field ["Line75", "Amount"] Amount,
      line76_sum = subCalculationFields "Line76" ["Amount"] ["Amount2"],
      line77_sum = Field ["Line77", "Amount"] Amount,
      line78_half = subCalculationFields "Line78" ["Amount1"] ["Amount2"],
      line79_half = subCalculationFields "Line79" ["Amount1"] ["Amount2"],
      line80_sum = Field ["Line80", "Amount2"] Amount,
      line81_positiveCopy = Field ["Line81", "Amount2"] Amount,
      line82_difference = Field ["Line82", "Amount"] Amount,
      line83_fraction = subCalculationFields "Line83" ["AmountA", "Amount1"] ["Amount2"],
      line84_copy = Field ["Line84", "Amount2"] Amount,
      line85_difference = Field ["Line85", "Amount"] Amount},
   page10 = within "Page10" . within "Part5-Cont" Rank2.<$> Page10{
      line86_copy = Field ["Line86", "Amount2"] Amount,
      line87_copy = Field ["Line87", "Amount2"] Amount,
      line88_copy = Field ["Line88", "Amount2"] Amount,
      line89_difference = subCalculationFields "Line89" ["Amount1"] ["Amount"],
      line90_copy = Field ["Line90", "Amount2"] Amount,
      line91_copy = Field ["Line91", "Amount2"] Amount,
      line92_difference = subCalculationFields "Line92" ["Amount1"] ["Amount"],
      line93_half = subCalculationFields "Line93" ["Amount1"] ["Amount2"],
      line94_copy = Field ["Line94", "Amount2"] Amount,
      line95_join = Field ["Line95", "Amount2"] Amount,
      line96_difference = subCalculationFields "Line96" ["Amount1"] ["Amount"],
      line97_sum = Field ["Line97", "Amount"] Amount}}
