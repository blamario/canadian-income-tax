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
import Data.Time.Calendar (MonthOfYear)
import Language.Haskell.TH qualified as TH
import Rank2 qualified
import Rank2.TH qualified
import Transformation.Shallow.TH qualified

import Tax.Canada.Shared (SubCalculation(calculation, result), fixSubCalculation, subCalculationFields)
import Tax.FDF (Entry (Amount, Count, Month), FieldConst (Field), within)
import Tax.Util (fixEq, difference, nonNegativeDifference, totalOf)

data Schedule8 line = Schedule8{
   page2 :: Page2 line,
   page3 :: Page3 line,
   page4 :: Page4 line,
   page5 :: Page5 line,
   page6 :: Page6 line,
   page7 :: Page7 line,
   page8 :: Page8 line,
   page9 :: Page9 line,
   page10 :: Page10 line,
   page11 :: Page11 line}

data Page2 line = Page2{
   line_50372_stopMonth :: line MonthOfYear,
   line_50374_revokeMonth :: line MonthOfYear}

-- 2025: B=max pensionable, C=max 2nd additional, D=additional max pensionable, E=basic exemption
data Page3 line = Page3{
   lineA_months :: line Word,
   lineB_maxPensionableEarnings :: line Centi,
   lineC_maxSubjectToSecondAdditionalContributions :: line Centi,
   lineD_additionalMaxPensionableEarnings :: line Centi,
   lineE_maxBasicExemption :: line Centi}

-- Part 3: Contributions and overpayment on employment income (lines 1-24)
data Page4 line = Page4{
   line_50339_totalPensionableEarnings :: line Centi,
   line2_least :: SubCalculation line,
   line3_copyB :: line Centi,
   line4_difference :: SubCalculation line,
   line5_difference :: line Centi,
   line6_copyE :: line Centi,
   line7_difference :: line Centi,
   line_50340_totalContributions :: line Centi,
   line9_fraction :: SubCalculation line,
   line10_difference :: line Centi,
   line11_fraction :: SubCalculation line,
   line12_fraction :: SubCalculation line,
   line13_sum :: line Centi,
   line14_copy :: line Centi,
   line15_copy :: line Centi,
   line16_difference :: line Centi,
   line17_copy :: line Centi,
   line18_copy :: line Centi,
   line19_difference :: SubCalculation line,
   line20_sum :: line Centi,
   line_50341_totalSecondContributions :: line Centi,
   line22_fraction :: SubCalculation line,
   line23_difference :: SubCalculation line,
   line24_sum :: line Centi}

-- Part 3 continued: Part 3a (lines 25-29) and Part 3b (lines 30-48)
data Page5 line = Page5{
   part3a :: Page5Part3a line,
   part3b :: Page5Part3b line}

data Page5Part3a line = Page5Part3a{
   line25_copy :: line Centi,
   line26_copy :: line Centi,
   line27_copy :: line Centi,
   line28_sum :: line Centi,
   line29_copy :: line Centi}

data Page5Part3b line = Page5Part3b{
   line30_conditionalCopy :: line Centi,
   line31_abs :: line Centi,
   line32_conditionalLeast :: SubCalculation line,
   line33_difference :: line Centi,
   line34_sum :: line Centi,
   line35_conditionalLeast :: line Centi,
   line36_sum :: line Centi}

data Page6 line = Page6{
   part3 :: Page6Part3 line,
   part4 :: Page6Part4 line}

data Page6Part3 line = Page6Part3{
   line37_conditionalCopy :: line Centi,
   line38_abs :: line Centi,
   line39_conditionalLeast :: SubCalculation line,
   line40_difference :: line Centi,
   line41_sum :: line Centi,
   line42_conditionalLeast :: line Centi,
   line43_sum :: line Centi,
   line44_conditionalCopy :: line Centi,
   line45_abs :: line Centi,
   line46_conditionalLeast :: line Centi,
   line47_sum :: SubCalculation line,
   line48_sum :: line Centi}

-- Part 4: Contributions on self-employment income only (lines 1-17)
data Page6Part4 line = Page6Part4{
   line1_netSelfEmploymentEarnings :: line Centi,
   line_50373_additionalEmploymentEarningsOffT4 :: line Centi,
   line3_sum :: line Centi,
   line4_least :: SubCalculation line,
   line5_copyB :: line Centi,
   line6_difference :: SubCalculation line,
   line7_difference :: line Centi,
   line8_copyE :: line Centi,
   line9_difference :: line Centi}

data Page7 line = Page7{
   part4 :: Page7Part4 line,
   part5 :: Page7Part5 line}

data Page7Part4 line = Page7Part4{
   line10_fraction :: SubCalculation line,
   line11_fraction :: SubCalculation line,
   line12_fraction :: SubCalculation line,
   line13_sum :: SubCalculation line,
   line14_sum :: line Centi,
   line15_half :: SubCalculation line,
   line16_copy :: line Centi,
   line17_sum :: line Centi}

-- Part 5: Contributions on self-employment income when you also have employment income (lines 1-16)
data Page7Part5 line = Page7Part5{
   line1_netSelfEmploymentEarnings :: line Centi,
   line_50373_additionalEmploymentEarningsOffT4 :: line Centi,
   line_50399_additionalEmploymentEarningsOnT4 :: line Centi,
   line4_sum :: line Centi,
   line5_copy :: line Centi,
   line6_conditionalCopy :: line Centi,
   line7_difference :: line Centi,
   line8_abs :: line Centi,
   line9_min :: line Centi,
   line10_sum :: line Centi}

data Page7Cond1 line = Page7Cond1{
   line8_abs :: line Centi,
   line9_conditionalLeast :: line Centi}

data Page7Cond2 line = Page7Cond2{
   line14_abs :: line Centi,
   line15_conditionalLeast :: line Centi}

-- Part 5 continued (lines 17-42)
data Page8 line = Page8{
   line11_copy :: line Centi,
   line12_copy :: line Centi,
   line13_difference :: line Centi,
   line14to15 :: Page7Cond2 line,
   line16_sum :: line Centi,
   line17_copyB :: line Centi,
   line18_copyE :: line Centi,
   line19_difference :: line Centi,
   line20_fraction :: SubCalculation line,
   line21_difference :: line Centi,
   line22_least :: line Centi,
   line23to29 :: Page8Cond1 line,
   line30_difference :: line Centi,
   line31_copy :: line Centi,
   line32_copy :: line Centi,
   line33_sum :: line Centi,
   line34to41 :: Page8Cond2 line}

data Page8Cond1 line = Page8Cond1{
   line23_copyE :: line Centi,
   line24_copy :: line Centi,
   line25_difference :: line Centi,
   line26_copy :: line Centi,
   line27_copy :: line Centi,
   line28_difference :: SubCalculation line,
   line29_difference :: SubCalculation line}

data Page8Cond2 line = Page8Cond2{
   line34_copyC :: line Centi,
   line35_fraction :: SubCalculation line,
   line36_difference :: line Centi,
   line37_copy :: line Centi,
   line38_copy :: line Centi,
   line39_difference :: line Centi,
   line40_copy :: line Centi,
   line41_difference :: line Centi}

-- Part 5 continued (lines 43-57)
data Page9 line = Page9{
   line42_least :: line Centi,
   line43_fraction :: SubCalculation line,
   line44_fraction :: SubCalculation line,
   line45_fraction :: SubCalculation line,
   line46_sum :: SubCalculation line,
   line47_sum :: line Centi,
   line48_double :: SubCalculation line,
   line49_difference :: line Centi,
   line50_half :: SubCalculation line,
   line51_conditionalCopy :: line Centi,
   line52_abs :: line Centi,
   line53_conditionalLeast :: SubCalculation line,
   line54_difference :: line Centi,
   line55_sum :: line Centi,
   line56_conditionalLeast :: line Centi,
   line57_sum :: line Centi}

-- Part 5 continued (lines 58-77)
data Page10 line = Page10{
   line58_conditionalCopy :: line Centi,
   line59_abs :: line Centi,
   line60_conditionalLeast :: SubCalculation line,
   line61_difference :: line Centi,
   line62_sum :: line Centi,
   line63_conditionalLeast :: line Centi,
   line64_sum :: line Centi,
   line65_conditionalCopy :: line Centi,
   line66_abs :: line Centi,
   line67_conditionalLeast :: line Centi,
   line68_sum :: SubCalculation line,
   line69_sum :: line Centi,
   line70_half :: SubCalculation line,
   line71_half :: SubCalculation line,
   line72_sum :: line Centi,
   line73_positiveCopy :: line Centi,
   line74_difference :: line Centi,
   line75_fraction :: SubCalculation line,
   line76_copy :: line Centi,
   line77_difference :: line Centi}

-- Part 5 continued (lines 78-88)
data Page11 line = Page11{
   line78_copy :: line Centi,
   line79_copy :: line Centi,
   line80_difference :: SubCalculation line,
   line81_copy :: SubCalculation line,
   line82_copy :: line Centi,
   line83_difference :: SubCalculation line,
   line84_half :: SubCalculation line,
   line85_copy :: line Centi,
   line86_conditionalCopy :: line Centi,
   line87_difference :: SubCalculation line,
   line88_sum :: line Centi}


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
    ''Page5, ''Page5Part3a, ''Page5Part3b,
    ''Page6, ''Page6Part3, ''Page6Part4,
    ''Page7, ''Page7Part4, ''Page7Part5, ''Page7Cond1, ''Page7Cond2,
    ''Page8, ''Page8Cond1, ''Page8Cond2,
    ''Page9, ''Page10, ''Page11])

fixSchedule8 :: Schedule8 Maybe -> Schedule8 Maybe
fixSchedule8 = fixEq $ \Schedule8{page2, page3, page4, page5, page6, page7, page8, page9, page10, page11}-> Schedule8{
   page2 = page2,
   page3 = page3{
      lineA_months = page3.lineA_months <|> Just 12,
      -- 2025 values: B=$71,300, C=$9,900, D=$81,200, E=$3,500
      lineB_maxPensionableEarnings = ((/ 12) . (71_300 *) . fromIntegral . min 12) <$> page3.lineA_months,
      lineC_maxSubjectToSecondAdditionalContributions = ((/ 12) . (9_900 *) . fromIntegral . min 12) <$> page3.lineA_months,
      lineD_additionalMaxPensionableEarnings = ((/ 12) . (81_200 *) . fromIntegral . min 12) <$> page3.lineA_months,
      lineE_maxBasicExemption = ((/ 12) . (3500 *) . fromIntegral . min 12) <$> page3.lineA_months},
   page4 = let Page4{..} = page4 in case line_50339_totalPensionableEarnings of
         Nothing -> Rank2.pure Nothing
         Just{} -> page4{
      line2_least = fixSubCalculation id $ min page3.lineD_additionalMaxPensionableEarnings line_50339_totalPensionableEarnings,
      line3_copyB = page3.lineB_maxPensionableEarnings,
      line4_difference = fixSubCalculation id $ nonNegativeDifference line2_least.result line3_copyB,
      line5_difference = nonNegativeDifference line2_least.result line4_difference.result,
      line6_copyE = page3.lineE_maxBasicExemption,
      line7_difference = nonNegativeDifference line5_difference line6_copyE,
      line9_fraction = fixSubCalculation (0.831933 *) line_50340_totalContributions,
      line10_difference = difference line_50340_totalContributions line9_fraction.result,
      line11_fraction = fixSubCalculation (0.0495 *) line7_difference,
      line12_fraction = fixSubCalculation (0.01 *) line7_difference,
      line13_sum = totalOf [line11_fraction.result, line12_fraction.result],
      line14_copy = line9_fraction.result,
      line15_copy = line11_fraction.result,
      line16_difference = difference line14_copy line15_copy,
      line17_copy = line10_difference,
      line18_copy = line12_fraction.result,
      line19_difference = fixSubCalculation id $ difference line17_copy line18_copy,
      line20_sum = totalOf [line16_difference, line19_difference.result],
      line22_fraction = fixSubCalculation (0.04 *) line4_difference.result,
      line23_difference = fixSubCalculation id $ difference line_50341_totalSecondContributions line22_fraction.result,
      line24_sum = totalOf [line20_sum, line23_difference.result]},
   page5 = case page4.line_50339_totalPensionableEarnings of
         Nothing -> Rank2.pure Nothing
         Just{}
           | any (> 0) page4.line24_sum -> page5{
               part3a = let Page5Part3a{..} = page5.part3a in page5.part3a{
                  line25_copy = page4.line15_copy,
                  line26_copy = page4.line18_copy,
                  line27_copy = page4.line22_fraction.result,
                  line28_sum = totalOf [line26_copy, line27_copy],
                  line29_copy = page4.line24_sum},
               part3b = Rank2.pure Nothing}
           | otherwise -> page5{
               part3a = Rank2.pure Nothing,
               part3b = let Page5Part3b{..} = page5.part3b in page5.part3b{
                  -- Lines 30-36: Base CPP contributions through employment income
                  line30_conditionalCopy = if all (>= 0) page4.line16_difference then page4.line15_copy else page4.line14_copy,
                  line31_abs = if any (< 0) page4.line16_difference then abs <$> page4.line16_difference else Nothing,
                  line32_conditionalLeast = fixSubCalculation id
                                            $ if any (< 0) page4.line16_difference && any (> 0) page4.line19_difference.result
                                              then minimum [page4.line19_difference.result, line31_abs]
                                              else Just 0,
                  line33_difference = difference line31_abs line32_conditionalLeast.calculation,
                  line34_sum = totalOf [line30_conditionalCopy, line32_conditionalLeast.result],
                  line35_conditionalLeast = if any (> 0) page4.line23_difference.result && any (> 0) line33_difference
                                            then minimum [page4.line23_difference.result, line33_difference]
                                            else Just 0,
                  line36_sum = totalOf [line34_sum, line35_conditionalLeast]}},
   page6 = case page4.line_50339_totalPensionableEarnings of
             Just{} -> Page6{
               part3 = if any (> 0) page4.line24_sum then Rank2.pure Nothing else
                  let section@Page6Part3{..} = page6.part3 in section{
                  -- Lines 37-43: First additional contributions
                  line37_conditionalCopy = if all (>= 0) page4.line19_difference.result then page4.line18_copy else page4.line17_copy,
                  line38_abs = if any (< 0) page4.line19_difference.result then abs <$> page4.line19_difference.result else Nothing,
                  line39_conditionalLeast = fixSubCalculation id
                                            $ if any (< 0) page4.line19_difference.result && any (> 0) page4.line16_difference
                                              then minimum [page4.line16_difference, line38_abs]
                                              else Just 0,
                  line40_difference = difference line38_abs line39_conditionalLeast.result,
                  line41_sum = totalOf [line37_conditionalCopy, line39_conditionalLeast.result],
                  line42_conditionalLeast = if any (> 0) page4.line23_difference.result && any (> 0) line40_difference
                                            then minimum [line40_difference, nonNegativeDifference page4.line23_difference.result page5.part3b.line35_conditionalLeast]
                                            else Just 0,
                  line43_sum = totalOf [line41_sum, line42_conditionalLeast],
                  -- Lines 44-48: Second additional contributions
                  line44_conditionalCopy = if all (>= 0) page4.line23_difference.result then page4.line22_fraction.result else page4.line_50341_totalSecondContributions,
                  line45_abs = if any (< 0) page4.line23_difference.result then abs <$> page4.line23_difference.result else Nothing,
                  line46_conditionalLeast = if any (< 0) page4.line23_difference.result && any (> 0) page4.line20_sum
                                            then minimum [page4.line20_sum, line45_abs]
                                            else Just 0,
                  line47_sum = fixSubCalculation id $ totalOf [line44_conditionalCopy, line46_conditionalLeast],
                  line48_sum = totalOf [line43_sum, line47_sum.result]},
               part4 = Rank2.pure Nothing}
             Nothing -> Page6{
               part3 = Rank2.pure Nothing,
               part4 = let section@Page6Part4{..} = page6.part4 in section{
                  line3_sum = max 0 <$> totalOf [line1_netSelfEmploymentEarnings,
                                                 line_50373_additionalEmploymentEarningsOffT4],
                  line4_least = fixSubCalculation id $ minimum [line3_sum, page3.lineD_additionalMaxPensionableEarnings],
                  line5_copyB = page3.lineB_maxPensionableEarnings,
                  line6_difference = fixSubCalculation id $ nonNegativeDifference line4_least.result line5_copyB,
                  line7_difference = nonNegativeDifference line4_least.result line6_difference.result,
                  line8_copyE = page3.lineE_maxBasicExemption,
                  line9_difference = nonNegativeDifference line7_difference line8_copyE}},
   page7 = case page4.line_50339_totalPensionableEarnings of
             Nothing -> Page7{
               part4 = let section@Page7Part4{..} = page7.part4 in section{
                  line10_fraction = fixSubCalculation (0.099 *) page6.part4.line9_difference,
                  line11_fraction = fixSubCalculation (0.02 *) page6.part4.line9_difference,
                  line12_fraction = fixSubCalculation (0.08 *) page6.part4.line6_difference.result,
                  line13_sum = fixSubCalculation id $ totalOf [line11_fraction.result, line12_fraction.result],
                  line14_sum = totalOf [line10_fraction.result, line13_sum.result],
                  line15_half = fixSubCalculation (/ 2) line10_fraction.result,
                  line16_copy = line13_sum.result,
                  line17_sum = totalOf [line15_half.result, line16_copy]},
                part5 = Rank2.pure Nothing}
             Just{} -> Page7{
               part4 = Rank2.pure Nothing,
               part5 = let section@Page7Part5{..} = page7.part5 in section{
                  line4_sum = totalOf [line1_netSelfEmploymentEarnings,
                                       line_50373_additionalEmploymentEarningsOffT4,
                                       line_50399_additionalEmploymentEarningsOnT4],
                  line5_copy = page4.line_50340_totalContributions,
                  line6_conditionalCopy = if all (>= 0) page4.line20_sum then page4.line20_sum else Just 0,
                  line7_difference = nonNegativeDifference line5_copy line6_conditionalCopy,
                  line8_abs = if any (< 0) page4.line20_sum && any (> 0) page4.line23_difference.result
                              then abs <$> page4.line20_sum
                              else Just 0,
                  line9_min = minimum [page4.line23_difference.result, line8_abs],
                  line10_sum = totalOf [line7_difference, line9_min]}},
   page8 = case page4.line_50339_totalPensionableEarnings of
             Nothing -> Rank2.pure Nothing
             Just{} -> let Page8{..} = page8 in page8{
               line11_copy = page4.line_50341_totalSecondContributions,
               line12_copy = if any (> 0) page4.line23_difference.result then page4.line23_difference.result else Just 0,
               line13_difference = nonNegativeDifference line11_copy line12_copy,
               line14to15 = if all (>= 0) page4.line23_difference.result || all (< 0) page4.line20_sum
                            then (Rank2.pure Nothing){line15_conditionalLeast = Just 0}
                            else let Page7Cond2{..} = line14to15 in Page7Cond2{
                              line14_abs = abs <$> page4.line23_difference.result,
                              line15_conditionalLeast = minimum [page4.line20_sum, line14_abs]},
               line16_sum = totalOf [line13_difference, line14to15.line15_conditionalLeast],
               line17_copyB = page3.lineB_maxPensionableEarnings,
               line18_copyE = page3.lineE_maxBasicExemption,
               line19_difference = nonNegativeDifference line17_copyB line18_copyE,
               line20_fraction = fixSubCalculation (16.80672 *) page7.part5.line10_sum,
               line21_difference = nonNegativeDifference line19_difference line20_fraction.result,
               line22_least = minimum [page7.part5.line4_sum, line21_difference],
               line23to29 = if page4.line_50339_totalPensionableEarnings >= page8.line18_copyE
                            then (Rank2.pure Nothing){line29_difference = fixSubCalculation id $ Just 0}
                            else let Page8Cond1{..} = line23to29 in Page8Cond1{
                              line23_copyE = line18_copyE,
                              line24_copy = page4.line_50339_totalPensionableEarnings,
                              line25_difference = nonNegativeDifference line23_copyE line24_copy,
                              line26_copy = page7.part5.line4_sum,
                              line27_copy = line19_difference,
                              line28_difference = fixSubCalculation id $ nonNegativeDifference line26_copy line27_copy,
                              line29_difference = fixSubCalculation id $ nonNegativeDifference line25_difference line28_difference.result},
               line30_difference = nonNegativeDifference line22_least line23to29.line29_difference.result,
               line31_copy = page7.part5.line4_sum,
               line32_copy = page4.line2_least.result,
               line33_sum = totalOf [line31_copy, line32_copy],
               line34to41 = if line33_sum <= line17_copyB then Rank2.pure Nothing
                            else let Page8Cond2{..} = line34to41 in Page8Cond2{
                              line34_copyC = page3.lineC_maxSubjectToSecondAdditionalContributions,
                              line35_fraction = fixSubCalculation (/ 0.04) line16_sum,
                              line36_difference = difference line34_copyC line35_fraction.result,
                              line37_copy = page7.part5.line4_sum,
                              line38_copy = line23to29.line25_difference,
                              line39_difference = difference line37_copy line38_copy,
                              line40_copy = line30_difference,
                              line41_difference = difference line39_difference line40_copy}},
   page9 = case page4.line_50339_totalPensionableEarnings of
             Nothing -> Rank2.pure Nothing
             Just{} -> let Page9{..} = page9 in page9{
               line42_least = minimum [page8.line34to41.line36_difference, page8.line34to41.line41_difference],
               line43_fraction = fixSubCalculation (0.099 *) page8.line30_difference,
               line44_fraction = fixSubCalculation (0.02 *) page8.line30_difference,
               line45_fraction = fixSubCalculation (0.08 *) line42_least,
               line46_sum = fixSubCalculation id $ totalOf [line44_fraction.result, line45_fraction.result],
               line47_sum = totalOf [line43_fraction.result, line46_sum.result],
               line48_double = fixSubCalculation (2 *)
                               $ if any (> 0) page4.line24_sum then page4.line24_sum else Nothing,
               line49_difference = difference line47_sum line48_double.result,
               line50_half = fixSubCalculation (/ 2)
                             $ if any (< 0) line49_difference then abs <$> line49_difference else Nothing,
               -- Lines 51-57: Base CPP contributions through employment income
               line51_conditionalCopy = if all (>= 0) page4.line16_difference then page4.line11_fraction.result else page4.line9_fraction.result,
               line52_abs = if any (< 0) page4.line16_difference then abs <$> page4.line16_difference else Nothing,
               line53_conditionalLeast = fixSubCalculation id
                                         $ if any (< 0) page4.line16_difference && any (> 0) page4.line19_difference.result
                                           then minimum [page4.line19_difference.result, line52_abs]
                                           else Just 0,
               line54_difference = difference line52_abs line53_conditionalLeast.calculation,
               line55_sum = totalOf [line51_conditionalCopy, line53_conditionalLeast.result],
               line56_conditionalLeast = if any (> 0) page4.line23_difference.result && any (> 0) line54_difference
                                         then minimum [page4.line23_difference.result, line54_difference]
                                         else Just 0,
               line57_sum = totalOf [line55_sum, line56_conditionalLeast]},
   page10 = case page4.line_50339_totalPensionableEarnings of
              Nothing -> Rank2.pure Nothing
              Just{} -> let Page10{..} = page10 in page10{
                -- Lines 58-64: First additional contributions through employment income
                line58_conditionalCopy = if all (>= 0) page4.line19_difference.result then page4.line12_fraction.result else page4.line10_difference,
                line59_abs = if any (< 0) page4.line19_difference.result then abs <$> page4.line19_difference.result else Nothing,
                line60_conditionalLeast = fixSubCalculation id
                                          $ if any (< 0) page4.line19_difference.result && any (> 0) page4.line16_difference
                                            then minimum [page4.line16_difference, line59_abs]
                                            else Just 0,
                line61_difference = difference line59_abs line60_conditionalLeast.calculation,
                line62_sum = totalOf [line58_conditionalCopy, line60_conditionalLeast.result],
                line63_conditionalLeast = if any (> 0) page4.line23_difference.result && any (> 0) line61_difference
                                          then minimum [line61_difference, nonNegativeDifference page4.line23_difference.result page9.line56_conditionalLeast]
                                          else Just 0,
                line64_sum = totalOf [line62_sum, line63_conditionalLeast],
                -- Lines 65-69: Second additional contributions through employment income
                line65_conditionalCopy = if all (>= 0) page4.line23_difference.result then page4.line22_fraction.result else page4.line_50341_totalSecondContributions,
                line66_abs = if any (< 0) page4.line23_difference.result then abs <$> page4.line23_difference.result else Nothing,
                line67_conditionalLeast = if any (< 0) page4.line23_difference.result && any (> 0) page4.line20_sum
                                          then minimum [page4.line20_sum, line66_abs]
                                          else Just 0,
                line68_sum = fixSubCalculation id $ totalOf [line65_conditionalCopy, line67_conditionalLeast],
                line69_sum = totalOf [line64_sum, line68_sum.result],
                -- Lines 70-77: Self-employment contributions
                line70_half = fixSubCalculation (/ 2) page9.line43_fraction.result,
                line71_half = fixSubCalculation (/ 2) page9.line44_fraction.result,
                line72_sum = totalOf [line70_half.result, line71_half.result],
                line73_positiveCopy = maximum [page4.line24_sum, Just 0],
                line74_difference = difference line72_sum line73_positiveCopy,
                line75_fraction = fixSubCalculation (0.831933 *) $ minimum [line72_sum, line73_positiveCopy],
                line76_copy = line75_fraction.result,
                line77_difference = difference line75_fraction.result line76_copy},
   page11 = case page4.line_50339_totalPensionableEarnings of
              Nothing -> Rank2.pure Nothing
              Just{} -> let Page11{..} = page11 in page11{
                line78_copy = page10.line70_half.result,
                line79_copy = page10.line75_fraction.result,
                line80_difference = fixSubCalculation id $ difference line78_copy line79_copy,
                line81_copy = fixSubCalculation id $ page10.line71_half.result,
                line82_copy = page10.line77_difference,
                line83_difference = fixSubCalculation id $ difference line81_copy.result line82_copy,
                line84_half = fixSubCalculation (/ 2) page9.line45_fraction.result,
                line85_copy = line84_half.result,
                line86_conditionalCopy = if any (< 0) page10.line74_difference then abs <$> page10.line74_difference
                                         else Just 0,
                line87_difference = fixSubCalculation id $ nonNegativeDifference line85_copy line86_conditionalCopy,
                line88_sum = totalOf [line80_difference.result, line81_copy.result, line83_difference.result,
                                      line84_half.result, line87_difference.result]}}

schedule8Fields :: Schedule8 FieldConst
schedule8Fields = within "form1" Rank2.<$> Schedule8{
   page2 = within "Page2" . within "Part1" . within "Election-Evocation" Rank2.<$> Page2{
      line_50372_stopMonth = Field ["Line50372", "Month_grp", "Stop-Month"] Month,
      line_50374_revokeMonth = Field ["Line50374", "Month_grp", "Revoke-Month"] Month},
   page3 = within "Page3" . within "Part2" Rank2.<$> Page3{
      lineA_months = Field ["LineA", "Number"] Count,
      lineB_maxPensionableEarnings = Field ["LineB", "Amount"] Amount,
      lineC_maxSubjectToSecondAdditionalContributions = Field ["LineC", "Amount"] Amount,
      lineD_additionalMaxPensionableEarnings = Field ["LineD", "Amount"] Amount,
      lineE_maxBasicExemption = Field ["LineE", "Amount"] Amount},
   page4 = within "Page4" . within "Part3" Rank2.<$> Page4{
      line_50339_totalPensionableEarnings = Field ["Line1", "Amount"] Amount,
      line2_least = subCalculationFields "Line2" ["Amount1"] ["Amount"],
      line3_copyB = Field ["Line3", "Amount"] Amount,
      line4_difference = subCalculationFields "Line4" ["Amount1"] ["Amount"],
      line5_difference = Field ["Line5", "Amount"] Amount,
      line6_copyE = Field ["Line6", "Amount"] Amount,
      line7_difference = Field ["Line7", "Amount"] Amount,
      line_50340_totalContributions = Field ["Line8", "Amount"] Amount,
      line9_fraction = subCalculationFields "Line9" ["Amount1"] ["Amount2"],
      line10_difference = Field ["Line10", "Amount"] Amount,
      line11_fraction = subCalculationFields "Line11" ["Amount1"] ["Amount2"],
      line12_fraction = subCalculationFields "Line12" ["Amount1"] ["Amount2"],
      line13_sum = Field ["Line13", "Amount"] Amount,
      line14_copy = Field ["Line14", "Amount"] Amount,
      line15_copy = Field ["Line15", "Amount"] Amount,
      line16_difference = Field ["Line16", "Amount"] Amount,
      line17_copy = Field ["Line17", "Amount"] Amount,
      line18_copy = Field ["Line18", "Amount"] Amount,
      line19_difference = subCalculationFields "Line19" ["Amount1"] ["Amount2"],
      line20_sum = Field ["Line20", "Amount"] Amount,
      line_50341_totalSecondContributions = Field ["Line21", "Amount"] Amount,
      line22_fraction = subCalculationFields "Line22" ["Amount1"] ["Amount2"],
      line23_difference = subCalculationFields "Line23" ["Amount1"] ["Amount2"],
      line24_sum = Field ["Line24", "Amount"] Amount},
   page5 = within "Page5" . within "Part3_Cont" Rank2.<$> Page5{
      part3a = Page5Part3a{
         line25_copy = Field ["Line25", "Amount"] Amount,
         line26_copy = Field ["Line26", "Amount"] Amount,
         line27_copy = Field ["Line27", "Amount"] Amount,
         line28_sum = Field ["Line28", "Amount"] Amount,
         line29_copy = Field ["Line29", "Amount"] Amount},
      part3b = Page5Part3b{
         line30_conditionalCopy = Field ["Line30", "Amount"] Amount,
         line31_abs = Field ["Line31", "Amount"] Amount,
         line32_conditionalLeast = subCalculationFields "Line32" ["Amount"] ["Amount2"],
         line33_difference = Field ["Line33", "Amount"] Amount,
         line34_sum = Field ["Line34", "Amount"] Amount,
         line35_conditionalLeast = Field ["Line35", "Amount"] Amount,
         line36_sum = Field ["Line36", "Amount"] Amount}},
   page6 = within "Page6" Rank2.<$> Page6{
      part3 = within "Part3" Rank2.<$> Page6Part3{
         line37_conditionalCopy = Field ["Line37", "Amount"] Amount,
         line38_abs = Field ["Line38", "Amount"] Amount,
         line39_conditionalLeast = subCalculationFields "Line39" ["Amount"] ["Amount2"],
         line40_difference = Field ["Line40", "Amount1"] Amount,
         line41_sum = Field ["Line41", "Amount1"] Amount,
         line42_conditionalLeast = Field ["Line42", "Amount"] Amount,
         line43_sum = Field ["Line43", "Amount1"] Amount,
         line44_conditionalCopy = Field ["Line44", "Amount"] Amount,
         line45_abs = Field ["Line45", "Amount"] Amount,
         line46_conditionalLeast = Field ["Line46", "Amount"] Amount,
         line47_sum = subCalculationFields "Line47" ["Amount1"] ["Amount2"],
         line48_sum = Field ["Line48", "Amount"] Amount},
      part4 = within "Part4" Rank2.<$> Page6Part4{
         line1_netSelfEmploymentEarnings = Field ["Line1", "Amount"] Amount,
         line_50373_additionalEmploymentEarningsOffT4 = Field ["Line2", "Amount"] Amount,
         line3_sum = Field ["Line3", "Amount"] Amount,
         line4_least = subCalculationFields "Line4" ["Amount1"] ["Amount"],
         line5_copyB = Field ["Line5", "Amount"] Amount,
         line6_difference = subCalculationFields "Line6" ["Amount1"] ["Amount"],
         line7_difference = Field ["Line7", "Amount"] Amount,
         line8_copyE = Field ["Line8", "Amount"] Amount,
         line9_difference = Field ["Line9", "Amount"] Amount}},
   page7 = within "Page7" Rank2.<$> Page7{
      part4 = within "Part4" Rank2.<$> Page7Part4{
         line10_fraction = subCalculationFields "Line10" ["Amount1"] ["Amount2"],
         line11_fraction = subCalculationFields "Line11" ["Amount1"] ["Amount2"],
         line12_fraction = subCalculationFields "Line12" ["Amount1"] ["Amount2"],
         line13_sum = subCalculationFields "Line13" ["Amount1"] ["Amount"],
         line14_sum = Field ["Line14", "Amount"] Amount,
         line15_half = subCalculationFields "Line15" ["Amount1"] ["Amount2"],
         line16_copy = Field ["Line16", "Amount"] Amount,
         line17_sum = Field ["Line17", "amount"] Amount},
      part5 = within "Part5" Rank2.<$> Page7Part5{
         line1_netSelfEmploymentEarnings = Field ["Line1", "Amount"] Amount,
         line_50373_additionalEmploymentEarningsOffT4 = Field ["Line2", "Amount"] Amount,
         line_50399_additionalEmploymentEarningsOnT4 = Field ["Line3", "Amount"] Amount,
         line4_sum = Field ["Line4", "Amount"] Amount,
         line5_copy = Field ["Line5", "Amount"] Amount,
         line6_conditionalCopy = Field ["Line6", "Amount"] Amount,
         line7_difference = Field ["Line7", "Amount"] Amount,
         line8_abs = Field ["Line8", "Amount"] Amount,
         line9_min = Field ["Line9", "Amount"] Amount,
         line10_sum = Field ["Line10", "Amount1"] Amount}},
   page8 = within "Page8" . within "Part5" Rank2.<$> Page8{
      line11_copy = Field ["Line11", "Amount"] Amount,
      line12_copy = Field ["Line12", "Amount"] Amount,
      line13_difference = Field ["Line13", "Amount1"] Amount,
      line14to15 = Page7Cond2{
         line14_abs = Field ["Line14", "Amount"] Amount,
         line15_conditionalLeast = Field ["Line15", "Amount"] Amount},
      line16_sum = Field ["Line16", "Amount1"] Amount,
      line17_copyB = Field ["Line17", "Amount"] Amount,
      line18_copyE = Field ["Line18", "Amount"] Amount,
      line19_difference = Field ["Line19", "Amount"] Amount,
      line20_fraction = subCalculationFields "Line20" ["Amount1"] ["Amount"],
      line21_difference = Field ["Line21", "Amount"] Amount,
      line22_least = Field ["Line22", "Amount"] Amount,
      line23to29 = Page8Cond1{
         line23_copyE = Field ["Line23", "Amount"] Amount,
         line24_copy = Field ["Line24", "Amount"] Amount,
         line25_difference = Field ["Line25", "Amount"] Amount,
         line26_copy = Field ["Line26", "Amount"] Amount,
         line27_copy = Field ["Line27", "Amount"] Amount,
         line28_difference = subCalculationFields "Line28" ["Amount1"] ["Amount2"],
         line29_difference = subCalculationFields "Line29" ["Amount1"] ["Amount2"]},
      line30_difference = Field ["Line30", "Amount"] Amount,
      line31_copy = Field ["Line31", "Amount"] Amount,
      line32_copy = Field ["Line32", "Amount"] Amount,
      line33_sum = Field ["Line33", "Amount"] Amount,
      line34to41 = Page8Cond2{
         line34_copyC = Field ["Line34", "Amount"] Amount,
         line35_fraction = subCalculationFields "Line35" ["Amount1"] ["Amount"],
         line36_difference = Field ["Line36", "Amount"] Amount,
         line37_copy = Field ["Line37", "Amount"] Amount,
         line38_copy = Field ["Line38", "Amount"] Amount,
         line39_difference = Field ["Line39", "Amount"] Amount,
         line40_copy = Field ["Line40", "Amount"] Amount,
         line41_difference = Field ["Line41", "Amount"] Amount}},
   page9 = within "Page9" . within "Part5_Cont" Rank2.<$> Page9{
      line42_least = Field ["Line42", "Amount"] Amount,
      line43_fraction = subCalculationFields "Line43" ["Amount1"] ["Amount2"],
      line44_fraction = subCalculationFields "Line44" ["Amount1"] ["Amount2"],
      line45_fraction = subCalculationFields "Line45" ["Amount1"] ["Amount2"],
      line46_sum = subCalculationFields "Line46" ["Amount"] ["Amount2"],
      line47_sum = Field ["Line47", "Amount"] Amount,
      line48_double = subCalculationFields "Line48" ["Amount1"] ["Amount2"],
      line49_difference = Field ["Line49", "Amount"] Amount,
      line50_half = subCalculationFields "Line50" ["Amount1"] ["Amount"],
      line51_conditionalCopy = Field ["Line51", "Amount"] Amount,
      line52_abs = Field ["Line52", "Amount"] Amount,
      line53_conditionalLeast = subCalculationFields "Line53" ["Amount"] ["Amount2"],
      line54_difference = Field ["Line54", "Amount"] Amount,
      line55_sum = Field ["Line55", "Amount"] Amount,
      line56_conditionalLeast = Field ["Line56", "Amount"] Amount,
      line57_sum = Field ["Line57", "Amount"] Amount},
   page10 = within "Page10" . within "Part5_Cont" Rank2.<$> Page10{
      line58_conditionalCopy = Field ["Line58", "Amount"] Amount,
      line59_abs = Field ["Line59", "Amount"] Amount,
      line60_conditionalLeast = subCalculationFields "Line60" ["Amount"] ["Amount2"],
      line61_difference = Field ["Line61", "Amount1"] Amount,
      line62_sum = Field ["Line62", "Amount1"] Amount,
      line63_conditionalLeast = Field ["Line63", "Amount"] Amount,
      line64_sum = Field ["Line64", "Amount1"] Amount,
      line65_conditionalCopy = Field ["Line65", "Amount"] Amount,
      line66_abs = Field ["Line66", "Amount"] Amount,
      line67_conditionalLeast = Field ["Line67", "Amount"] Amount,
      line68_sum = subCalculationFields "Line68" ["Amount1"] ["Amount2"],
      line69_sum = Field ["Line69", "Amount"] Amount,
      line70_half = subCalculationFields "Line70" ["Amount1"] ["Amount2"],
      line71_half = subCalculationFields "Line71" ["Amount1"] ["Amount2"],
      line72_sum = Field ["Line72", "Amount2"] Amount,
      line73_positiveCopy = Field ["Line73", "Amount2"] Amount,
      line74_difference = Field ["Line74", "Amount"] Amount,
      line75_fraction = subCalculationFields "Line75" ["AmountA", "Amount1"] ["Amount2"],
      line76_copy = Field ["Line76", "Amount2"] Amount,
      line77_difference = Field ["Line77", "Amount"] Amount},
   page11 = within "Page11" . within "Part5-Cont" Rank2.<$> Page11{
      line78_copy = Field ["Line79", "Amount2"] Amount,
      line79_copy = Field ["Line80", "Amount2"] Amount,
      line80_difference = subCalculationFields "Line81" ["Amount1"] ["Amount"],
      line81_copy = subCalculationFields "Line82" ["Amount2"] ["Amount"],
      line82_copy = Field ["Line83", "Amount2"] Amount,
      line83_difference = subCalculationFields "Line84" ["Amount1"] ["Amount"],
      line84_half = subCalculationFields "Line85" ["Amount1"] ["Amount2"],
      line85_copy = Field ["Line86", "Amount2"] Amount,
      line86_conditionalCopy = Field ["Line87", "Amount2"] Amount,
      line87_difference = subCalculationFields "Line88" ["Amount1"] ["Amount"],
      line88_sum = Field ["Line89", "Amount"] Amount}}
