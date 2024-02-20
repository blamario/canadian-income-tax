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

module Tax.Canada.Federal.Schedule7 where

import Data.Fixed (Centi)
import Data.Text (Text)
import Language.Haskell.TH qualified as TH
import Rank2 qualified
import Rank2.TH qualified
import Transformation.Shallow.TH qualified

import Tax.Canada.Shared (SubCalculation(SubCalculation, result), fixSubCalculation, subCalculationFields)
import Tax.Canada.T1.Types (T1)
import Tax.Canada.T1.Types qualified as T1
import Tax.FDF (Entry (Amount, Checkbox), FieldConst (Field), within)
import Tax.Util (fixEq, fractionOf, difference, nonNegativeDifference, totalOf)

data Schedule7 line = Schedule7{
   page2 :: Page2 line,
   page3 :: Page3 line,
   page4 :: Page4 line}

data Page2 line = Page2{
   line1_pastUnused :: line Centi,
   line2_pastYearContributions :: line Centi,
   line3_thisYearContributions :: line Centi,
   line_24500_contributions_sum :: SubCalculation line,
   line5_sum :: line Centi}

data Page3 line = Page3{
   partB :: PartB line,
   partC :: PartC line}

data PartB line = PartB{
   line6_contributions_copy :: line Centi,
   line_24600_hbp :: line Centi,
   line_24620_llp :: line Centi,
   line9_repayments_sum :: SubCalculation line,
   line10_difference :: line Centi}

data PartC line = PartC{
   line11_deductionLimit :: line Centi,
   line12_prpp_copy :: line Centi,
   line13_difference :: line Centi,
   line14_copy :: line Centi,
   line_24640_transfers :: line Centi,
   line15_cont :: line Centi,
   line16_difference :: line Centi,
   line17_lesser :: line Centi,
   line18_deducting :: line Centi,
   line19_sum :: line Centi,
   line20_deduction :: line Centi}

data Page4 line = Page4{
   partD :: PartD line,
   partE :: PartE line,
   line_26700_athleteTrust :: line Centi}

data PartD line = PartD{
   line21_copy :: line Centi,
   line22_copy :: line Centi,
   line23_difference :: line Centi}

data PartE line = PartE{
   line_24700_hbp :: line Centi,
   line_25900_hbpSame :: line Bool,
   line_26300_llp :: line Centi,
   line_26400_llpSpouse :: line Bool}

$(foldMap
   (\t-> concat <$> sequenceA [
       [d|
           deriving instance (Show (line Bool), Show (line Centi), Show (line Rational)) => Show ($(TH.conT t) line)
           deriving instance (Eq (line Bool), Eq (line Centi), Eq (line Rational)) => Eq ($(TH.conT t) line)
       |],
       Rank2.TH.deriveAll t,
       Transformation.Shallow.TH.deriveAll t])
   [''Schedule7, ''Page2, ''Page3, ''Page4, ''PartB, ''PartC, ''PartD, ''PartE])

fixSchedule7 :: T1 Maybe -> Schedule7 Maybe -> Schedule7 Maybe
fixSchedule7 t1  = fixEq $ \Schedule7{page2, page3, page4} -> Schedule7{
   page2 = let Page2{..} = page2 in page2{
      line_24500_contributions_sum = fixSubCalculation $ totalOf [line2_pastYearContributions, line3_thisYearContributions],
      line5_sum = totalOf [line1_pastUnused, line_24500_contributions_sum.result]},
   page3 = let Page3{partB = partB@PartB{..}, partC = partC@PartC{..}} = page3 in Page3{
      partB = partB{
         line6_contributions_copy = page2.line5_sum,
         line9_repayments_sum = fixSubCalculation $ totalOf [line_24600_hbp, line_24620_llp],
         line10_difference = difference line6_contributions_copy line9_repayments_sum.result},
      partC = partC{
         line12_prpp_copy = t1.page4.line_20810_PRPP,
         line13_difference = difference line11_deductionLimit line12_prpp_copy,
         line14_copy = partB.line10_difference,
         line15_cont = line_24640_transfers,
         line16_difference = difference line14_copy line15_cont,
         line17_lesser = min line13_difference line16_difference,
         line19_sum = totalOf [line15_cont, line18_deducting],
         line20_deduction = min line10_difference line19_sum}},
   page4 = page4{
      partD = let PartD{line21_copy, line22_copy} = page4.partD in PartD{
         line21_copy = page3.partB.line10_difference,
         line22_copy = page3.partC.line20_deduction,
         line23_difference = difference line21_copy line22_copy}}}

schedule7Fields :: Schedule7 FieldConst
schedule7Fields = within "form1" Rank2.<$> Schedule7{
   page2 = within "Page2" . within "PartA" Rank2.<$> Page2{
      line1_pastUnused = Field ["Line1", "Amount"] Amount,
      line2_pastYearContributions = Field ["Line2", "Amount"] Amount,
      line3_thisYearContributions = Field ["Line3", "Amount"] Amount,
      line_24500_contributions_sum = subCalculationFields "Line4" ["Amount1"] ["Amount2"],
      line5_sum = Field ["Line5", "Amount"] Amount},
   page3 = within "Page3" Rank2.<$> Page3{
      partB = within "PartB" Rank2.<$> PartB{
         line6_contributions_copy = Field ["Line6", "Amount"] Amount,
         line_24600_hbp = Field ["Line7", "Amount"] Amount,
         line_24620_llp = Field ["Line8", "Amount"] Amount,
         line9_repayments_sum = subCalculationFields "Line9" ["Amount1"] ["Amount2"],
         line10_difference = Field ["Line10", "Amount"] Amount},
      partC = within "PartC" Rank2.<$> PartC{
         line11_deductionLimit = Field ["Line11", "Amount"] Amount,
         line12_prpp_copy = Field ["Line12", "Amount"] Amount,
         line13_difference = Field ["Line13", "Amount"] Amount,
         line14_copy = Field ["Line14", "Amount"] Amount,
         line_24640_transfers = Field ["Line15", "Amount1"] Amount,
         line15_cont = Field ["Line15", "Amount2"] Amount,
         line16_difference = Field ["Line16", "Amount"] Amount,
         line17_lesser = Field ["Line17", "Amount"] Amount,
         line18_deducting = Field ["Line18", "Amount"] Amount,
         line19_sum = Field ["Line19", "Amount"] Amount,
         line20_deduction = Field ["Line20", "Amount"] Amount}},
   page4 = within "Page4" Rank2.<$> Page4{
      partD = within "PartD" Rank2.<$> PartD{
         line21_copy = Field ["Line22", "Amount"] Amount,
         line22_copy = Field ["Line23", "Amount"] Amount,
         line23_difference = Field ["Line24", "Amount"] Amount},
      partE = within "PartE" Rank2.<$> PartE{
         line_24700_hbp = Field ["Line25", "Amount"] Amount,
         line_25900_hbpSame = Field ["Line26", "Box_Line25"] Checkbox,
         line_26300_llp = Field ["Line27", "Amount"] Amount,
         line_26400_llpSpouse = Field ["Line28", "Box_Line27"] Checkbox},
      line_26700_athleteTrust = Field ["PartF", "Line29", "Amount"] Amount}}
