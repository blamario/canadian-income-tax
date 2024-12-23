{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

module Tax.Canada.Federal.Schedule11 where

import Data.Fixed (Centi)
import Data.Text (Text)
import Language.Haskell.TH qualified as TH
import Rank2 qualified
import Rank2.TH qualified
import Transformation.Shallow.TH qualified

import Tax.Canada.Shared (SubCalculation(result), fixSubCalculation, subCalculationFields)
import Tax.Canada.T1.Types (T1)
import Tax.Canada.T1.Types qualified
import Tax.FDF (Entry (Amount, Checkbox, Count), FieldConst (Field), within)
import Tax.Util (fixEq, fractionOf, difference, nonNegativeDifference, totalOf)

data Schedule11 line = Schedule11{
   page1 :: Page1 line,
   page2 :: Page2 line}

data Page1 line = Page1{
   line_32000_tuition :: line Centi,
   line2_copy :: line Centi,
   line2_fraction :: line Centi,
   line3_limit :: line Centi,
   line4_min :: line Centi,
   line5_trainingClaim :: line Centi,
   line6_difference :: line Centi,
   line_32001_eligible :: line Centi,
   line8_sum :: line Centi,
   line9_pastUnused :: line Centi,
   line10_sum :: line Centi,
   line11_copy :: line Centi,
   line11_numerator :: line Centi,
   line12_copy :: line Centi,
   line13_difference :: line Centi,
   line14_minUnused :: SubCalculation line,
   line15_difference :: line Centi,
   line16_min :: line Centi,
   line17_sum :: line Centi}

data Page2 line = Page2{
   line_32005_diability :: line Bool,
   line_32010_partTimeMonths :: line Word,
   line_32020_fullTimeMonths :: line Word,
   line18_copy :: line Centi,
   line19_copy :: line Centi,
   line20_difference :: line Centi,
   line21_copy :: line Centi,
   line22_copy :: line Centi,
   line23_difference :: line Centi,
   line24_transferred :: line Centi,
   line25_difference :: line Centi}

$(foldMap
   (\t-> concat <$> sequenceA [
       [d|
           deriving instance (Show (line Bool), Show (line Centi),
                              Show (line Rational), Show (line Word)) => Show ($(TH.conT t) line)
           deriving instance (Eq (line Bool), Eq (line Centi),
                              Eq (line Rational), Eq (line Word)) => Eq ($(TH.conT t) line)
       |],
       Rank2.TH.deriveAll t,
       Transformation.Shallow.TH.deriveAll t])
   [''Schedule11, ''Page1, ''Page2])

fixSchedule11 :: T1 Maybe -> Schedule11 Maybe -> Schedule11 Maybe
fixSchedule11 t1 = fixEq $ \Schedule11{page1 = page1@Page1{..}, page2 = page2@Page2{..}} -> Schedule11{
   page1 = page1{
      line2_copy = line_32000_tuition,
      line2_fraction = (0.5 *) <$> line2_copy,
      line4_min = minimum [line2_fraction, line3_limit],
      line6_difference = difference line_32000_tuition line5_trainingClaim,
      line8_sum = totalOf [line6_difference, line_32001_eligible],
      line10_sum = totalOf [line9_pastUnused, line8_sum],
      line11_copy = if taxableIncomeUnderThreshold then Nothing else t1.page7.partC_NetFederalTax.line118_copy,
      line11_numerator = if taxableIncomeUnderThreshold then taxableIncome else (/ 0.15) <$> line11_copy,
      line12_copy = t1.page6.line101_sum,
      line13_difference = nonNegativeDifference line11_numerator line12_copy,
      line14_minUnused = fixSubCalculation id $ minimum [line9_pastUnused, line13_difference],
      line15_difference = difference line13_difference line14_minUnused.result,
      line16_min = minimum [line8_sum, line15_difference],
      line17_sum = totalOf [line14_minUnused.result, line16_min]},
   page2 = page2{
      line18_copy = line10_sum,
      line19_copy = line17_sum,
      line20_difference = difference line18_copy line19_copy,
      line21_copy = min 5000 <$> line8_sum,
      line22_copy = line16_min,
      line23_difference = nonNegativeDifference line21_copy line22_copy,
      line25_difference = nonNegativeDifference line20_difference line24_transferred}}
   where taxableIncome = t1.page5.step4_TaxableIncome.line_26000_TaxableIncome
         taxableIncomeUnderThreshold = taxableIncome <= Just 50_197

schedule11Fields :: Schedule11 FieldConst
schedule11Fields = within "form1" Rank2.<$> Schedule11 {
   page1 = page1Fields,
   page2 = page2Fields}

page1Fields :: Page1 FieldConst
page1Fields = within "Page1"  . within "YourTuition" Rank2.<$> Page1 {
   line_32000_tuition = Field ["Line1", "Amount2"] Amount,
   line2_copy = Field ["Line2", "Amount1"] Amount,
   line2_fraction = Field ["Line2", "Amount2"] Amount,
   line3_limit = Field ["Line3", "Amount4"] Amount,
   line4_min = Field ["Line4", "Amount5"] Amount,
   line5_trainingClaim = Field ["Line5", "Amount6"] Amount,
   line6_difference = Field ["Line6", "Amount7"] Amount,
   line_32001_eligible = Field ["Line7", "Amount8"] Amount,
   line8_sum = Field ["Line8", "Amount1"] Amount,
   line9_pastUnused = Field ["Line9", "Amount1"] Amount,
   line10_sum = Field ["Line10", "Amount10"] Amount,
   line11_copy = Field ["Line11", "Line11", "Amount11"] Amount,
   line11_numerator = Field ["Line11", "Amount11"] Amount,
   line12_copy = Field ["Line12", "Amount12"] Amount,
   line13_difference = Field ["Line13", "Amount13"] Amount,
   line14_minUnused = subCalculationFields "Line14" ["Amount1"] ["Amount14"],
   line15_difference = Field ["Line15", "Amount15"] Amount,
   line16_min = Field ["Line16", "Amount16"] Amount,
   line17_sum = Field ["Line17", "Amount17"] Amount}

page2Fields :: Page2 FieldConst
page2Fields = within "Page2" Rank2.<$> Page2 {
   line_32005_diability = Field ["Enrolment", "Line32005", "CheckBox"] Checkbox,
   line_32010_partTimeMonths = Field ["Enrolment", "Line32010", "parttime_Months"] Count,
   line_32020_fullTimeMonths = Field ["Enrolment", "Line32020", "Fulltime_Months"] Count,
   line18_copy = Field ["Transfer_CF", "Line18", "Amount18"] Amount,
   line19_copy = Field ["Transfer_CF", "Line19", "Amount19"] Amount,
   line20_difference = Field ["Transfer_CF", "Line20", "Amount20"] Amount,
   line21_copy = Field ["Transfer_CF", "Line21", "Amount21"] Amount,
   line22_copy = Field ["Transfer_CF", "Line22", "Amount22"] Amount,
   line23_difference = Field ["Transfer_CF", "Line23", "Amount22"] Amount,
   line24_transferred = Field ["Transfer_CF", "Line24", "Amount24"] Amount,
   line25_difference = Field ["Transfer_CF", "Line25", "Amount25"] Amount}
