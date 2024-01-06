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

import Tax.Canada.T1.Types (T1)
import Tax.Canada.T1.Types qualified
import Tax.FDF (Entry (Amount, Checkbox, Count), FieldConst (Field), within)
import Tax.Util (fixEq, fractionOf, difference, nonNegativeDifference, totalOf)

data Schedule11 line = Schedule11{
   page1 :: Page1 line,
   page2 :: Page2 line}

data Page1 line = Page1{
   line1_pastUnused :: line Centi,
   line_32000_tuition :: line Centi,
   line3_copy :: line Centi,
   line3_fraction :: line Centi,
   line4_limit :: line Centi,
   line5_min :: line Centi,
   line6_trainingClaim :: line Centi,
   line7_difference :: line Centi,
   line_32001_eligible :: line Centi,
   line9_sum :: line Centi,
   line9_cont :: line Centi,
   line10_sum :: line Centi,
   line11_copy :: line Centi,
   line11_numerator :: line Centi,
   line12_copy :: line Centi,
   line13_difference :: line Centi,
   line14_minUnused :: line Centi,
   line14_cont :: line Centi,
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
           deriving instance (Show (line Bool), Show (line Centi), Show (line Word)) => Show ($(TH.conT t) line)
           deriving instance (Eq (line Bool), Eq (line Centi), Eq (line Word)) => Eq ($(TH.conT t) line)
       |],
       Rank2.TH.deriveAll t,
       Transformation.Shallow.TH.deriveAll t])
   [''Schedule11, ''Page1, ''Page2])

fixSchedule11 :: T1 Maybe -> Schedule11 Maybe -> Schedule11 Maybe
fixSchedule11 t1 = fixEq $ \Schedule11{page1 = page1@Page1{..}, page2 = page2@Page2{..}} -> Schedule11{
   page1 = page1{
      line3_copy = line_32000_tuition,
      line3_fraction = (0.5 *) <$> line3_copy,
      line5_min = minimum [line3_fraction, line4_limit],
      line7_difference = difference line_32000_tuition line6_trainingClaim,
      line9_sum = totalOf [line7_difference, line_32001_eligible],
      line9_cont = line9_sum,
      line10_sum = totalOf [line1_pastUnused, line9_cont],
      line11_copy = if taxableIncomeUnderThreshold then Nothing else t1.page7.partC_NetFederalTax.line116,
      line11_numerator = if taxableIncomeUnderThreshold then taxableIncome else (/ 0.15) <$> line11_copy,
      line12_copy = t1.page6.line99,
      line13_difference = nonNegativeDifference line11_numerator line12_copy,
      line14_minUnused = minimum [line1_pastUnused, line13_difference],
      line14_cont = line14_minUnused,
      line15_difference = difference line13_difference line14_minUnused,
      line16_min = minimum [line9_cont, line15_difference],
      line17_sum = totalOf [line14_cont, line16_min]},
   page2 = page2{
      line18_copy = line10_sum,
      line19_copy = line17_sum,
      line20_difference = difference line18_copy line19_copy,
      line21_copy = line9_cont,
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
page1Fields = within "Page1" Rank2.<$> Page1 {
   line1_pastUnused = Field ["Line1", "Amount1"] Amount,
   line_32000_tuition = Field ["Line2", "Amount2"] Amount,
   line3_copy = Field ["Line3", "Amount1"] Amount,
   line3_fraction = Field ["Line3", "Amount2"] Amount,
   line4_limit = Field ["Line4", "Amount4"] Amount,
   line5_min = Field ["Line5", "Amount5"] Amount,
   line6_trainingClaim = Field ["Line6", "Amount6"] Amount,
   line7_difference = Field ["Line7", "Amount7"] Amount,
   line_32001_eligible = Field ["Line8", "Amount8"] Amount,
   line9_sum = Field ["Line9", "Amount1"] Amount,
   line9_cont = Field ["Line9", "Amount9"] Amount,
   line10_sum = Field ["Line10", "Amount10"] Amount,
   line11_copy = Field ["Line11", "Line11", "Amount11"] Amount,
   line11_numerator = Field ["Line11", "Amount11"] Amount,
   line12_copy = Field ["Line12", "Amount12"] Amount,
   line13_difference = Field ["Line13", "Amount13"] Amount,
   line14_minUnused = Field ["Line14", "Amount1"] Amount,
   line14_cont = Field ["Line14", "Amount14"] Amount,
   line15_difference = Field ["Line15", "Amount15"] Amount,
   line16_min = Field ["Line16", "Amount16"] Amount,
   line17_sum = Field ["Line17", "Amount17"] Amount}

page2Fields :: Page2 FieldConst
page2Fields = within "Page2" Rank2.<$> Page2 {
   line_32005_diability = Field ["Enrolment", "Line32005", "CheckBox"] Checkbox,
   line_32010_partTimeMonths = Field ["Enrolment", "Line32010", "parttime_Months"] Count,
   line_32020_fullTimeMonths = Field ["Enrolment", "Line32020", "Fulltime_Months"] Count,
   line18_copy = Field ["Line18", "Amount18"] Amount,
   line19_copy = Field ["Line19", "Amount19"] Amount,
   line20_difference = Field ["Line20", "Amount20"] Amount,
   line21_copy = Field ["Line21", "Amount21"] Amount,
   line22_copy = Field ["Line22", "Amount22"] Amount,
   line23_difference = Field ["Line23", "Amount22"] Amount,
   line24_transferred = Field ["Line24", "Amount24"] Amount,
   line25_difference = Field ["Line25", "Amount25"] Amount}
