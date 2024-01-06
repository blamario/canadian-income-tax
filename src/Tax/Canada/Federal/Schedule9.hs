{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Tax.Canada.Federal.Schedule9 where

import Data.Fixed (Centi)
import Data.Text (Text)
import Rank2 qualified
import Rank2.TH qualified
import Transformation.Shallow.TH qualified

import Tax.Canada.T1.Types (T1)
import Tax.Canada.T1.Types qualified
import Tax.FDF (Entry (Amount), FieldConst (Field), within)
import Tax.Util (fixEq, fractionOf, difference, nonNegativeDifference, totalOf)

data Schedule9 line = Schedule9{
   line1_charities :: line Centi,
   line_32900_government :: line Centi,
   line_33300_universities :: line Centi,
   line_33400_UN :: line Centi,
   line5_sum :: line Centi,
   line6_copy :: line Centi,
   line6_fraction :: line Centi,
   line_33700_depreciable :: line Centi,
   line_33900_capital :: line Centi,
   line7_sum :: line Centi,
   line7_fraction :: line Centi,
   line8_sum :: line Centi,
   line9_limit :: line Centi,
   line_34000_allowable :: line Centi,
   line_34200_ecocultural :: line Centi,
   line12_sum :: line Centi,
   line13_min :: line Centi,
   line14_difference :: line Centi,
   line_34210_ecological :: line Centi,
   line16_difference :: line Centi,
   line17_copy :: line Centi,
   line18_threshold :: line Centi,
   line19_difference :: line Centi,
   lineE_copy :: line Centi,
   line20_min :: line Centi,
   line20_fraction :: line Centi,
   line21_difference :: line Centi,
   line21_fraction :: line Centi,
   line22_copy :: line Centi,
   line22_fraction :: line Centi,
   line23_sum :: line Centi}

deriving instance Show (line Centi) => Show (Schedule9 line)
deriving instance Eq (line Centi) => Eq (Schedule9 line)

Rank2.TH.deriveAll ''Schedule9
Transformation.Shallow.TH.deriveAll ''Schedule9

fixSchedule9 :: T1 Maybe -> Schedule9 Maybe -> Schedule9 Maybe
fixSchedule9 t1 = fixEq $ \form@Schedule9{..} -> form{
   line5_sum = totalOf [line1_charities, line_32900_government, line_33300_universities, line_33400_UN],
   line6_copy = t1.page4.line_23600_NetIncome,
   line6_fraction = (0.75 *) <$> line6_copy,
   line7_sum = totalOf [line_33700_depreciable, line_33900_capital],
   line7_fraction = (0.25 *) <$> line7_sum,
   line8_sum = totalOf [line6_fraction, line7_fraction],
   line9_limit = minimum [line6_copy, line8_sum],
   line_34000_allowable = minimum [line5_sum, line9_limit],
   line12_sum = totalOf [line_34000_allowable, line_34200_ecocultural],
   line13_min = minimum [line12_sum, Just 200],
   line14_difference = difference line12_sum line13_min,
   line16_difference = nonNegativeDifference line14_difference line_34210_ecological,
   line17_copy = t1.page5.step4_TaxableIncome.line_26000_TaxableIncome,
   line19_difference = nonNegativeDifference line17_copy line18_threshold,
   lineE_copy = line14_difference,
   line20_min = minimum [line16_difference, line19_difference],
   line20_fraction = (0.33 *) <$> line20_min,
   line21_difference = difference lineE_copy line20_min,
   line21_fraction = (0.29 *) <$> line21_difference,
   line22_copy = line13_min,
   line22_fraction = (0.15 *) <$> line22_copy,
   line23_sum = totalOf [line20_fraction, line21_fraction, line22_fraction]}

schedule9Fields :: Schedule9 FieldConst
schedule9Fields = within "form1" . within "Page1" Rank2.<$> Schedule9 {
   line1_charities = Field ["Line1", "Amount"] Amount,
   line_32900_government = Field ["Line2", "Amount"] Amount,
   line_33300_universities = Field ["Line3", "Amount"] Amount,
   line_33400_UN = Field ["Line4", "Amount"] Amount,
   line5_sum = Field ["Line5", "Amount"] Amount,
   line6_copy = Field ["Line6", "AmountA", "Amount"] Amount,
   line6_fraction = Field ["Line6", "Amount"] Amount,
   line_33700_depreciable = Field ["AmountB", "Amount"] Amount,
   line_33900_capital = Field ["AmountC", "Amount"] Amount,
   line7_sum = Field ["Line7", "AmountD", "Amount"] Amount,
   line7_fraction = Field ["Line7", "Amount"] Amount,
   line8_sum = Field ["Line8", "Amount"] Amount,
   line9_limit = Field ["Line9", "Amount"] Amount,
   line_34000_allowable = Field ["Line10", "Amount"] Amount,
   line_34200_ecocultural = Field ["Line11", "Amount"] Amount,
   line12_sum = Field ["Line12", "Amount"] Amount,
   line13_min = Field ["Line13", "Amount_Line13"] Amount,
   line14_difference = Field ["Line14", "Amount_Line14"] Amount,
   line_34210_ecological = Field ["Line15", "Amount"] Amount,
   line16_difference = Field ["Line16", "Amount_Line16"] Amount,
   line17_copy = Field ["Line17", "Amount"] Amount,
   line18_threshold = Field ["Line18", "Amount"] Amount,
   line19_difference = Field ["Line19", "Amount"] Amount,
   lineE_copy = Field ["AmountE", "Amount_Line14"] Amount,
   line20_min = Field ["Line20", "AmountF", "Amount"] Amount,
   line20_fraction = Field ["Line20", "Amount"] Amount,
   line21_difference = Field ["Line21", "AmountG", "Amount"] Amount,
   line21_fraction = Field ["Line21", "Amount"] Amount,
   line22_copy = Field ["Line22", "AmountH", "Amount_Line14"] Amount,
   line22_fraction = Field ["Line22", "Amount"] Amount,
   line23_sum = Field ["Line23", "Amount"] Amount}

