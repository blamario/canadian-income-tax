{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tax.Canada.Province.ON.ON479.Fix (ON479, fixON479) where

import Control.Applicative (liftA2)
import Control.Monad (guard, mfilter)
import Data.Fixed (Centi)
import Rank2 qualified

import Tax.Canada.Province.ON.ON479.Types
import Tax.Util (fixEq, fractionOf, nonNegativeDifference, totalOf)

fixON479 :: ON479 Maybe -> ON479 Maybe
fixON479 = fixEq $ \on479@ON479{..}-> ON479{page1 = fixPage1 page1,
                                            page2 = fixPage2 on479 page2}

fixPage1 :: Page1 Maybe -> Page1 Maybe
fixPage1 = fixEq $ \page@Page1{..}-> page{
   line_63052_fraction = (0.2 *) <$> line_63052_staycationCost,
   line_63055_fraction = (0.5 *) <$> line_63055_copy,
   line6_fraction = line5_allowable `fractionOf` line4_homecare_copy,
   line9_sum = totalOf [line7_netIncome_copy, line8_spouse_copy],
   line11_difference = nonNegativeDifference line9_sum line10_base,
   line13_fraction = line12_rate `fractionOf` line11_difference,
   line13_cont = line13_fraction,
   line_63095_difference = nonNegativeDifference line6_fraction line13_cont,
   line_63095_cont = line_63095_difference,
   line_63100_fraction = (0.15 *) <$> line_63100_transit,
   line16_sum = totalOf [line_63050_childcare, line_63052_fraction, line_63055_fraction, line_63095_cont, line_63100_fraction]}

fixPage2 :: ON479 Maybe -> Page2 Maybe -> Page2 Maybe
fixPage2 on479 = fixEq $ \page2@Page2{..}-> page2{
   line17_copy = on479.page1.line16_sum,
   line_63105_fraction = (0.25 *) <$> line_63105_renovation,
   line_63110_credit = if line_63110_contributions >= Just 3315 then Just 1457 else line_63110_credit, -- TODO worksheet?
   line_63220_fraction = (0.05 *) <$> line_63220_fromT1221,
   line23_credits = totalOf [line17_copy, line_63105_fraction, line_63110_credit, line_63220_fraction, line_63300_total]}
