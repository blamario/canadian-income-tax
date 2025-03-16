{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tax.Canada.Province.ON.ON479.Fix (ON479, fixON479) where

import Tax.Canada.Province.ON.ON479.Types
import Tax.Canada.Shared (fixSubCalculation, SubCalculation(result))
import Tax.Util (fixEq, fractionOf, nonNegativeDifference, totalOf)

fixON479 :: ON479 Maybe -> ON479 Maybe
fixON479 = fixEq $ \on479@ON479{..}-> ON479{page1 = fixPage1 page1,
                                            page2 = fixPage2 on479 page2}

fixPage1 :: Page1 Maybe -> Page1 Maybe
fixPage1 = fixEq $ \page@Page1{..}-> page{
   line6_fraction = line5_allowable `fractionOf` line4_homecare_copy,
   line9_sum = totalOf [line7_netIncome_copy, line8_spouse_copy],
   line11_difference = nonNegativeDifference line9_sum line10_base,
   line13_fraction = fixSubCalculation id $ line12_rate `fractionOf` line11_difference,
   line_63095_difference = fixSubCalculation id $ nonNegativeDifference line6_fraction line13_fraction.result,
   line_63100_fraction = (0.15 *) <$> line_63100_transit,
   line16_sum = totalOf [line_63050_childcare, line_63095_difference.result, line_63100_fraction]}

fixPage2 :: ON479 Maybe -> Page2 Maybe -> Page2 Maybe
fixPage2 on479 = fixEq $ \page2@Page2{..}-> page2{
   line17_copy = on479.page1.line16_sum,
   line_63110_credit = if line_63110_contributions >= Just 3691 then Just 1622.08 else line_63110_credit, -- TODO worksheet?
   line_63220_fraction = (0.05 *) <$> line_63220_fromT1221,
   line23_credits = totalOf [line17_copy, line_63110_credit, line_63220_fraction, line_63300_total]}
