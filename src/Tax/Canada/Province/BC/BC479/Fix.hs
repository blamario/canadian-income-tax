{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Tax.Canada.Province.BC.BC479.Fix (BC479, fixBC479) where

import Tax.Canada.Province.BC.BC479.Types
import Tax.Util (fixEq, nonNegativeDifference, totalOf)

fixBC479 :: BC479 Maybe -> BC479 Maybe
fixBC479 = fixEq $ \bc479@BC479{..}-> BC479{page1 = fixPage1 page1,
                                            page2 = fixPage2 bc479 page2}

fixPage1 :: Page1 Maybe -> Page1 Maybe
fixPage1 = fixEq $ \page@Page1{..}-> page{
   line3_sum_self = totalOf [line1_netIncome_self, line2_uccb_rdsp_repayment_self],
   line3_sum_spouse = totalOf [line1_netIncome_spouse, line2_uccb_rdsp_repayment_spouse],
   line5_difference_self = nonNegativeDifference line3_sum_self line4_uccb_rdsp_income_self,
   line5_difference_spouse = nonNegativeDifference line3_sum_spouse line4_uccb_rdsp_income_spouse,
   line6_sum = totalOf [line5_difference_self, line5_difference_spouse],
   line8_difference = nonNegativeDifference line6_sum line7_threshold,
   line_60330_sales = Just 75,
   line11_sum = totalOf [line_60330_sales, line_60350_spouse],
   line12_copy = line8_difference,
   line12_fraction = (0.02 *) <$> line8_difference,
   line13_difference = nonNegativeDifference line11_sum line12_fraction,
   line14_fraction = (0.1 *) <$> line_60480_renovation,
   line15_sum = totalOf [line13_difference, line14_fraction]}

fixPage2 :: BC479 Maybe -> Page2 Maybe -> Page2 Maybe
fixPage2 bc479 = fixEq $ \page2@Page2{..}-> page2{
   line16_copy = bc479.page1.line15_sum,
   line22_sum = totalOf [line17_venture, line_60490_shares, line_60495_shares],
   line22_cont = line22_sum,
   line28_sum = totalOf [line_60550_training, line_60560_training, line_60570_ships],
   line28_cont = line28_sum,
   line29_credits = totalOf [line16_copy, line22_cont, line_60510_fromT88, line28_cont]}