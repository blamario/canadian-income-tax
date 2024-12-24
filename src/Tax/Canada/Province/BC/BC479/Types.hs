{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Tax.Canada.Province.BC.BC479.Types where

import Data.Fixed (Centi)
import Data.Text (Text)
import Language.Haskell.TH qualified as TH
import Rank2.TH qualified
import Transformation.Shallow.TH qualified

import Tax.Canada.Shared (SubCalculation)

data BC479 line = BC479 {
   page1 :: Page1 line,
   page2 :: Page2 line,
   page3 :: Page3 line}

data Page1 line = Page1 {
   line1_netIncome_self :: line Centi,
   line1_netIncome_spouse :: line Centi,
   line2_uccb_rdsp_repayment_self :: line Centi,
   line2_uccb_rdsp_repayment_spouse :: line Centi,
   line3_sum_self :: line Centi,
   line3_sum_spouse :: line Centi,
   line4_uccb_rdsp_income_self :: line Centi,
   line4_uccb_rdsp_income_spouse :: line Centi,
   line5_difference_self :: line Centi,
   line5_difference_spouse :: line Centi,
   line6_sum :: line Centi,
   line7_threshold :: line Centi,
   line8_difference :: line Centi,
   line_60330_sales :: line Centi,
   line_60350_spouse :: line Centi,
   line11_sum :: line Centi,
   line12_fraction :: SubCalculation line,
   line13_difference :: line Centi,
   line_60890_separate :: line Bool,
   line_60480_renovation :: line Centi,
   line14_fraction :: line Centi,
   line15_sum :: line Centi}

data Page2 line = Page2 {
   line16_copy :: line Centi,
   line17_venture :: line Centi,
   line_60490_shares :: line Centi,
   line_60491_certificate :: line Text,
   line_60495_shares :: line Centi,
   line_60496_certificate :: line Text,
   line22_sum :: SubCalculation line,
   line_60510_fromT88 :: line Centi,
   line_60530_fromT88 :: line Centi,
   line_60545_buildings :: line Centi,
   line_60546_partnership :: line Centi,
   line27_sum :: SubCalculation line,
   line_60550_training :: line Centi,
   line_60560_training :: line Centi,
   line_60570_ships :: line Centi,
   line31_sum :: SubCalculation line,
   line32_credits :: line Centi}

data Page3 line = Page3 {
   line33_copy :: line Centi,
   tenancy_months1 :: line Word,
   tenancy_months2 :: line Word,
   rent_paid1 :: line Centi,
   rent_paid2 :: line Centi,
   line_60575_sum :: line Word,
   line35_ceiling :: line Centi,
   line36_income_copy :: line Centi,
   line37_threshold :: line Centi,
   line38_difference :: line Centi,
   line39_rate :: line Rational,
   line40_fraction :: SubCalculation line,
   line_60576_difference :: SubCalculation line,
   line42_credits :: line Centi}

$(foldMap
   (\t-> concat <$> sequenceA [
       [d|
           deriving instance (Show (line Bool), Show (line Centi), Show (line Word),
                              Show (line Rational), Show (line Text)) => Show ($(TH.conT t) line)
           deriving instance (Eq (line Bool), Eq (line Centi), Eq (line Word),
                              Eq (line Rational), Eq (line Text)) => Eq ($(TH.conT t) line)
       |],
       Rank2.TH.deriveAll t,
       Transformation.Shallow.TH.deriveAll t])
   [''BC479, ''Page1, ''Page2, ''Page3])
