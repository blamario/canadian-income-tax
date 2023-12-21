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

data BC479 line = BC479 {
   page1 :: Page1 line,
   page2 :: Page2 line}

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
   line12_copy :: line Centi,
   line12_fraction :: line Centi,
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
   line22_sum :: line Centi,
   line22_cont :: line Centi,
   line_60510_fromT88 :: line Centi,
   line_60530_fromT88 :: line Centi,
   line_60550_training :: line Centi,
   line_60560_training :: line Centi,
   line_60570_ships :: line Centi,
   line28_sum :: line Centi,
   line28_cont :: line Centi,
   line29_credits :: line Centi}

$(foldMap
   (\t-> concat <$> sequenceA [
       [d|
           deriving instance (Show (line Bool), Show (line Centi), Show (line Text)) => Show ($(TH.conT t) line)
           deriving instance (Eq (line Bool), Eq (line Centi), Eq (line Text)) => Eq ($(TH.conT t) line)
       |],
       Rank2.TH.deriveAll t,
       Transformation.Shallow.TH.deriveAll t])
   [''BC479, ''Page1, ''Page2])
