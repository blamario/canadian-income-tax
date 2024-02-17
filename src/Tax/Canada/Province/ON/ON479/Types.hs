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

module Tax.Canada.Province.ON.ON479.Types where

import Data.Fixed (Centi)
import Data.Text (Text)
import Language.Haskell.TH qualified as TH
import Rank2.TH qualified
import Transformation.Shallow.TH qualified

import Tax.Canada.Shared (SubCalculation)

data ON479 line = ON479 {
   page1 :: Page1 line,
   page2 :: Page2 line}

data Page1 line = Page1 {
   line_63050_childcare :: line Centi,
   line4_homecare_copy :: line Centi,
   line5_allowable :: line Rational,
   line6_fraction :: line Centi,
   line7_netIncome_copy :: line Centi,
   line8_spouse_copy :: line Centi,
   line9_sum :: line Centi,
   line10_base :: line Centi,
   line11_difference :: line Centi,
   line12_rate :: line Rational,
   line13_fraction :: SubCalculation line,
   line_63095_difference :: SubCalculation line,
   line_63100_transit :: line Centi,
   line_63100_fraction :: line Centi,
   line16_sum :: line Centi}

data Page2 line = Page2 {
   line17_copy :: line Centi,
   line_63110_contributions :: line Centi,
   line_63110_credit :: line Centi,
   line_63220_fromT1221 :: line Centi,
   line_63220_fraction :: line Centi,
   line_63260_placements :: line Word,
   line_63265_partnership :: line Bool,
   line_63270_business :: line Text,
   line_63300_total :: line Centi,
   line23_credits :: line Centi}

$(foldMap
   (\t-> concat <$> sequenceA [
       [d|
           deriving instance (Show (line Bool), Show (line Centi), Show (line Rational), Show (line Text), Show (line Word))
                          => Show ($(TH.conT t) line)
           deriving instance (Eq (line Bool), Eq (line Centi), Eq (line Rational), Eq (line Text), Eq (line Word))
                          => Eq ($(TH.conT t) line)
       |],
       Rank2.TH.deriveAll t,
       Transformation.Shallow.TH.deriveAll t])
   [''ON479, ''Page1, ''Page2])
