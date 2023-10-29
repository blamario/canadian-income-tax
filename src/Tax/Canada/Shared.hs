{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The T1 form type, currently verified only for Ontario. Hopefully it covers the other provinces' T1s as well.
module Tax.Canada.Shared where

import Control.Monad (guard)
import Data.Fixed (Centi)
import Rank2.TH qualified
import Transformation.Shallow.TH qualified

data TaxIncomeBracket line = TaxIncomeBracket {
   income :: line Centi,
   threshold :: line Centi,
   overThreshold :: line Centi,
   rate :: line Rational,
   timesRate :: line Centi,
   baseTax :: line Centi,
   equalsTax :: line Centi}

fixTaxIncomeBracket :: Maybe Centi -> Maybe (TaxIncomeBracket Maybe) -> TaxIncomeBracket Maybe -> TaxIncomeBracket Maybe
fixTaxIncomeBracket theIncome nextBracket bracket@TaxIncomeBracket{..} = bracket{
   income = do i <- theIncome
               floor <- threshold
               let ceiling = nextBracket >>= (.threshold)
               guard (floor <= i && all (i <) ceiling)
               Just i,
   overThreshold = liftA2 (-) income threshold,
   timesRate = fromRational <$> liftA2 (*) (toRational <$> overThreshold) rate,
   equalsTax = liftA2 (+) timesRate baseTax}

deriving instance (Show (line Centi), Show (line Rational)) => Show (TaxIncomeBracket line)
deriving instance (Eq (line Centi), Eq (line Rational)) => Eq (TaxIncomeBracket line)
Rank2.TH.deriveAll ''TaxIncomeBracket
Transformation.Shallow.TH.deriveAll ''TaxIncomeBracket
