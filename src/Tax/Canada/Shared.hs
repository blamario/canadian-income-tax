{-# LANGUAGE DuplicateRecordFields #-}
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

import Control.Monad (guard, mfilter)
import Data.Fixed (Centi)
import Language.Haskell.TH qualified as TH
import Rank2.TH qualified
import Transformation.Shallow.TH qualified

import Tax.Util (fixEq, fractionOf, nonNegativeDifference)

data TaxIncomeBracket line = TaxIncomeBracket {
   income :: line Centi,
   threshold :: line Centi,
   overThreshold :: line Centi,
   rate :: line Rational,
   timesRate :: line Centi,
   baseTax :: line Centi,
   equalsTax :: line Centi}

data MedicalExpenses line = MedicalExpenses {
   expenses :: line Centi,
   netIncome :: line Centi,
   incomeRate :: line Rational,
   fraction :: line Centi,
   lesser :: line Centi,
   difference :: line Centi}

data BaseCredit line = BaseCredit {
   baseAmount :: line Centi,
   reduction :: line Centi,
   difference :: line Centi,
   cont :: line Centi}

$(foldMap
   (\t-> concat <$> sequenceA [
       [d|
           deriving instance (Show (line Centi), Show (line Rational), Show (line Word))
                          => Show ($(TH.conT t) line)
           deriving instance (Eq (line Centi), Eq (line Rational), Eq (line Word))
                          => Eq ($(TH.conT t) line)
       |],
       Rank2.TH.deriveAll t,
       Transformation.Shallow.TH.deriveAll t])
   [''BaseCredit, ''MedicalExpenses, ''TaxIncomeBracket])

fixTaxIncomeBracket :: Maybe Centi -> Maybe (TaxIncomeBracket Maybe) -> TaxIncomeBracket Maybe -> TaxIncomeBracket Maybe
fixTaxIncomeBracket theIncome nextBracket = fixEq $ \bracket@TaxIncomeBracket{..} -> bracket{
   income = do i <- theIncome
               floor <- threshold
               let ceiling = nextBracket >>= (.threshold)
               guard (floor <= i && all (i <) ceiling)
               Just i,
   overThreshold = liftA2 (-) income threshold,
   timesRate = fromRational <$> liftA2 (*) (toRational <$> overThreshold) rate,
   equalsTax = liftA2 (+) timesRate baseTax}

fixBaseCredit :: BaseCredit Maybe -> BaseCredit Maybe
fixBaseCredit = fixEq $ \credit@BaseCredit{..}-> credit{
   difference = mfilter (> 0) $ liftA2 (-) baseAmount reduction,
   cont = difference}

fixMedicalExpenses :: Centi -> MedicalExpenses Maybe -> MedicalExpenses Maybe
fixMedicalExpenses ceiling = fixEq $ \part@MedicalExpenses{..} -> part{
   fraction = incomeRate `fractionOf` netIncome,
   lesser = min ceiling <$> fraction,
   difference = nonNegativeDifference expenses lesser}
