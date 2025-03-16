{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Transformations where

import Data.Functor.Compose
import Data.Void (Void)
import Data.Text (Text)
import Data.Time (Day)
import Data.Time.Calendar (fromGregorian)
import Data.CAProvinceCodes qualified as Province

import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Transformation (Transformation (Domain, Codomain), At)
import Transformation qualified

import Tax.FDF (FieldConst (Field, NoField), Entry (Constant))
import Tax.Canada.T1 (LanguageOfCorrespondence, MaritalStatus)
import Data.Fixed (Centi, Fixed (MkFixed))

data Gen = Gen

instance Transformation Gen where
  type Domain Gen = FieldConst
  type Codomain Gen = Compose Hedgehog.Gen Maybe

adjust :: Hedgehog.Gen a -> Compose Hedgehog.Gen Maybe a
adjust g = Compose $ Gen.frequency [(4, pure Nothing), (1, Just <$> g)]

instance Gen `At` Void where
  _ $ _ = Compose (pure Nothing)

instance Gen `At` Bool where
  _ $ NoField = Compose (pure Nothing)
  _ $ _ = adjust Gen.enumBounded

instance Gen `At` Centi where
  _ $ NoField = Compose (pure Nothing)
  _ $ Field _ (Constant c _) = Compose (pure $ Just c)
  _ $ _ = adjust $ MkFixed <$> Gen.integral (Range.linear 0 1_000_000_000)

instance Gen `At` Rational where
  _ $ Field _ (Constant c _) = Compose (pure $ Just c)
  _ $ _ = adjust $ ((/ 10_000) . toRational) <$> Gen.integral (Range.linear 0 10_000)

instance Gen `At` Int where
  _ $ _ = adjust $ Gen.int (Range.linear 1900 2030)

instance Gen `At` Word where
  _ $ _ = adjust $ Gen.word (Range.linear 0 1_000)

instance Gen `At` Text where
  _ $ _ = adjust $ Gen.text (Range.linear 1 500) Gen.unicode

instance Gen `At` Province.Code where
  _ $ _ = adjust $ Gen.element Province.all

instance Gen `At` Day where
  _ $ _ = adjust $ Gen.enum (fromGregorian 1910 1 1) (fromGregorian 2050 12 31)

instance Gen `At` LanguageOfCorrespondence where
  _ $ _ = adjust Gen.enumBounded

instance Gen `At` MaritalStatus where
  _ $ _ = adjust Gen.enumBounded
