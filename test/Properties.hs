{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Tax.Canada (fixOntarioReturns)
import Tax.Canada.T1.FieldNames (t1Fields)
import Tax.Canada.T1.Fix (T1, fixT1)
import Tax.Canada.ON428.FieldNames (on428Fields)
import Tax.Canada.ON428.Fix (ON428, fixON428)
import Tax.FDF as FDF
import Paths_canadian_income_tax (getDataDir)

import Test.Transformations qualified as Transformations

import Data.ByteString qualified as ByteString
import Data.Functor.Const (Const (Const, getConst))
import Data.List qualified as List
import Data.Semigroup (All (All, getAll))
import Rank2 qualified
import System.Directory (listDirectory)
import System.Exit (die)
import System.FilePath.Posix (combine)
import Transformation.Shallow qualified as Shallow
import Text.FDF (FDF, parse)

import Hedgehog (Gen, Property, (===), annotateShow, forAll, property)
import Hedgehog.Gen qualified as Gen
import Test.Tasty
import Test.Tasty.Hedgehog

main = do
  dataDir <- getDataDir
  fdfFileNames <- listDirectory dataDir
  fdfBytes <- traverse (ByteString.readFile . combine dataDir) fdfFileNames
  case traverse parse fdfBytes of
    Left err -> die err
    Right fdfs -> defaultMain $ properties $ zip fdfFileNames fdfs

properties :: [(FilePath, FDF)] -> TestTree
properties fdfMap =
  testGroup "Properties" [
    testGroup "Idempotence" [
      testProperty "T1" (checkFormIdempotent fixT1),
      testProperty "ON428" (checkFormIdempotent fixON428),
      testProperty "T1+ON428" (checkFormIdempotent fixOntarioReturns')],
    testGroup "Roundtrip" [
      testProperty "T1" (checkFormFields t1Fields $ List.lookup "5006-r-fill-22e.fdf" fdfMap),
      testProperty "ON428" (checkFormFields on428Fields $ List.lookup "5006-c-fill-22e.fdf" fdfMap)]]
  where fixOntarioReturns' :: Rank2.Product T1 ON428 Maybe -> Rank2.Product T1 ON428 Maybe
        fixOntarioReturns' (Rank2.Pair x y) = uncurry Rank2.Pair $ fixOntarioReturns (x, y)

checkFormIdempotent :: (Eq (g Maybe), Show (g Maybe),
                        Rank2.Applicative g, Shallow.Traversable Transformations.Gen g)
                    => (g Maybe -> g Maybe) -> Property
checkFormIdempotent f = checkIdempotent generateForm f

checkFormFields :: (Eq (g Maybe), Show (g Maybe),
                    Rank2.Applicative g, Shallow.Traversable Transformations.Gen g)
                => g FieldConst ->  Maybe FDF -> Property
checkFormFields _ Nothing = error "Missing FDF template"
checkFormFields fields (Just fdf) = property $ do
  form <- forAll (Gen.filter (formValidForFDF fields) generateForm)
  let fdf' = FDF.update fields form fdf
  annotateShow fdf'
  FDF.load fields fdf' === Right form

formValidForFDF :: (Rank2.Apply g, Rank2.Foldable g) => g FieldConst -> g Maybe -> Bool
formValidForFDF fields = getAll . Rank2.foldMap (All . getConst) . Rank2.liftA2 nonoField fields
  where nonoField :: FieldConst a -> Maybe a -> Const Bool a
        nonoField Field {entry = Textual} (Just "") = Const False
        nonoField Field{} _ = Const True
        nonoField NoField Nothing = Const True
        nonoField NoField _ = Const False

generateForm :: (Rank2.Applicative g, Shallow.Traversable Transformations.Gen g) => Gen (g Maybe)
generateForm = Shallow.traverse Transformations.Gen (Rank2.pure Nothing)

checkIdempotent :: (Eq a, Show a) => Gen a -> (a -> a) -> Property
checkIdempotent gen f = property $ forAll gen >>= \x-> let x' = f x in f x' === x'

