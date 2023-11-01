{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Tax.Canada (fixOntarioReturns)
import Tax.Canada.T1.FieldNames (t1Fields)
import Tax.Canada.T1.FieldNames.AB qualified as AB (t1Fields)
import Tax.Canada.T1.FieldNames.BC qualified as BC (t1Fields)
import Tax.Canada.T1.Fix (T1, fixT1)
import Tax.Canada.ON428.FieldNames (on428Fields)
import Tax.Canada.ON428.Fix (ON428, fixON428)
import Tax.FDF as FDF
import Paths_canadian_income_tax (getDataDir)

import Test.Transformations qualified as Transformations

import Data.ByteString qualified as ByteString
import Data.Either (isRight)
import Data.Functor.Const (Const (Const, getConst))
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Semigroup (All (All, getAll))
import Data.Text (isInfixOf, isSuffixOf, stripSuffix)
import Rank2 qualified
import System.Directory (listDirectory)
import System.Exit (die)
import System.FilePath.Posix (combine)
import Transformation.Shallow qualified as Shallow
import Text.FDF (FDF, parse)
import Text.FDF qualified

import Hedgehog (Gen, Property, (===), annotateShow, assert, forAll, property)
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
      testProperty "T1" (checkFormIdempotent t1Fields fixT1),
      testProperty "ON428" (checkFormIdempotent on428Fields fixON428),
      testProperty "T1+ON428" (checkFormIdempotent (Rank2.Pair t1Fields on428Fields) fixOntarioReturns')],
    testGroup "Roundtrip" [
      testProperty "T1 Alberta" (checkFormFields AB.t1Fields $ List.lookup "5015-r-fill-22e.fdf" fdfMap),
      testProperty "T1 British Columbia" (checkFormFields BC.t1Fields $ List.lookup "5010-r-fill-22e.fdf" fdfMap),
      testProperty "T1 Ontario" (checkFormFields t1Fields $ List.lookup "5006-r-fill-22e.fdf" fdfMap),
      testProperty "ON428" (checkFormFields on428Fields $ List.lookup "5006-c-fill-22e.fdf" fdfMap)]]
  where fixOntarioReturns' :: Rank2.Product T1 ON428 Maybe -> Rank2.Product T1 ON428 Maybe
        fixOntarioReturns' (Rank2.Pair x y) = uncurry Rank2.Pair $ fixOntarioReturns (x, y)

checkFormIdempotent :: (Eq (g Maybe), Show (g Maybe),
                        Rank2.Applicative g, Shallow.Traversable Transformations.Gen g)
                    => g FieldConst -> (g Maybe -> g Maybe) -> Property
checkFormIdempotent fields f = checkIdempotent (generateForm fields) f

checkFormFields :: (Eq (g Maybe), Show (g Maybe),
                    Rank2.Applicative g, Shallow.Traversable Transformations.Gen g)
                => g FieldConst -> Maybe FDF -> Property
checkFormFields _ Nothing = error "Missing FDF template"
checkFormFields fields (Just fdf) = property $ do
  annotateShow $ FDF.load fields fdf
  assert $ isRight $ FDF.load fields fdf
  form <- forAll (generateForm fields)
  let fdf' = FDF.update fields form fdf
      formKeys = FDF.formKeys fields
      fdfKeys = Text.FDF.foldMapWithKey (const . (:[]) . map dropIndex) fdf
      dropIndex t = fromMaybe t (stripSuffix "[0]" t)
      keyHeads = List.nub $ take 2 <$> formKeys
      noCheckbox = filter $ not . any (liftA2 (||) (isSuffixOf "Checkbox") (isInfixOf "CheckBox"))
  -- annotateShow fdf'
  annotateShow keyHeads
  FDF.load fields fdf' === Right form
  List.sort (noCheckbox formKeys) === List.sort (noCheckbox $ filter (\x-> any (`List.isPrefixOf` x) keyHeads) fdfKeys)

generateForm :: (Rank2.Applicative g, Shallow.Traversable Transformations.Gen g) => g FieldConst -> Gen (g Maybe)
generateForm = Shallow.traverse Transformations.Gen

checkIdempotent :: (Eq a, Show a) => Gen a -> (a -> a) -> Property
checkIdempotent gen f = property $ forAll gen >>= \x-> let x' = f x in f x' === x'

