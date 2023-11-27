{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Tax.Canada (fixOntarioReturns)
import Tax.Canada.T1.FieldNames.ON qualified as ON (t1Fields)
import Tax.Canada.T1.FieldNames.AB qualified as AB (t1Fields)
import Tax.Canada.T1.FieldNames.BC qualified as BC (t1Fields)
import Tax.Canada.T1.FieldNames.NB qualified as NB (t1Fields)
import Tax.Canada.T1.FieldNames.NL qualified as NL (t1Fields)
import Tax.Canada.T1.FieldNames.NT qualified as NT (t1Fields)
import Tax.Canada.T1.FieldNames.NU qualified as NU (t1Fields)
import Tax.Canada.T1.FieldNames.QC qualified as QC (t1Fields)
import Tax.Canada.T1.FieldNames.YT qualified as YT (t1Fields)
import Tax.Canada.T1.Fix (T1, fixT1)
import Tax.Canada.Province.BC.BC428.FieldNames (bc428Fields)
import Tax.Canada.Province.ON.ON428.FieldNames (on428Fields)
import Tax.Canada.Province.ON.ON428.Fix (ON428, fixON428)
import Tax.FDF as FDF
import Paths_canadian_income_tax (getDataDir)

import Test.Transformations qualified as Transformations

import Data.ByteString qualified as ByteString
import Data.Either (isLeft, isRight)
import Data.Functor.Const (Const (Const, getConst))
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Semigroup (All (All, getAll))
import Data.Text (isInfixOf, isSuffixOf, stripSuffix)
import Rank2 qualified
import System.Directory (listDirectory)
import System.Exit (die)
import System.FilePath.Posix (combine, isExtensionOf)
import Transformation.Shallow qualified as Shallow
import Text.FDF (FDF, parse)
import Text.FDF qualified

import Hedgehog (Gen, Property, (===), annotateShow, assert, forAll, property)
import Hedgehog.Gen qualified as Gen
import Test.Tasty
import Test.Tasty.Hedgehog

main = do
  dataDir <- getDataDir
  fdfT1FileNames <- filter (".fdf" `isExtensionOf`) <$> listDirectory (combine dataDir "T1")
  fdf428FileNames <- filter (".fdf" `isExtensionOf`) <$> listDirectory (combine dataDir "428")
  fdfT1Bytes <- traverse (ByteString.readFile . combine dataDir . combine "T1") fdfT1FileNames
  fdf428Bytes <- traverse (ByteString.readFile . combine dataDir . combine "428") fdf428FileNames
  case (,) <$> traverse parse fdfT1Bytes <*> traverse parse fdf428Bytes of
    Left err -> die err
    Right (fdfsT1, fdfs428) -> defaultMain $ properties (zip fdfT1FileNames fdfsT1) (zip fdf428FileNames fdfs428)

properties :: [(FilePath, FDF)] -> [(FilePath, FDF)] -> TestTree
properties fdfT1Map fdf428Map =
  testGroup "Properties" [
    testGroup "Idempotence" [
      testProperty "T1" (checkFormIdempotent ON.t1Fields fixT1),
      testProperty "ON428" (checkFormIdempotent on428Fields fixON428),
      testProperty "T1+ON428" (checkFormIdempotent (Rank2.Pair ON.t1Fields on428Fields) fixOntarioReturns')],
    testGroup "Roundtrip" [
      testGroup "T1" [
        testProperty ("T1 for " <> name) (checkFormFields fields $ List.lookup (prefix <> "-r-fill-22e.fdf") fdfT1Map)
        | (name, prefix, fields, _) <- provinces],
      testGroup "428" [
        testProperty ("Form 428 for " <> name) (checkFields $ List.lookup (prefix <> "-c-fill-22e.fdf") fdf428Map)
        | (name, prefix, _, Just checkFields) <- provinces]],
    testGroup "Load mismatch" [
      testProperty ("Load T1 for " <> p1name <> " from FDF for " <> p2name) $ property $ assert
        $ any (isLeft  . FDF.load p1fields) $ List.lookup (p2fdfPrefix <> "-r-fill-22e.fdf") fdfT1Map
      | (p1name, _, p1fields, _) <- provinces,
        (p2name, p2fdfPrefix, _, _) <- provinces,
        p1name /= p2name,
        not (p1name == "New Brunswick & PEI" && p2name == "Nunavut")]]
  where fixOntarioReturns' :: Rank2.Product T1 ON428 Maybe -> Rank2.Product T1 ON428 Maybe
        fixOntarioReturns' (Rank2.Pair x y) = uncurry Rank2.Pair $ fixOntarioReturns (x, y)
        provinces = [("New Brunswick & PEI", "5000", NB.t1Fields, Nothing),
                     ("Newfoundland and Labrador", "5001", NL.t1Fields, Nothing),
                     ("Quebec", "5005", QC.t1Fields, Nothing),
                     ("Ontario", "5006", ON.t1Fields, Just $ checkFormFields on428Fields),
                     ("British Columbia", "5010", BC.t1Fields, Just $ checkFormFields bc428Fields),
                     ("Northwest Territories", "5012", NT.t1Fields, Nothing),
                     ("Yukon", "5011", YT.t1Fields, Nothing),
                     ("Nunavut", "5014", NU.t1Fields, Nothing),
                     ("Alberta, Manitoba, Nova Scotia, and Saskatchewan", "5015", AB.t1Fields, Nothing)]

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
      noCheckbox = filter $ not . any (liftA2 (||) (isSuffixOf "Checkbox") $ liftA2 (||) (isInfixOf "CheckBox") (== "QuestionA"))
  -- annotateShow fdf'
  FDF.load fields fdf' === Right form
  List.sort (noCheckbox formKeys) === List.sort (noCheckbox $ filter (\x-> any (`List.isPrefixOf` x) keyHeads) fdfKeys)

generateForm :: (Rank2.Applicative g, Shallow.Traversable Transformations.Gen g) => g FieldConst -> Gen (g Maybe)
generateForm = Shallow.traverse Transformations.Gen

checkIdempotent :: (Eq a, Show a) => Gen a -> (a -> a) -> Property
checkIdempotent gen f = property $ forAll gen >>= \x-> let x' = f x in f x' === x'

