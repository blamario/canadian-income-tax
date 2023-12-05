{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Tax.Canada (fixAlbertaReturns, fixBritishColumbiaReturns, fixManitobaReturns, fixOntarioReturns)
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
import Tax.Canada.Province.AB.AB428.FieldNames (ab428Fields)
import Tax.Canada.Province.BC.BC428.FieldNames (bc428Fields)
import Tax.Canada.Province.MB.MB428.FieldNames (mb428Fields)
import Tax.Canada.Province.ON.ON428.FieldNames (on428Fields)
import Tax.Canada.Province.AB.AB428.Fix (AB428, fixAB428)
import Tax.Canada.Province.BC.BC428.Fix (BC428, fixBC428)
import Tax.Canada.Province.MB.MB428.Fix (MB428, fixMB428)
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
  fdfT1FileNames <- listDirectory (combine dataDir "T1/fdf")
  fdf428FileNames <- filter (".fdf" `isExtensionOf`) <$> listDirectory (combine dataDir "428")
  fdfT1Bytes <- traverse (ByteString.readFile . combine dataDir . combine "T1/fdf") fdfT1FileNames
  fdf428Bytes <- traverse (ByteString.readFile . combine dataDir . combine "428") fdf428FileNames
  case (,) <$> traverse parse fdfT1Bytes <*> traverse parse fdf428Bytes of
    Left err -> die err
    Right (fdfsT1, fdfs428) -> defaultMain $ properties (zip fdfT1FileNames fdfsT1) (zip fdf428FileNames fdfs428)

properties :: [(FilePath, FDF)] -> [(FilePath, FDF)] -> TestTree
properties fdfT1Map fdf428Map =
  testGroup "Properties" [
    testGroup "Idempotence" [
      testGroup "Alberta" [
        testProperty "T1" (checkFormIdempotent AB.t1Fields fixT1),
        testProperty "AB428" (checkFormIdempotent ab428Fields fixAB428),
        testProperty "T1+AB428" (checkFormPairIdempotent AB.t1Fields ab428Fields fixAlbertaReturns)],
      testGroup "British Columbia" [
        testProperty "T1" (checkFormIdempotent BC.t1Fields fixT1),
        testProperty "BC428" (checkFormIdempotent bc428Fields fixBC428),
        testProperty "T1+BC428" (checkFormPairIdempotent BC.t1Fields bc428Fields fixBritishColumbiaReturns)],
      testGroup "Manitoba" [
        testProperty "T1" (checkFormIdempotent AB.t1Fields fixT1),
        testProperty "MB428" (checkFormIdempotent mb428Fields fixMB428),
        testProperty "T1+MB428" (checkFormPairIdempotent AB.t1Fields mb428Fields fixManitobaReturns)],
      testGroup "Ontario" [
        testProperty "T1" (checkFormIdempotent ON.t1Fields fixT1),
        testProperty "ON428" (checkFormIdempotent on428Fields fixON428),
        testProperty "T1+ON428" (checkFormPairIdempotent ON.t1Fields on428Fields fixOntarioReturns)]],
    testGroup "Roundtrip" [
      testGroup "T1" [
        testProperty ("T1 for " <> name) (checkFormFields fields $ List.lookup (prefix <> "-r-fill-22e.fdf") fdfT1Map)
        | (name, prefix, fields) <- provincesT1],
      testGroup "428" [
        testProperty ("Form 428 for " <> name) (checkFields $ List.lookup (prefix <> "-c-fill-22e.fdf") fdf428Map)
        | (name, prefix, checkFields) <- provinces428]],
    testGroup "Load mismatch" [
      testProperty ("Load T1 for " <> p1name <> " from FDF for " <> p2name) $ property $ assert
        $ any (isLeft  . FDF.load p1fields) $ List.lookup (p2fdfPrefix <> "-r-fill-22e.fdf") fdfT1Map
      | (p1name, _, p1fields) <- provincesT1,
        (p2name, p2fdfPrefix, _) <- provincesT1,
        p1name /= p2name,
        not (p1name == "New Brunswick & PEI" && p2name == "Nunavut")]]
  where provincesT1 = [("New Brunswick & PEI", "5000", NB.t1Fields),
                       ("Newfoundland and Labrador", "5001", NL.t1Fields),
                       ("Quebec", "5005", QC.t1Fields),
                       ("Ontario", "5006", ON.t1Fields),
                       ("British Columbia", "5010", BC.t1Fields),
                       ("Northwest Territories", "5012", NT.t1Fields),
                       ("Yukon", "5011", YT.t1Fields),
                       ("Nunavut", "5014", NU.t1Fields),
                       ("Alberta, Manitoba, Nova Scotia, and Saskatchewan", "5015", AB.t1Fields)]
        provinces428 = [("Ontario",  "5006", checkFormFields on428Fields),
                        ("Manitoba", "5007", checkFormFields mb428Fields),
                        ("Alberta",  "5009", checkFormFields ab428Fields),
                        ("British Columbia", "5010", checkFormFields bc428Fields)]

checkFormPairIdempotent :: (Eq (g Maybe), Show (g Maybe), Eq (h Maybe), Show (h Maybe),
                            Rank2.Applicative g, Shallow.Traversable Transformations.Gen g,
                            Rank2.Applicative h, Shallow.Traversable Transformations.Gen h)
                        => g FieldConst -> h FieldConst -> ((g Maybe, h Maybe) -> (g Maybe, h Maybe)) -> Property
checkFormPairIdempotent fields1 fields2 f =
   checkFormIdempotent (Rank2.Pair fields1 fields2) (\(Rank2.Pair x y)-> uncurry Rank2.Pair $ curry f x y)

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

