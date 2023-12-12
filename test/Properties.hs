{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Tax.Canada.Province.AB qualified as AB
import Tax.Canada.Province.BC qualified as BC
import Tax.Canada.Province.MB qualified as MB
import Tax.Canada.Province.NB qualified as NB
import Tax.Canada.Province.NL qualified as NL
import Tax.Canada.Province.ON qualified as ON
import Tax.Canada.Province.QC qualified as QC
import Tax.Canada.Territory.NT qualified as NT
import Tax.Canada.Territory.NU qualified as NU
import Tax.Canada.Territory.YT qualified as YT
import Tax.Canada.T1 (T1, fixT1)
import Tax.FDF as FDF
import Paths_canadian_income_tax (getDataDir)

import Test.Transformations qualified as Transformations

import Data.ByteString qualified as ByteString
import Data.Either (isLeft, isRight)
import Data.Functor.Const (Const (Const, getConst))
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Semigroup (All (All, getAll))
import Data.Text (Text, isInfixOf, isSuffixOf, stripSuffix)
import Rank2 qualified
import System.Directory (listDirectory)
import System.Exit (die)
import System.FilePath.Posix (combine, isExtensionOf)
import Transformation.Shallow qualified as Shallow
import Text.FDF (FDF (body), Field, parse)
import Text.FDF qualified

import Hedgehog (Gen, Property, (===), annotateShow, assert, forAll, property)
import Hedgehog.Gen qualified as Gen
import Test.Tasty
import Test.Tasty.Hedgehog

main = do
  dataDir <- getDataDir
  fdfMaps <- traverse (readFDFs . combine dataDir) ["T1/fdf", "428", "479"]
  either die (defaultMain . properties) $ sequenceA fdfMaps
  where
    readFDFs :: FilePath -> IO (Either String [(FilePath, FDF)])
    readFDFs dir = do
      fdfFileNames <- listDirectory dir
      fdfBytes <- traverse (ByteString.readFile . combine dir) fdfFileNames
      pure $ traverse (traverse parse) $ zip fdfFileNames fdfBytes

properties :: [[(FilePath, FDF)]] -> TestTree
properties [fdfT1Map, fdf428Map, fdf479Map] =
  testGroup "Properties" [
    testGroup "Idempotence" [
      testGroup "Alberta" [
        testProperty "T1" (checkFormIdempotent AB.t1Fields fixT1),
        testProperty "AB428" (checkFormIdempotent AB.ab428Fields AB.fixAB428),
        testProperty "T1+AB428" (checkFormPairIdempotent AB.t1Fields AB.ab428Fields AB.fixReturns)],
      testGroup "British Columbia" [
        testProperty "T1" (checkFormIdempotent BC.t1Fields fixT1),
        testProperty "BC428" (checkFormIdempotent BC.bc428Fields BC.fixBC428),
        testProperty "T1+BC428" (checkFormPairIdempotent BC.t1Fields BC.bc428Fields BC.fixReturns)],
      testGroup "Manitoba" [
        testProperty "T1" (checkFormIdempotent MB.t1Fields fixT1),
        testProperty "MB428" (checkFormIdempotent MB.mb428Fields MB.fixMB428),
        testProperty "T1+MB428" (checkFormPairIdempotent AB.t1Fields MB.mb428Fields MB.fixReturns)],
      testGroup "Ontario" [
        testProperty "T1" (checkFormIdempotent ON.returnFields.t1 fixT1),
        testProperty "ON428" (checkFormIdempotent ON.returnFields.on428 ON.fixON428),
        testProperty "ON479" (checkFormIdempotent ON.returnFields.on479 ON.fixON479),
        testProperty "T1+ON428" (checkFormIdempotent ON.returnFields ON.fixReturns)]],
    testGroup "Roundtrip" [
      testGroup "T1" [
        testProperty ("T1 for " <> name) (checkFormFields fields $ List.lookup (prefix <> "-r-fill-22e.fdf") fdfT1Map)
        | (name, prefix, fields) <- provincesT1],
      testGroup "428" [
        testProperty ("Form 428 for " <> name) (checkFields $ List.lookup (prefix <> "-c-fill-22e.fdf") fdf428Map)
        | (name, prefix, checkFields) <- provinces428],
      testGroup "479" [
        testProperty ("Form 479 for " <> name) (checkFields $ List.lookup (prefix <> "-tc-fill-22e.fdf") fdf479Map)
        | (name, prefix, checkFields) <- provinces479]],
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
        provinces428 = [("Ontario",  "5006", checkFormFields ON.on428Fields),
                        ("Manitoba", "5007", checkFormFields MB.mb428Fields),
                        ("Alberta",  "5009", checkFormFields AB.ab428Fields),
                        ("British Columbia", "5010", checkFormFields BC.bc428Fields)]
        provinces479 = [("Ontario",  "5006", checkFormFields ON.on479Fields)]
properties maps = error ("Unexpected data directory contents: " <> show maps)

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
  let Right fdf' = FDF.update fields form fdf
      formKeys = FDF.formKeys fields
      fdfKeys = Text.FDF.foldMapWithKey (const . (:[]) . map dropIndex) fdf
      dropIndex t = fromMaybe t (stripSuffix "[0]" t)
      keyHeads = List.nub $ take 2 <$> formKeys
      noCheckbox :: [[Text]] -> [[Text]]
      noCheckbox = filter $ not . or . ([isSuffixOf "Checkbox", isInfixOf "CheckBox", (== "QuestionA"), (== "Note1")] <*>)
  -- annotateShow fdf'
  FDF.load fields fdf' === Right form
  List.sort (noCheckbox formKeys) === List.sort (noCheckbox $ filter (\x-> any (`List.isPrefixOf` x) keyHeads) fdfKeys)

generateForm :: (Rank2.Applicative g, Shallow.Traversable Transformations.Gen g) => g FieldConst -> Gen (g Maybe)
generateForm = Shallow.traverse Transformations.Gen

checkIdempotent :: (Eq a, Show a) => Gen a -> (a -> a) -> Property
checkIdempotent gen f = property $ forAll gen >>= \x-> let x' = f x in f x' === x'

