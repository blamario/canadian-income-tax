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
import Tax.Canada.Province.PE qualified as PE
import Tax.Canada.Province.QC qualified as QC
import Tax.Canada.Territory.NT qualified as NT
import Tax.Canada.Territory.NU qualified as NU
import Tax.Canada.Territory.YT qualified as YT
import Tax.Canada.T1 (T1, fixT1)
import Tax.Canada.T4 (t4Fields)
import Tax.Canada.Federal qualified as Federal
import Tax.Canada.Federal.Schedule6 (schedule6Fields)
import Tax.Canada.Federal.Schedule7 (schedule7Fields)
import Tax.Canada.Federal.Schedule8 (schedule8Fields)
import Tax.Canada.Federal.Schedule9 (schedule9Fields)
import Tax.Canada.Federal.Schedule11 (schedule11Fields)
import Tax.FDF as FDF
import Paths_canadian_income_tax (getDataDir)

import Test.Transformations qualified as Transformations

import Data.ByteString qualified as ByteString
import Data.Char qualified as Char
import Data.Either (isLeft, isRight)
import Data.Functor.Const (Const (Const, getConst))
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Monoid.Textual qualified as Textual
import Data.Semigroup.Cancellative (isSuffixOf, stripPrefix)
import Data.Semigroup (All (All, getAll))
import Data.Text (Text, isInfixOf, stripSuffix)
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
  fdfMaps <- traverse (readFDFs . combine dataDir) ["fdf", "fdf/T1", "fdf/428", "fdf/479"]
  either die (defaultMain . properties) $ sequenceA fdfMaps
  where
    readFDFs :: FilePath -> IO (Either String [(FilePath, FDF)])
    readFDFs dir = do
      fdfFileNames <- filter (".fdf" `isSuffixOf`) <$> listDirectory dir
      fdfBytes <- traverse (ByteString.readFile . combine dir) fdfFileNames
      pure $ traverse (traverse parse) $ zip fdfFileNames fdfBytes

properties :: [[(FilePath, FDF)]] -> TestTree
properties [dataRootMap, fdfT1Map, fdf428Map, fdf479Map] =
  testGroup "Properties" [
    testGroup "Idempotence" [
      testGroup "Alberta" [
        testProperty "T1" (checkFormIdempotent AB.t1Fields fixT1),
        testProperty "AB428" (checkFormIdempotent AB.ab428Fields AB.fixAB428),
        testProperty "Federal+AB428" (checkFederalFormIdempotent AB.returnFields AB.fixReturns)],
      testGroup "British Columbia" [
        testProperty "T1" (checkFormIdempotent BC.t1Fields fixT1),
        testProperty "BC428" (checkFormIdempotent BC.bc428Fields BC.fixBC428),
        testProperty "Federal+BC428+BC479" (checkFederalFormIdempotent BC.returnFields BC.fixReturns)],
      testGroup "Manitoba" [
        testProperty "T1" (checkFormIdempotent MB.t1Fields fixT1),
        testProperty "MB428" (checkFormIdempotent MB.mb428Fields MB.fixMB428),
        testProperty "Federal+MB428" (checkFederalFormIdempotent MB.returnFields MB.fixReturns)],
      testGroup "Ontario" [
        testProperty "T1" (checkFormIdempotent ON.t1Fields fixT1),
        testProperty "ON428" (checkFormIdempotent ON.returnFields.on428 ON.fixON428),
        testProperty "ON479" (checkFormIdempotent ON.returnFields.on479 ON.fixON479),
        testProperty "Federal+ON428+ON479" (checkFederalFormIdempotent ON.returnFields ON.fixReturns)]],
    testGroup "Roundtrip" [
      testGroup "T1" [
        testProperty ("T1 for " <> name) (checkFormFields fields $ List.lookup (prefix <> "-r-fill-24e.fdf") fdfT1Map)
        | (name, prefix, fields) <- provincesT1],
      testProperty "T4" (checkFormFields t4Fields $ List.lookup "t4-fill-24e.fdf" dataRootMap),
      testProperty "Schedule 6" (checkFormFields schedule6Fields $ List.lookup "5000-s6-fill-24e.fdf" dataRootMap),
      testProperty "Schedule 7" (checkFormFields schedule7Fields $ List.lookup "5000-s7-fill-24e.fdf" dataRootMap),
      testProperty "Schedule 8" (checkFormFields schedule8Fields $ List.lookup "5000-s8-fill-24e.fdf" dataRootMap),
      testProperty "Schedule 9" (checkFormFields schedule9Fields $ List.lookup "5000-s9-fill-24e.fdf" dataRootMap),
      testProperty "Schedule 11" (checkFormFields schedule11Fields $ List.lookup "5000-s11-fill-24e.fdf" dataRootMap),
      testGroup "428" [
        testProperty ("Form 428 for " <> name) (checkFields $ List.lookup (prefix <> "-c-fill-24e.fdf") fdf428Map)
        | (name, prefix, checkFields) <- provinces428],
      testGroup "479" [
        testProperty ("Form 479 for " <> name) (checkFields $ List.lookup (prefix <> "-tc-fill-24e.fdf") fdf479Map)
        | (name, prefix, checkFields) <- provinces479]],
    testGroup "Load mismatch" [
      testProperty ("Load T1 for " <> p1name <> " from FDF for " <> p2name) $ property $ assert
        $ any (isLeft  . FDF.load p1fields) $ List.lookup (p2fdfPrefix <> "-r-fill-24e.fdf") fdfT1Map
      | (p1name, _, p1fields) <- provincesT1,
        (p2name, p2fdfPrefix, _) <- provincesT1,
        p1name /= p2name]]
  where provincesT1 = [("Newfoundland and Labrador", "5001", NL.t1Fields),
                       ("PEI", "5002", PE.t1Fields),
                       ("Quebec", "5005", QC.t1Fields),
                       ("Ontario", "5006", ON.t1Fields),
                       ("British Columbia", "5010", BC.t1Fields),
                       ("Northwest Territories", "5012", NT.t1Fields),
                       ("Yukon", "5011", YT.t1Fields),
                       ("Nunavut", "5014", NU.t1Fields),
                       ("Alberta, Manitoba, New Brunswick, Nova Scotia, and Saskatchewan", "5015", AB.t1Fields)]
        provinces428 = [("Ontario",  "5006", checkFormFields ON.on428Fields),
                        ("Manitoba", "5007", checkFormFields MB.mb428Fields),
                        ("Alberta",  "5009", checkFormFields AB.ab428Fields),
                        ("British Columbia", "5010", checkFormFields BC.bc428Fields)]
        provinces479 = [("Ontario",  "5006", checkFormFields ON.on479Fields),
                        ("British Columbia", "5010", checkFormFields BC.bc479Fields)]
properties maps = error ("Unexpected data directory contents: " <> show maps)

checkFormIdempotent :: (Eq (g Maybe), Show (g Maybe),
                        Rank2.Applicative g, Shallow.Traversable Transformations.Gen g)
                    => g FieldConst -> (g Maybe -> g Maybe) -> Property
checkFormIdempotent fields f = checkIdempotent (generateForm fields) f

checkFederalFormIdempotent :: (Eq (g Maybe), Show (g Maybe),
                               Rank2.Applicative g, Shallow.Traversable Transformations.Gen g)
                           => g FieldConst -> (Federal.InputForms Maybe -> g Maybe -> g Maybe) -> Property
checkFederalFormIdempotent fields f = checkIdempotent (generateForm fields) (f mempty)

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
      noCheckbox = filter $ not . or . ([isSuffixOf "Checkbox", isInfixOf "CheckBox",
                                         isInfixOf "Footnote", isInfixOf "address", isInfixOf "Nameof",
                                         any (Textual.all Char.isDigit) . stripPrefix "Note",
                                         (== "QuestionA"), (== "CAI-2023")] <*>)
  -- annotateShow fdf'
  FDF.load fields fdf' === Right form
  List.sort (noCheckbox formKeys) === List.sort (noCheckbox $ filter (\x-> any (`List.isPrefixOf` x) keyHeads) fdfKeys)

generateForm :: (Rank2.Applicative g, Shallow.Traversable Transformations.Gen g) => g FieldConst -> Gen (g Maybe)
generateForm = Shallow.traverse Transformations.Gen

checkIdempotent :: (Eq a, Show a) => Gen a -> (a -> a) -> Property
checkIdempotent gen f = property $ forAll gen >>= \x-> let x' = f x in f x' === x'

