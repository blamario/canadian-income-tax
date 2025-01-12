{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Tax.Canada.Federal qualified as Federal
import Tax.Canada.Province.ON qualified as ON
import Tax.Canada.Province.AB qualified as AB (t1Fields)
import Tax.Canada.Province.BC qualified as BC (t1Fields)
import Tax.Canada.T1 (T1, fixT1)
import Tax.FDF as FDF

import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy (fromStrict)
import Data.Foldable (toList)
import Data.Functor.Const (Const (Const, getConst))
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup (All (All, getAll))
import Data.Text (Text)
import Data.Text qualified as Text
import Rank2 qualified
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (die)
import System.FilePath.Posix (combine)
import Text.FDF (FDF, parse, serialize)
import Text.FDF qualified

import Test.Tasty
import Test.Tasty.Golden

main = listDirectory inputDir >>= traverse test >>= defaultMain . testGroup "Regression"

rootDir, inputDir, outputDir, referenceDir :: FilePath
rootDir = combine "test" "regression"
inputDir = combine rootDir "inputs"
outputDir = combine rootDir "outputs"
referenceDir = combine rootDir "reference"

test :: FilePath -> IO TestTree
test path = do
  let inputPath = combine inputDir path
  isDir <- liftIO (doesDirectoryExist inputPath)
  if isDir
    then testReturn path
    else testT1 path

testT1 :: FilePath -> IO TestTree
testT1 path = do
  fdfBytes <- ByteString.readFile $ combine inputDir path
  case parse fdfBytes of
    Left err -> die err
    Right fdf ->
      pure $
        goldenVsString path
          (combine referenceDir path)
          (pure $ fromStrict $ serialize $ either error id $ FDF.mapForm ON.t1Fields fixT1 fdf)

testReturn :: FilePath -> IO TestTree
testReturn path = do
  let inputPath = combine inputDir path
  fdfFileNames <- listDirectory inputPath
  fdfBytes <- traverse (ByteString.readFile . combine inputPath) fdfFileNames
  case do fdfs <- traverse parse fdfBytes
          let (inputs, keyedFillables) = foldMap decide $ zip fdfFileNames fdfs
              decide (name, fdf)
                | "t4-" `List.isPrefixOf` name = ([("T4", fdf)], [])
                | otherwise = ([], [(formKey name, fdf)])
          loadedInputs <- Federal.loadInputForms inputs
          FDF.mapForms ON.returnFields (ON.fixReturns loadedInputs) $ Map.fromList keyedFillables
    of Left err -> die err
       Right filled
         | let fdfOutputs = fromStrict . serialize <$> filled
               diff ref new = ["diff", "-uw", ref, new]
               compare key fdfOutput
                 | let Just fileName = List.find ((key ==) . formKey) fdfFileNames
                         = pure $ goldenVsStringDiff fileName diff (combine referenceDir $ combine path fileName)
                           $ pure fdfOutput
           -> testGroup path . toList <$> Map.traverseWithKey compare fdfOutputs


formKey :: FilePath -> Text
formKey "5000-s6-fill-23e.fdf" = "Schedule6"
formKey "5000-s8-fill-23e.fdf" = "Schedule8"
formKey "5006-c-fill-23e.fdf" = "428"
formKey "5006-r-fill-23e.fdf" = "T1"
formKey name = error ("File name " <> name <> " is not recognized as a form.")
