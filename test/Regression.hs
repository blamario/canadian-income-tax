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
import Tax.Canada.T1.FieldNames.QC qualified as QC (t1Fields)
import Tax.Canada.T1.Fix (T1, fixT1)
import Tax.Canada.Province.ON.ON428.FieldNames (on428Fields)
import Tax.Canada.Province.ON.ON428.Fix (ON428, fixON428)
import Tax.FDF as FDF

import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy (fromStrict)
import Data.Functor.Const (Const (Const, getConst))
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Semigroup (All (All, getAll))
import Data.Text (isInfixOf, isSuffixOf, stripSuffix)
import Rank2 qualified
import System.Directory (listDirectory)
import System.Exit (die)
import System.FilePath.Posix (combine)
import Text.FDF (FDF, parse, serialize)
import Text.FDF qualified

import Test.Tasty
import Test.Tasty.Golden

main = do
  fdfFileNames <- listDirectory inputDir 
  fdfBytes <- traverse (ByteString.readFile . combine inputDir) fdfFileNames
  case traverse parse fdfBytes of
    Left err -> die err
    Right fdfs -> defaultMain $ golden $ zip fdfFileNames fdfs

rootDir, inputDir, outputDir, referenceDir :: FilePath
rootDir = combine "test" "regression"
inputDir = combine rootDir "inputs"
outputDir = combine rootDir "outputs"
referenceDir = combine rootDir "reference"

golden :: [(FilePath, FDF)] -> TestTree
golden fdfMap =
  testGroup "Regression" [
      goldenVsString
         path
         (combine referenceDir path)
         (pure $ fromStrict $ serialize $ FDF.update ON.t1Fields (fixT1 $ either error id $ FDF.load ON.t1Fields fdf) fdf)
      | (path, fdf) <- fdfMap]
