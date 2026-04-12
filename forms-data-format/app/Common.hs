-- | Common definitions for executables

{-# LANGUAGE Haskell2010, ImportQualifiedPost, OverloadedRecordDot, OverloadedStrings, NoFieldSelectors  #-}

module Common where

import Control.Monad (unless)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import System.Directory (doesFileExist)

import Text.FDF (FDF)
import Text.FDF qualified as FDF


-- | A single item of a difference between two FDFs
data Difference
  = Deletion Text
  | Addition Text
  | Change Text Text
  deriving (Eq, Read, Show)

-- | Read an FDF file from the given path, standard input if path is @-@
readFDF :: FilePath -> IO FDF
readFDF inputPath = do
   exists <- doesFileExist inputPath
   unless (inputPath == "-" || exists) (error $ "Input file " <> show inputPath <> " doesn't exist.")
   content <- if inputPath == "-" then ByteString.getContents else ByteString.readFile inputPath
   case FDF.parse content of
     Left err -> error ((if inputPath == "-" then "Standard input" else "File " <> inputPath)
                        <> " is not valid FDF:\n" <> err)
     Right fdf -> pure fdf
