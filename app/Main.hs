{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.ByteString qualified as ByteString
import System.Environment (getArgs)
import Text.FDF (parse)

import Tax.Canada.T1.FieldNames
import Tax.Canada.T1.FDF qualified as FDF
import Tax.Canada.T1.Types

main :: IO ()
main = do
  [file] <- getArgs
  fdf <- ByteString.readFile file
  case parse fdf >>= FDF.load of
    Left err -> error err
    Right t1 -> print t1
