{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.State.Strict (get, put, evalState)
import Data.ByteString qualified as ByteString
import Data.Text qualified as Text
import System.Environment (getArgs)
import Text.FDF (parse, serialize)

import Tax.Canada.T1.FieldNames
import Tax.Canada.T1.FDF qualified as FDF
import Tax.Canada.T1.Types

main :: IO ()
main = do
  [path] <- getArgs
  bytes <- ByteString.readFile path
  case parse bytes >>= (\x-> (,) x <$> FDF.load x) of
    Left err -> error err
    Right (fdf, t1) -> do
      let fdf' = FDF.update t1 fdf
          textPath = Text.pack path
          (prefix, extension) = evalState (Text.spanEndM beforeDot textPath) True
          beforeDot '.' = True <$ put False
          beforeDot c = get <* put True
          updatedPath = Text.unpack (prefix <> "-updated" <> extension)
      ByteString.writeFile updatedPath (serialize fdf')
      print t1
