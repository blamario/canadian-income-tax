{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as Lazy
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.String (fromString)
import Network.HTTP.Types.Status (ok200, unsupportedMediaType415)
import Text.FDF (parse, serialize)
import Web.Scotty

import Tax.FDF qualified as FDF
import Tax.Canada (fixOntarioReturns, fixON428, fixT1, on428Fields, t1Fields)

main :: IO ()
main = scotty 3000 $
   post "/t1/FDF" $ do
      bytes <- body
      case parse (Lazy.toStrict bytes) >>= \fdf-> (,) fdf <$> FDF.load t1Fields fdf of
        Left err -> status unsupportedMediaType415 >> text (fromString err)
        Right (fdf, form) -> do
          let fdf' = FDF.update t1Fields form' fdf
              form' = fixT1 form
          status ok200
          setHeader "Content-Type" "application/fdf"
          raw (Lazy.fromStrict $ serialize fdf')
