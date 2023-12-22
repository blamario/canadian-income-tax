{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Codec.Archive.Zip (addEntryToArchive, emptyArchive, fromArchive, toEntry)
import Control.Category ((>>>))
import Control.Monad (forM, join)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (assert)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as Lazy
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Functor.Compose (Compose(..))
import Data.Map.Lazy qualified as Map
import Data.Monoid.Textual (toString)
import Data.Monoid.Instances.ByteString.UTF8 (ByteStringUTF8(..))
import Data.String (fromString)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Text (Text)
import Data.Text.Lazy qualified as Text.Lazy
import Network.HTTP.Types.Status (ok200, internalServerError500,
                                  notFound404, unsupportedMediaType415, unprocessableEntity422)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Parse (FileInfo (..))
import System.FilePath.Posix ((</>))
import System.Posix.Temp (mkdtemp)
import Text.FDF (parse, serialize)
import Text.Read (readMaybe)
import Web.Scotty

import Paths_canadian_income_tax (getDataDir)
import Tax.Canada (completeForms)
import Tax.FDF qualified as FDF
import Tax.PDFtk (fdf2pdf, pdfFile2fdf)

main :: IO ()
main = scotty 3000 $ do
   middleware logStdoutDev
   get "/" $ do
      setHeader "Content-Type" "text/html; charset=utf-8"
      file "web/client/build/index.html"
   post "/t1/PDF/:province" $ do
      provinceCode <- captureParam "province"
      province <- case readMaybe (Text.Lazy.unpack provinceCode)
                  of Nothing -> raiseStatus notFound404 ("No such province as " <> provinceCode)
                     Just p -> pure p
      pdfFiles <- map (first Text.Lazy.toStrict) <$> files
      dir <- liftIO $ mkdtemp "tax"
      fdfBytes <- liftIO $ fmap sequenceA $ forM pdfFiles $ \(key, FileInfo name _ content)-> do
        let path = dir </> fromUTF8 name
        Lazy.writeFile path content
        fdf <- pdfFile2fdf path
        pure ((,) key <$> fdf)
      case fdfBytes >>= traverse (traverse $ Lazy.toStrict >>> parse) of
        Left err -> status unsupportedMediaType415 >> text (fromString err)
        Right fdfs -> case completeForms province (Map.fromList fdfs) of
          Left err -> status unprocessableEntity422 >> text (fromString err)
          Right fdfs' -> do
            let fdfBytes' = (Lazy.fromStrict . serialize <$>) <$> Map.toList fdfs'
                replaceContent (key1, c) (key2, FileInfo name ty _) =
                  assert (key1 == key2) $
                  ((,) key1 . FileInfo name ty <$>) <$> fdf2pdf (dir </> fromUTF8 name) c
            pdfFiles' <- liftIO $ sequenceA $ zipWith replaceContent fdfBytes' pdfFiles
            case sequenceA pdfFiles' of
              Left err -> do
                status internalServerError500 >> text (fromString err)
              Right [(_, FileInfo _ _ pdf)] -> do
                status ok200
                setHeader "Content-Type" "application/pdf"
                raw pdf
              Right pdfFiles' -> do
                now <- liftIO $ round . nominalDiffTimeToSeconds <$> getPOSIXTime
                let pdfArchive = foldr addPDF emptyArchive pdfFiles'
                    addPDF (_, FileInfo name _ c) = addEntryToArchive (toEntry (fromUTF8 name) now c)
                status ok200
                setHeader "Content-Type" "application/zip"
                raw (fromArchive pdfArchive)
   middleware $ staticPolicy (noDots >-> addBase "web/client/build")

fromUTF8 :: ByteString -> String
fromUTF8 = toString mempty . ByteStringUTF8
