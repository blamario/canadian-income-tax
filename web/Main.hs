{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Category ((>>>))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as Lazy
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.String (fromString)
import Data.Text.Lazy qualified as Text.Lazy
import Network.HTTP.Types.Status (ok200, internalServerError500, notFound404, unsupportedMediaType415)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import System.FilePath.Posix (combine)
import Text.FDF (parse, serialize)
import Text.Read (readMaybe)
import Web.Scotty

import Paths_canadian_income_tax (getDataDir)
import Tax.Canada (fixOntarioReturns, fixT1)
import Tax.Canada.T1.FieldNames (formPrefixForProvince, t1FieldsForProvince)
import Tax.FDF qualified as FDF
import Tax.PDFtk (fdf2pdf, pdf2fdf)

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
      let t1Fields = t1FieldsForProvince province
          t1FormFile = formPrefixForProvince province <> "-r-fill-22e.pdf"
      pdfBytes <- body
      fdfBytes <- liftIO (pdf2fdf pdfBytes)
      case fdfBytes >>= (Lazy.toStrict >>> parse) >>= \fdf-> (,) fdf <$> FDF.load t1Fields fdf of
        Left err -> status unsupportedMediaType415 >> text (fromString err)
        Right (fdf, form) -> do
          let fdf' = FDF.update t1Fields form' fdf
              form' = fixT1 form
          dataDir <- liftIO getDataDir
          liftIO (fdf2pdf (combine dataDir $ combine "T1/pdf" t1FormFile) (Lazy.fromStrict $ serialize fdf')) >>= \case
            Left err -> do
              status internalServerError500
              text (fromString err)
            Right pdf' -> do
              status ok200
              setHeader "Content-Type" "application/fdf"
              raw pdf'
   middleware $ staticPolicy (noDots >-> addBase "web/client/build")
