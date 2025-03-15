{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Codec.Archive.Zip (addEntryToArchive, emptyArchive, fromArchive, toEntry)
import Control.Category ((>>>))
import Control.Monad (forM, join)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (assert)
import Data.Aeson (decode)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as Lazy
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Fixed (Centi)
import Data.Foldable (fold, toList)
import Data.Functor.Compose (Compose(..))
import Data.List qualified as List
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import Data.Monoid.Textual (fromText, toString)
import Data.Monoid.Instances.ByteString.UTF8 (ByteStringUTF8(..))
import Data.String (fromString)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Network.HTTP.Types.Status (statusCode, ok200, internalServerError500,
                                  notFound404, unsupportedMediaType415, unprocessableEntity422)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Parse (FileInfo (..))
import System.Directory (removeDirectoryRecursive)
import System.FilePath.Posix ((</>))
import System.Log.FastLogger (FastLogger, LogType'(LogFile, LogStdout), FileLogSpec(..), LogStr,
                              toLogStr, withTimedFastLogger)
import System.Log.FastLogger.Date (newTimeCache, simpleTimeFormat)
import System.Posix.Temp (mkdtemp)
import Text.FDF qualified as FDF (mapWithKey, parse, serialize)
import Text.Read (readMaybe)
import Web.Scotty (file, files, finish, formParamMaybe, get, middleware, pathParam, post, raw,
                   scotty, setHeader, status, text)

import Paths_canadian_income_tax (getDataDir)
import Tax.Canada (completeRelevantForms)
import Tax.Canada.Federal qualified as Federal
import Tax.Canada.FormKey (FormKey)
import Tax.Canada.FormKey qualified as FormKey
import Tax.FDF qualified as FDF
import Tax.PDFtk (fdf2pdf, pdfFile2fdf)

main :: IO ()
main = do
  dataDir <- getDataDir
  t4fdfBytes <- ByteString.readFile (dataDir </> "fdf" </> "t4-fill-24e.fdf")
  let readEmptyForm baseName = (,) completeName <$> ByteString.Lazy.readFile completePath
        where completePath = dataDir </> "pdf" </> completeName
              completeName = Text.unpack baseName <> "-fill-24e.pdf"
  emptyForms <- traverse readEmptyForm Federal.formFileNames
  let t4fdf = case FDF.parse t4fdfBytes of
        Left err -> error ("Can't load built-in T4 FDF: " <> err)
        Right parsed -> parsed
      logDestination = LogFile (FileLogSpec "taxell.log" (2^(20::Int)) 16) 1024
  timer <- newTimeCache simpleTimeFormat
  withTimedFastLogger timer logDestination
    $ \log-> let log' msg = liftIO $ log (\timestamp-> toLogStr timestamp <> " - " <> msg <> "\n") in (log' "Started" >>)
    $ scotty 3000 $ do
   middleware logStdoutDev
   get "/" $ do
      setHeader "Content-Type" "text/html; charset=utf-8"
      file "web/client/build/index.html"
   get "/about.html" $ do
      file "web/static/about.html"
   get "/shared.css" $ do
      file "web/static/shared.css"
   post "/save/:province" $ do
      provinceCode <- pathParam "province"
      t4param <- formParamMaybe "T4"
      pdfFiles <- files
      log' ("Save " <> toLogStr provinceCode <> ": "
            <> maybe "no" (const "with") t4param <> " T4s, " <> toLogStr (show (fst <$> pdfFiles)))
      now <- liftIO $ round . nominalDiffTimeToSeconds <$> getPOSIXTime
      let pdfArchive = foldr addPDF emptyArchive pdfFiles
          addPDF (_, FileInfo name _ c) = addEntryToArchive (toEntry (fromUTF8 name) now c)
          ByteStringUTF8 province = fromText provinceCode
          completeArchive = fromArchive
                            $ addEntryToArchive (toEntry "province" now $ Lazy.fromStrict province)
                            $ maybe id (addEntryToArchive . toEntry "T4s" now) t4param
                            $ pdfArchive
      setHeader "Content-Type" "application/zip"
      setHeader "Content-Disposition" "attachment; filename=\"taxell-save.zip\"; filename*=\"taxell-save.zip\""
      raw completeArchive
   post "/t1/PDF/:province" $ do
      provinceCode <- pathParam "province"
      province <- case readMaybe (Text.Lazy.unpack provinceCode)
                  of Nothing -> status notFound404
                                >> text ("No such province as " <> provinceCode)
                                >> finish
                     Just p -> pure p
      t4param <- formParamMaybe "T4"
      let t4m = Map.mapKeysMonotonic (\k-> "Box" <> k <> "[0]") <$> fold (t4param >>= decode) :: [Map Text Centi]
          resolveForm (key, FileInfo name _ content)
            | Just formKey <- readMaybe (Text.unpack key) = Right (formKey, (fromUTF8 name, content))
            | otherwise = Left key
      pdfFiles <- files >>= \fs-> case traverse resolveForm fs of
        Left name -> status notFound404
                     >> text ("No such form key as " <> Text.Lazy.fromStrict name)
                     >> finish
        Right fs -> pure fs
      log' ("Complete " <> toLogStr provinceCode <> ": "
            <> toLogStr (length t4m) <> " T4s, " <> toLogStr (show (fst <$> pdfFiles)))
      let allPdfFiles = Map.toList (Map.fromList pdfFiles <> emptyForms)
      dir <- liftIO $ mkdtemp "tax"
      fdfBytes <- liftIO $ fmap sequenceA $ forM allPdfFiles $ \(key, (name, content))-> do
        let path = dir </> name
        Lazy.writeFile path content
        fdf <- pdfFile2fdf path
        pure ((,) key <$> fdf)
      case do fdfs <- first ((,) unsupportedMediaType415)
                      $ fdfBytes >>= traverse (traverse $ Lazy.toStrict >>> FDF.parse)
              let (inputFDFs, ioFDFs) = List.partition ((FormKey.T4 ==) . fst) fdfs
                  inputT4s = foldMap injectT4 t4m
                  injectT4 values = [(FormKey.T4, FDF.mapWithKey injectT4box t4fdf)]
                    where injectT4box keyPath ""
                            | Just v <- List.find (Text.isPrefixOf "Box") keyPath >>= (`Map.lookup` values)
                            = Text.pack $ show v
                          injectT4box _ v = v
              inputForms <- first ((,) unprocessableEntity422) $ Federal.loadInputForms $ inputFDFs <> inputT4s
              first ((,) unprocessableEntity422) $ completeRelevantForms province inputForms (Map.fromList fdfs)
        of Left (code, err) ->
             log' ("Error " <> toLogStr code.statusCode <> ": " <> toLogStr err)
             >> status code >> text (fromString err)
           Right fdfs' -> do
             log' ("Completed " <> toLogStr provinceCode <> ": " <> toLogStr (show (Map.keys fdfs')))
             let fdfBytes' = Lazy.fromStrict . FDF.serialize <$> fdfs'
                 replaceContent :: FormKey -> Lazy.ByteString
                                -> IO (Either String (FilePath, Lazy.ByteString))
                 replaceContent key content = case List.lookup key allPdfFiles of
                   Just (name, _) -> ((,) name <$>) <$> fdf2pdf (dir </> name) content
                   Nothing -> pure (Left $ "Unknown key " <> show key)
             pdfFiles' <- liftIO $ Map.traverseWithKey replaceContent fdfBytes'
             case toList <$> sequenceA pdfFiles' of
               Left err -> do
                 log' ("Error 500: " <> toLogStr err)
                 status internalServerError500 >> text (fromString err)
               Right [(name, pdf)] -> do
                 status ok200
                 setHeader "Content-Type" "application/pdf"
                 setHeader "Content-Disposition" ("attachment; filename=\"" <> Text.Lazy.pack name
                                                  <> "\"; filename*=\"" <> Text.Lazy.pack name <> "\"")
                 raw pdf
               Right pdfFiles'' -> do
                 now <- liftIO $ round . nominalDiffTimeToSeconds <$> getPOSIXTime
                 let pdfArchive = foldr addPDF emptyArchive pdfFiles''
                     addPDF (name, c) = addEntryToArchive (toEntry name now c)
                 status ok200
                 setHeader "Content-Type" "application/zip"
                 raw (fromArchive pdfArchive)
      liftIO $ removeDirectoryRecursive dir
   middleware $ staticPolicy (noDots >-> addBase "web/client/build")

fromUTF8 :: ByteString -> String
fromUTF8 = toString mempty . ByteStringUTF8
