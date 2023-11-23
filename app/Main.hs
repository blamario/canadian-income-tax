{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry (fileEntry, toTarPath)
import Control.Applicative ((<**>), optional)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as Lazy
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.CAProvinceCodes qualified as Province
import Data.Char (toUpper)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Any (Any))
import Data.Semigroup.Cancellative (isPrefixOf, isSuffixOf)
import Options.Applicative (Parser, ReadM, long, metavar, short)
import Options.Applicative qualified as OptsAp
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.FilePath (replaceDirectory, takeFileName)
import System.IO (hPutStrLn, stderr)
import Text.FDF (parse, serialize)

import Tax.Canada (fixOntarioReturns, fixON428, fixT1, on428Fields)
import Tax.Canada.T1.FieldNames.AB qualified as AB
import Tax.Canada.T1.FieldNames.BC qualified as BC
import Tax.Canada.T1.FieldNames qualified as ON
import Tax.Canada.T1.FieldNames.NB qualified as NB
import Tax.Canada.T1.FieldNames.NT qualified as NT
import Tax.Canada.T1.FieldNames.NU qualified as NU
import Tax.Canada.T1.FieldNames.NL qualified as NL
import Tax.Canada.T1.FieldNames.QC qualified as QC
import Tax.Canada.T1.FieldNames.YT qualified as YT
import Tax.FDF qualified as FDF
import Tax.PDFtk (fdf2pdf, pdf2fdf)

main :: IO ()
main = OptsAp.execParser (OptsAp.info optionsParser
                          $ OptsAp.progDesc "Update all calculated fields in a Canadian T1 tax form")
       >>= process

data Options = Options {
   province :: Province.Code,
   t1InputPath :: Maybe FilePath,
   on428InputPath :: Maybe FilePath,
   outputPath :: FilePath,
   verbose :: Bool}

optionsParser :: Parser Options
optionsParser =
   Options
   <$> OptsAp.argument readProvince (metavar "<two-letter province code>")
   <*> optional (OptsAp.strOption (long "t1" <> metavar "<input T1 file>"))
   <*> optional (OptsAp.strOption (long "on428" <> metavar "<input ON428 file>"))
   <*> OptsAp.strOption (short 'o' <> long "output" <> OptsAp.value "-" <> metavar "<output FDF file>")
   <*> OptsAp.switch (short 'v' <> long "verbose")
   <**> OptsAp.helper

readProvince :: ReadM Province.Code
readProvince = OptsAp.eitherReader (tryRead . map toUpper)
   where tryRead s = case reads s
                     of [(p, "")] -> Right p
                        _ -> Left ("Invalid province code " <> s <> " - expecting one of "
                                   <> intercalate ", " (onLast ("or " <>) $ show <$> Province.all))
         onLast f [x] = [f x]
         onLast f (x:xs) = x : onLast f xs
         onLast _ [] = []

readFDF :: FilePath -> IO (Any, Lazy.ByteString)
readFDF inputPath = do
   exists <- doesFileExist inputPath
   unless (inputPath == "-" || exists) (error $ "Input file " <> show inputPath <> " doesn't exist.")
   content <- if inputPath == "-" then ByteString.Lazy.getContents else ByteString.Lazy.readFile inputPath
   if "%FDF-1." `isPrefixOf` content
      then pure (Any False, content)
      else if "%PDF-1." `isPrefixOf` content
           then either error ((,) (Any True)) <$> pdf2fdf content
           else error "Expecting an FDF or PDF file"

readMaybeFDF :: FilePath -> Maybe FilePath -> IO (Maybe (FilePath, Any, ByteString))
readMaybeFDF baseName path = traverse (\p-> addPath p . fmap Lazy.toStrict <$> readFDF p) path
   where addPath "-" (isPDF@(Any True), content) = (baseName <> ".pdf", isPDF, content)
         addPath "-" (isPDF@(Any False), content) = (baseName <> ".fdf", isPDF, content)
         addPath p (isPDF, content) = (p, isPDF, content)

process :: Options -> IO ()
process Options{province, t1InputPath, on428InputPath, outputPath, verbose} = do
   [t1, on428] <- traverse (uncurry readMaybeFDF) [("t1", t1InputPath), ("on428", on428InputPath)]
   when ("/" `isSuffixOf` outputPath) (createDirectoryIfMissing True outputPath)
   let writeFrom :: FilePath -> Bool -> ByteString.ByteString -> IO ()
       writeFrom inputPath asPDF content = do
          content' <- (if asPDF then (either error Lazy.toStrict <$>) . fdf2pdf inputPath . Lazy.fromStrict else pure) content
          if outputPath == "-"
             then ByteString.putStr content'
             else do
                isDir <- doesDirectoryExist outputPath
                if isDir
                   then ByteString.writeFile (replaceDirectory inputPath outputPath) content'
                   else ByteString.writeFile outputPath content'
   let t1Fields = case province of
         Province.AB -> AB.t1Fields
         Province.BC -> BC.t1Fields
         Province.MB -> AB.t1Fields
         Province.NB -> NB.t1Fields
         Province.NL -> NL.t1Fields
         Province.NS -> AB.t1Fields
         Province.NT -> NT.t1Fields
         Province.NU -> NU.t1Fields
         Province.ON -> ON.t1Fields
         Province.PE -> NB.t1Fields
         Province.QC -> QC.t1Fields
         Province.SK -> AB.t1Fields
         Province.YT -> YT.t1Fields
   case (t1, on428) of
      (Nothing, Nothing) -> error "You must specify a T1 form, ON428 form, or both."
      (Just (t1Path, Any t1isPDF, t1bytes), Nothing) -> do
         case parse t1bytes >>= \x-> (,) x <$> FDF.load t1Fields x of
            Left err -> error err
            Right (fdf, form) -> do
               let fdf' = FDF.update t1Fields form' fdf
                   form' = fixT1 form
               when verbose (hPutStrLn stderr $ show form')
               writeFrom t1Path t1isPDF (serialize fdf')
      (Nothing, Just (on428Path, Any on428isPDF, on428bytes)) -> do
         case parse on428bytes >>= \x-> (,) x <$> FDF.load on428Fields x of
            Left err -> error err
            Right (fdf, form) -> do
               let fdf' = FDF.update on428Fields form' fdf
                   form' = fixON428 form
               when verbose (hPutStrLn stderr $ show form')
               writeFrom on428Path on428isPDF (serialize fdf')
      (Just (t1Path, Any t1isPDF, t1bytes), Just (on428path, Any on428isPDF, on428bytes)) -> do
         case (,) <$> (parse t1bytes >>= \x-> (,) x <$> FDF.load t1Fields x)
                  <*> (parse on428bytes >>= \x-> (,) x <$> FDF.load on428Fields x) of
            Left err -> error err
            Right ((fdfT1, formT1), (fdfON, formON)) -> do
               let fdf'T1 = serialize $ FDF.update t1Fields form'T1 fdfT1
                   fdf'ON = serialize $ FDF.update on428Fields form'ON fdfON
                   (form'T1, form'ON) = fixOntarioReturns (formT1, formON)
                   fdfEntry path content =
                      (`fileEntry` ByteString.Lazy.fromStrict content) <$> toTarPath False (takeFileName path)
                   tarEntries = sequenceA [fdfEntry (fromMaybe "t1.fdf" t1InputPath) fdf'T1,
                                           fdfEntry (fromMaybe "on428.fdf" on428InputPath) fdf'ON]
                   tarFile = either (error . ("Can't tar: " <>)) (ByteString.Lazy.toStrict . Tar.write) tarEntries 
               when verbose (hPutStrLn stderr $ show (form'T1, form'ON))
               if outputPath == "-"
                  then ByteString.putStr tarFile
                  else do isDir <- doesDirectoryExist outputPath
                          if isDir
                             then do writeFrom t1Path t1isPDF fdf'T1
                                     writeFrom on428path on428isPDF fdf'ON
                             else ByteString.writeFile outputPath tarFile
