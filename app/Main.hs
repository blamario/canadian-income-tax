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
import Data.Maybe (fromMaybe)
import Data.Semigroup (Any (Any))
import Data.Semigroup.Cancellative (isPrefixOf, isSuffixOf)
import Options.Applicative (Parser, execParser,
                            helper, info, long, metavar, progDesc, short, strArgument, strOption, switch, value)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.FilePath (replaceDirectory, takeFileName)
import System.IO (hPutStrLn, stderr)
import System.Process.Typed (ExitCode (ExitFailure, ExitSuccess), byteStringInput, readProcess, setStdin, shell)
import Text.FDF (parse, serialize)

import Tax.FDF qualified as FDF
import Tax.Canada (fixOntarioReturns, fixON428, fixT1, on428Fields, t1Fields)

main :: IO ()
main = execParser (info optionsParser $ progDesc "Update all calculated fields in a Canadian T1 tax form")
       >>= process

data Options = Options {
   t1InputPath :: Maybe FilePath,
   on428InputPath :: Maybe FilePath,
   outputPath :: FilePath,
   verbose :: Bool}


optionsParser :: Parser Options
optionsParser =
   Options
   <$> optional (strOption (long "t1" <> metavar "<input T1 FDF file>"))
   <*> optional (strOption (long "on428" <> metavar "<input ON428 FDF file>"))
   <*> strOption (short 'o' <> long "output" <> value "-" <> metavar "<output FDF file>")
   <*> switch (short 'v' <> long "verbose")
   <**> helper

pdf2fdf :: Lazy.ByteString -> IO Lazy.ByteString
pdf2fdf pdf = do
   (exitCode, fdf, errors) <- readProcess (setStdin (byteStringInput pdf) $ shell "pdftk - generate_fdf output -")
   case exitCode of
      ExitSuccess -> pure fdf
      ExitFailure n -> error ("Error converting PDF to FDF (exit code " <> show n <> ").\n" <> show errors)

fdf2pdf :: FilePath -> Lazy.ByteString -> IO Lazy.ByteString
fdf2pdf pdfPath fdf = do
   (exitCode, pdf, errors)
      <- readProcess (setStdin (byteStringInput fdf) $ shell $ "pdftk " <> pdfPath <> " fill_form - output -")
   case exitCode of
      ExitSuccess -> pure pdf
      ExitFailure n -> error ("Error converting FDF to PDF (exit code " <> show n <> ").\n" <> show errors)

readFDF :: FilePath -> IO (Any, Lazy.ByteString)
readFDF inputPath = do
   exists <- doesFileExist inputPath
   unless (inputPath == "-" || exists) (error $ "Input file " <> show inputPath <> " doesn't exist.")
   content <- if inputPath == "-" then ByteString.Lazy.getContents else ByteString.Lazy.readFile inputPath
   if "%FDF-1." `isPrefixOf` content
      then pure (Any False, content)
      else if "%PDF-1." `isPrefixOf` content
           then (,) (Any True) <$> pdf2fdf content
           else error "Expecting an FDF or PDF file"

readMaybeFDF :: FilePath -> Maybe FilePath -> IO (Maybe (FilePath, Any, ByteString))
readMaybeFDF baseName path = traverse (\p-> addPath p . fmap Lazy.toStrict <$> readFDF p) path
   where addPath "-" (isPDF@(Any True), content) = (baseName <> ".pdf", isPDF, content)
         addPath "-" (isPDF@(Any False), content) = (baseName <> ".fdf", isPDF, content)
         addPath p (isPDF, content) = (p, isPDF, content)

process :: Options -> IO ()
process Options{t1InputPath, on428InputPath, outputPath, verbose} = do
   [t1, on428] <- traverse (uncurry readMaybeFDF) [("t1", t1InputPath), ("on428", on428InputPath)]
   when ("/" `isSuffixOf` outputPath) (createDirectoryIfMissing True outputPath)
   let writeFrom :: FilePath -> Bool -> ByteString.ByteString -> IO ()
       writeFrom inputPath asPDF content = do
          content' <- (if asPDF then fmap Lazy.toStrict . fdf2pdf inputPath . Lazy.fromStrict else pure) content
          if outputPath == "-"
             then ByteString.putStr content'
             else do
                isDir <- doesDirectoryExist outputPath
                if isDir
                   then ByteString.writeFile (replaceDirectory inputPath outputPath) content'
                   else ByteString.writeFile outputPath content'
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
