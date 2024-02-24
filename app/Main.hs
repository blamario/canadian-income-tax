{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry (fileEntry, toTarPath)
import Control.Applicative ((<**>), optional)
import Control.Arrow ((&&&))
import Control.Monad (unless, void, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as Lazy
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.CAProvinceCodes qualified as Province
import Data.Char (toUpper)
import Data.Foldable (toList)
import Data.List (intercalate, sortOn)
import Data.Map.Lazy qualified as Map
import Data.Maybe (catMaybes)
import Data.Semigroup (Any (Any))
import Data.Semigroup.Cancellative (isPrefixOf, isSuffixOf)
import Data.Text (Text)
import Options.Applicative (Parser, ReadM, long, metavar, short)
import Options.Applicative qualified as OptsAp
import Rank2 qualified
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.FilePath (replaceDirectory, takeFileName)
import System.IO (hPutStrLn, stderr)
import Text.FDF (FDF, parse, serialize)

import Tax.Canada (completeForms)
import Tax.FDF (FDFs)
import Tax.FDF qualified as FDF
import Tax.PDFtk (fdf2pdf, pdf2fdf)

main :: IO ()
main = OptsAp.execParser (OptsAp.info optionsParser
                          $ OptsAp.progDesc "Update all calculated fields in a Canadian T1 tax form")
       >>= process

data Options = Options {
   province :: Province.Code,
   t1InputPath :: FilePath,
   p428InputPath :: Maybe FilePath,
   p479InputPath :: Maybe FilePath,
   schedule6InputPath :: Maybe FilePath,
   schedule7InputPath :: Maybe FilePath,
   schedule9InputPath :: Maybe FilePath,
   schedule11InputPath :: Maybe FilePath,
   outputPath :: FilePath,
   verbose :: Bool}

optionsParser :: Parser Options
optionsParser =
   Options
   <$> OptsAp.argument readProvince (metavar "<two-letter province code>")
   <*> OptsAp.strOption (long "t1" <> metavar "<input T1 form file>")
   <*> optional (OptsAp.strOption (long "428" <> metavar "<input 428 form file>"))
   <*> optional (OptsAp.strOption (long "479" <> metavar "<input 479 form file>"))
   <*> optional (OptsAp.strOption (long "s6" <> metavar "<input Schedule 6 form file>"))
   <*> optional (OptsAp.strOption (long "s7" <> metavar "<input Schedule 7 form file>"))
   <*> optional (OptsAp.strOption (long "s9" <> metavar "<input Schedule 9 form file>"))
   <*> optional (OptsAp.strOption (long "s11" <> metavar "<input Schedule 11 form file>"))
   <*> OptsAp.strOption (short 'o' <> long "output" <> OptsAp.value "-" <> metavar "<output file or directory>")
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

readFDF :: FilePath -> IO (Bool, Lazy.ByteString)
readFDF inputPath = do
   exists <- doesFileExist inputPath
   unless (inputPath == "-" || exists) (error $ "Input file " <> show inputPath <> " doesn't exist.")
   content <- if inputPath == "-" then ByteString.Lazy.getContents else ByteString.Lazy.readFile inputPath
   if "%FDF-1." `isPrefixOf` content
      then pure (False, content)
      else if "%PDF-1." `isPrefixOf` content
           then either error ((,) True) <$> pdf2fdf content
           else error "Expecting an FDF or PDF file"

process :: Options -> IO ()
process Options{province, t1InputPath, p428InputPath, p479InputPath,
                schedule6InputPath, schedule7InputPath, schedule9InputPath, schedule11InputPath,
                outputPath, verbose} = do
   let inputFiles :: [(Text, FilePath)]
       inputFiles = sortOn fst $
                    catMaybes [(,) "T1"  <$> Just t1InputPath,
                               (,) "428" <$> p428InputPath,
                               (,) "479" <$> p479InputPath,
                               (,) "Schedule6" <$> schedule6InputPath,
                               (,) "Schedule7" <$> schedule7InputPath,
                               (,) "Schedule9" <$> schedule9InputPath,
                               (,) "Schedule11" <$> schedule11InputPath]
   inputs <- traverse (traverse readFDF) inputFiles :: IO [(Text, (Bool, Lazy.ByteString))]
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
       paths = snd <$> inputFiles :: [FilePath]
       arePDFs = fst . snd <$> inputs
       bytesMap = Lazy.toStrict . snd <$> Map.fromAscList inputs
   case traverse parse bytesMap >>= completeForms province of
      Left err -> error err
      Right fixedFDFs -> do
         let bytesMap' = serialize <$> fixedFDFs
             byteses' = toList bytesMap'
             tarEntries = sequenceA (zipWith fdfEntry paths byteses')
             fdfEntry path content =
                (`fileEntry` ByteString.Lazy.fromStrict content) <$> toTarPath False (takeFileName path)
             tarFile = either (error . ("Can't tar: " <>)) (ByteString.Lazy.toStrict . Tar.write) tarEntries
         -- when verbose (hPutStrLn stderr $ show (form'T1, form'ON))
         when ("/" `isSuffixOf` outputPath) (createDirectoryIfMissing True outputPath)
         if outputPath == "-"
            then ByteString.putStr tarFile
            else do isDir <- doesDirectoryExist outputPath
                    if isDir
                       then void $ sequenceA (zipWith3 writeFrom paths arePDFs byteses')
                       else ByteString.writeFile outputPath tarFile
