{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry (fileEntry, toTarPath)
import Control.Applicative ((<**>), many, optional)
import Control.Monad (unless, void, when)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as Lazy
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.CAProvinceCodes qualified as Province
import Data.Char (toUpper)
import Data.Foldable (toList)
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.List qualified as List
import Data.Map.Lazy qualified as Map
import Data.Maybe (catMaybes)
import Data.Semigroup.Cancellative (isPrefixOf, isSuffixOf)
import Data.Text qualified as Text
import Options.Applicative (Parser, ReadM, long, metavar, short)
import Options.Applicative qualified as OptsAp
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.FilePath (combine, replaceDirectory, takeFileName)
import Text.FDF (FDF, parse, serialize)

import Paths_canadian_income_tax (getDataDir)
import Tax.Canada (completeAndFilterForms, allFormKeys, relevantFormKeys, formFileNames)
import Tax.Canada.Federal (loadInputForms)
import Tax.Canada.FormKey (FormKey)
import Tax.Canada.FormKey qualified as FormKey
import Tax.PDFtk (fdf2pdf, pdf2fdf)

main :: IO ()
main = OptsAp.execParser (OptsAp.info optionsParser
                          $ OptsAp.progDesc "Update all calculated fields in a Canadian T1 tax form")
       >>= process

data Options = Options {
   province :: Province.Code,
   t1InputPath :: FilePath,
   t4InputPaths :: [FilePath],
   p428InputPath :: Maybe FilePath,
   p479InputPath :: Maybe FilePath,
   schedule6InputPath :: Maybe FilePath,
   schedule7InputPath :: Maybe FilePath,
   schedule8InputPath :: Maybe FilePath,
   schedule9InputPath :: Maybe FilePath,
   schedule11InputPath :: Maybe FilePath,
   outputPath :: FilePath,
   onlyGivenForms :: Bool,
   keepIrrelevantForms :: Bool,
   verbose :: Bool}

optionsParser :: Parser Options
optionsParser =
   Options
   <$> OptsAp.argument readProvince (metavar "<two-letter province code>")
   <*> OptsAp.strOption (long "t1" <> metavar "<input T1 form file>")
   <*> many (OptsAp.strOption (long "t4" <> metavar "<input t4 slip file>"))
   <*> optional (OptsAp.strOption (long "428" <> metavar "<input 428 form file>"))
   <*> optional (OptsAp.strOption (long "479" <> metavar "<input 479 form file>"))
   <*> optional (OptsAp.strOption (long "s6" <> metavar "<input Schedule 6 form file>"))
   <*> optional (OptsAp.strOption (long "s7" <> metavar "<input Schedule 7 form file>"))
   <*> optional (OptsAp.strOption (long "s8" <> metavar "<input Schedule 8 form file>"))
   <*> optional (OptsAp.strOption (long "s9" <> metavar "<input Schedule 9 form file>"))
   <*> optional (OptsAp.strOption (long "s11" <> metavar "<input Schedule 11 form file>"))
   <*> OptsAp.strOption (short 'o' <> long "output" <> OptsAp.value "-" <> metavar "<output file or directory>")
   <*> OptsAp.switch (long "only-given" <> OptsAp.help "Complete only the forms given on the command line")
   <*> OptsAp.switch (long "keep-irrelevant" <> OptsAp.help "Complete the forms that have no effect on T1")
   <*> OptsAp.switch (short 'v' <> long "verbose")
   <**> OptsAp.helper

readProvince :: ReadM Province.Code
readProvince = OptsAp.eitherReader (tryRead . map toUpper)
   where tryRead s = case reads s
                     of [(p, "")] -> Right p
                        _ -> Left ("Invalid province code " <> s <> " - expecting one of "
                                   <> List.intercalate ", " (onLast ("or " <>) $ show <$> Province.all))
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
process Options{province, t1InputPath, t4InputPaths, p428InputPath, p479InputPath,
                schedule6InputPath, schedule7InputPath, schedule8InputPath, schedule9InputPath, schedule11InputPath,
                outputPath, onlyGivenForms, keepIrrelevantForms} = do
   dataDir <- getDataDir
   let inputFiles :: [(FormKey, FilePath)]
       inputFiles = List.sortOn fst $
                    ((,) FormKey.T4 <$> t4InputPaths) <>
                    catMaybes [(,) FormKey.T1  <$> Just t1InputPath,
                               (,) FormKey.Provincial428 <$> p428InputPath,
                               (,) FormKey.Provincial479 <$> p479InputPath,
                               (,) FormKey.Schedule6 <$> schedule6InputPath,
                               (,) FormKey.Schedule7 <$> schedule7InputPath,
                               (,) FormKey.Schedule8 <$> schedule8InputPath,
                               (,) FormKey.Schedule9 <$> schedule9InputPath,
                               (,) FormKey.Schedule11 <$> schedule11InputPath]
       allFiles
         | onlyGivenForms = inputFiles
         | otherwise = Map.toList $ Map.fromAscList inputFiles <> emptyFiles
       emptyFiles = completePath <$> Map.delete FormKey.Provincial479 (formFileNames province)
       completePath baseName = combine dataDir $ combine "pdf" $ Text.unpack baseName <> "-fill-25e.pdf"
   inputs <- traverse (traverse readFDF) allFiles :: IO [(FormKey, (Bool, Lazy.ByteString))]
   let writeFrom :: FormKey -> ByteString.ByteString -> IO ()
       writeFrom key content = do
          let Just inputPath = List.lookup key allFiles
              Just (asPDF, _) = List.lookup key inputs
              fromFDF = if asPDF then (either error Lazy.toStrict <$>) . fdf2pdf inputPath . Lazy.fromStrict else pure
          content' <- fromFDF content
          if outputPath == "-"
             then ByteString.putStr content'
             else do
                isDir <- doesDirectoryExist outputPath
                if isDir
                   then ByteString.writeFile (replaceDirectory inputPath outputPath) content'
                   else ByteString.writeFile outputPath content'
       fdfs = getCompose <$> traverse (parse . Lazy.toStrict . snd) (Compose inputs) :: Either String [(FormKey, FDF)]
   case do (inputFDFs, ioFDFs) <- List.partition ((FormKey.T4 ==) . fst) <$> fdfs
           inputForms <- loadInputForms inputFDFs
           let formKeys = if keepIrrelevantForms then allFormKeys else relevantFormKeys
           completeAndFilterForms formKeys province inputForms (Map.fromAscList ioFDFs)
     of
      Left err -> error err
      Right fixedFDFs -> do
         let bytesMap' = serialize <$> fixedFDFs
             tarEntries = Map.traverseWithKey fdfEntry bytesMap'
             fdfEntry key content
               | Just path <- List.lookup key allFiles
               = (`fileEntry` ByteString.Lazy.fromStrict content) <$> toTarPath False (takeFileName path)
               | otherwise = Left (show key)
             tarFile = either (error . ("Can't tar: " <>)) (ByteString.Lazy.toStrict . Tar.write . toList) tarEntries
         -- when verbose (hPutStrLn stderr $ show (form'T1, form'ON))
         when ("/" `isSuffixOf` outputPath) (createDirectoryIfMissing True outputPath)
         if outputPath == "-"
            then ByteString.putStr tarFile
            else do isDir <- doesDirectoryExist outputPath
                    if isDir
                       then void $ Map.traverseWithKey writeFrom bytesMap'
                       else ByteString.writeFile outputPath tarFile
