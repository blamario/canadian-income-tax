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
import Data.Text (Text)
import Options.Applicative (Parser, ReadM, long, metavar, short)
import Options.Applicative qualified as OptsAp
import Rank2 qualified
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.FilePath (replaceDirectory, takeFileName)
import System.IO (hPutStrLn, stderr)
import Text.FDF (FDF, parse, serialize)

import Tax.Canada.T1 (T1, fixT1, t1FieldsForProvince)
import Tax.Canada.Province.AB qualified as AB
import Tax.Canada.Province.BC qualified as BC
import Tax.Canada.Province.MB qualified as MB
import Tax.Canada.Province.ON qualified as ON
import Tax.FDF qualified as FDF
import Tax.PDFtk (fdf2pdf, pdf2fdf)

main :: IO ()
main = OptsAp.execParser (OptsAp.info optionsParser
                          $ OptsAp.progDesc "Update all calculated fields in a Canadian T1 tax form")
       >>= process

data Options = Options {
   province :: Province.Code,
   t1InputPath :: Maybe FilePath,
   p428InputPath :: Maybe FilePath,
   outputPath :: FilePath,
   verbose :: Bool}

optionsParser :: Parser Options
optionsParser =
   Options
   <$> OptsAp.argument readProvince (metavar "<two-letter province code>")
   <*> optional (OptsAp.strOption (long "t1" <> metavar "<input T1 form file>"))
   <*> optional (OptsAp.strOption (long "428" <> metavar "<input 428 form file>"))
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

fix428fdf :: Province.Code -> FDF -> Either String FDF
fix428fdf Province.AB = FDF.mapForm AB.ab428Fields AB.fixAB428
fix428fdf Province.BC = FDF.mapForm BC.bc428Fields BC.fixBC428
fix428fdf Province.MB = FDF.mapForm MB.mb428Fields MB.fixMB428
fix428fdf Province.ON = FDF.mapForm ON.returnFields.on428 ON.fixON428

fix428t1fdfs :: Province.Code -> T1 FDF.FieldConst -> (FDF, FDF) -> Either String (FDF, FDF)
fix428t1fdfs Province.AB t1Fields = FDF.mapForm2 (t1Fields, AB.ab428Fields) AB.fixReturns
fix428t1fdfs Province.BC t1Fields = FDF.mapForm2 (t1Fields, BC.bc428Fields) BC.fixReturns
fix428t1fdfs Province.MB t1Fields = FDF.mapForm2 (t1Fields, MB.mb428Fields) MB.fixReturns
fix428t1fdfs Province.ON _ = FDF.mapForm2 (ON.returnFields.t1, ON.returnFields.on428) fix2
  where fix2 (t1, on428) = ((.t1) &&& (.on428)) $
                           ON.fixReturns ON.Returns{ON.t1, ON.on428, ON.on479 = Rank2.pure Nothing}

process :: Options -> IO ()
process Options{province, t1InputPath, p428InputPath, outputPath, verbose} = do
   [t1, p428] <- traverse (uncurry readMaybeFDF) [("t1", t1InputPath), ("428", p428InputPath)]
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
       t1Fields = t1FieldsForProvince province
   case (t1, p428) of
      (Nothing, Nothing) -> error "You must specify a T1 form, provincial 428 form, or both."
      (Just (t1Path, Any t1isPDF, t1bytes), Nothing) -> do
         case parse t1bytes >>= FDF.mapForm t1Fields fixT1 of
            Left err -> error err
            Right fdf' -> writeFrom t1Path t1isPDF (serialize fdf')
      (Nothing, Just (p428Path, Any p428isPDF, bytes428)) -> do
         case parse bytes428 >>= fix428fdf province of
            Left err -> error err
            Right fdf' -> writeFrom p428Path p428isPDF (serialize fdf')
      (Just (t1Path, Any t1isPDF, t1bytes), Just (p428path, Any p428isPDF, bytes428)) -> do
        case (,) <$> parse t1bytes <*> parse bytes428 >>= fix428t1fdfs province t1Fields of
            Left err -> error err
            Right (fdfT1, fdf428) -> do
               let bytesT1' = serialize fdfT1
                   bytes428' = serialize fdf428
                   fdfEntry path content =
                      (`fileEntry` ByteString.Lazy.fromStrict content) <$> toTarPath False (takeFileName path)
                   tarEntries = sequenceA [fdfEntry (fromMaybe "t1.fdf" t1InputPath) bytesT1',
                                           fdfEntry (fromMaybe "p428.fdf" p428InputPath) bytes428']
                   tarFile = either (error . ("Can't tar: " <>)) (ByteString.Lazy.toStrict . Tar.write) tarEntries 
               -- when verbose (hPutStrLn stderr $ show (form'T1, form'ON))
               if outputPath == "-"
                  then ByteString.putStr tarFile
                  else do isDir <- doesDirectoryExist outputPath
                          if isDir
                             then do writeFrom t1Path t1isPDF bytesT1'
                                     writeFrom p428path p428isPDF bytes428'
                             else ByteString.writeFile outputPath tarFile
