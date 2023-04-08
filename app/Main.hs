{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry (fileEntry, toTarPath)
import Control.Applicative ((<**>), optional)
import Control.Monad (when)
import Control.Monad.Trans.State.Strict (get, put, evalState)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Options.Applicative (Parser, execParser,
                            helper, info, long, metavar, progDesc, short, strArgument, strOption, switch, value)
import System.Directory (doesDirectoryExist)
import System.FilePath (replaceDirectory, takeFileName)
import System.IO (hPutStrLn, stderr)
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


process :: Options -> IO ()
process Options{t1InputPath, on428InputPath, outputPath, verbose} = do
   let read path = if path == "-" then ByteString.getContents else ByteString.readFile path
       writeFrom inputPath content =
          if outputPath == "-"
          then ByteString.putStr content
          else do isDir <- doesDirectoryExist outputPath
                  if isDir
                     then ByteString.writeFile (replaceDirectory inputPath outputPath) content
                     else ByteString.writeFile outputPath content
   case (t1InputPath, on428InputPath) of
      (Nothing, Nothing) -> error "You must specify a T1 form, ON428 form, or both."
      (Just path, Nothing) -> do
         bytes <- read path
         case parse bytes >>= \x-> (,) x <$> FDF.load t1Fields x of
            Left err -> error err
            Right (fdf, form) -> do
               let fdf' = FDF.update t1Fields form' fdf
                   form' = fixT1 form
               when verbose (hPutStrLn stderr $ show form')
               writeFrom path (serialize fdf')
      (Nothing, Just path) -> do
         bytes <- read path
         case parse bytes >>= \x-> (,) x <$> FDF.load on428Fields x of
            Left err -> error err
            Right (fdf, form) -> do
               let fdf' = FDF.update on428Fields form' fdf
                   form' = fixON428 form
               when verbose (hPutStrLn stderr $ show form')
               writeFrom path (serialize fdf')
      (Just pathT1, Just pathON) -> do
         bytesT1 <- read pathT1
         bytesON <- read pathON
         case (,) <$> (parse bytesT1 >>= \x-> (,) x <$> FDF.load t1Fields x)
                  <*> (parse bytesON >>= \x-> (,) x <$> FDF.load on428Fields x) of
            Left err -> error err
            Right ((fdfT1, formT1), (fdfON, formON)) -> do
               let fdf'T1 = serialize $ FDF.update t1Fields form'T1 fdfT1
                   fdf'ON = serialize $ FDF.update on428Fields form'ON fdfON
                   (form'T1, form'ON) = fixOntarioReturns (formT1, formON)
                   fdfEntry path content =
                      (`fileEntry` ByteString.Lazy.fromStrict content) <$> toTarPath False (takeFileName path)
                   tarEntries = sequenceA [fdfEntry pathT1 fdf'T1,
                                           fdfEntry pathON fdf'ON]
                   tarFile = either (error . ("Can't tar: " <>)) (ByteString.Lazy.toStrict . Tar.write) tarEntries 
               when verbose (hPutStrLn stderr $ show (form'T1, form'ON))
               if outputPath == "-"
                  then ByteString.putStr tarFile
                  else do isDir <- doesDirectoryExist outputPath
                          if isDir
                             then do writeFrom pathT1 fdf'T1
                                     writeFrom pathON fdf'ON
                             else ByteString.writeFile outputPath tarFile
