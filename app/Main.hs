{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<**>), optional)
import Control.Monad (when)
import Control.Monad.Trans.State.Strict (get, put, evalState)
import Data.ByteString qualified as ByteString
import Options.Applicative (Parser, execParser,
                            helper, info, long, metavar, progDesc, short, strArgument, strOption, switch, value)
import System.IO (hPutStrLn, stderr)
import Text.FDF (parse, serialize)

import Tax.FDF qualified as FDF
import Tax.Canada.ON428.FieldNames (on428Fields)
import Tax.Canada.ON428.Fix (fixON428)
import Tax.Canada.T1.FieldNames (t1Fields)
import Tax.Canada.T1.Fix (fixT1)
import Tax.Canada.T1.Types

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
process Options{t1InputPath, on428InputPath, outputPath, verbose} =
   case (t1InputPath, on428InputPath) of
      (Just path, Nothing) -> do
         bytes <- if path == "-" then ByteString.getContents else ByteString.readFile path
         case parse bytes >>= (\x-> (,) x <$> FDF.load t1Fields x) of
            Left err -> error err
            Right (fdf, form) -> do
               let fdf' = FDF.update t1Fields form' fdf
                   form' = fixT1 form
                   write = if outputPath == "-" then ByteString.putStr else ByteString.writeFile outputPath
               when verbose (hPutStrLn stderr $ show form')
               write (serialize fdf')
      (Nothing, Just path) -> do
         bytes <- if path == "-" then ByteString.getContents else ByteString.readFile path
         case parse bytes >>= (\x-> (,) x <$> FDF.load on428Fields x) of
            Left err -> error err
            Right (fdf, form) -> do
               let fdf' = FDF.update on428Fields form' fdf
                   form' = fixON428 form
                   write = if outputPath == "-" then ByteString.putStr else ByteString.writeFile outputPath
               when verbose (hPutStrLn stderr $ show form')
               write (serialize fdf')
