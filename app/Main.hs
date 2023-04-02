{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<**>))
import Control.Monad (when)
import Control.Monad.Trans.State.Strict (get, put, evalState)
import Data.ByteString qualified as ByteString
import Options.Applicative (Parser, execParser,
                            helper, info, long, metavar, progDesc, short, strArgument, strOption, switch, value)
import System.IO (hPutStrLn, stderr)
import Text.FDF (parse, serialize)

import Tax.Canada.T1.FDF qualified as FDF
import Tax.Canada.T1.FieldNames
import Tax.Canada.T1.Fix (fixT1)
import Tax.Canada.T1.Types

main :: IO ()
main = execParser (info optionsParser $ progDesc "Update all calculated fields in a Canadian T1 tax form")
       >>= process

data Options = Options {
   inputPath :: FilePath,
   outputPath :: FilePath,
   verbose :: Bool}


optionsParser :: Parser Options
optionsParser =
   Options
   <$> strArgument (metavar "<input FDF file>")
   <*> strOption (short 'o' <> long "output" <> value "-" <> metavar "<output FDF file>")
   <*> switch (short 'v' <> long "verbose")
   <**> helper


process :: Options -> IO ()
process Options{inputPath, outputPath, verbose} = do
   bytes <- if inputPath == "-" then ByteString.getContents else ByteString.readFile inputPath
   case parse bytes >>= (\x-> (,) x <$> FDF.load x) of
      Left err -> error err
      Right (fdf, t1) -> do
         let fdf' = FDF.update t1' fdf
             t1' = fixT1 t1
             write = if outputPath == "-" then ByteString.putStr else ByteString.writeFile outputPath
         when verbose (hPutStrLn stderr $ show t1')
         write (serialize fdf')
