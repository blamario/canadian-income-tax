{-# LANGUAGE Haskell2010, ImportQualifiedPost, OverloadedStrings #-}

-- | Fill the AcroForm fields of a PDF using an FDF file.

module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.ByteString qualified as ByteString
import System.IO (hSetBinaryMode, stdin)
import Options.Applicative qualified as OptsAp

import Text.FDF qualified as FDF
import Text.FDF.PDF (fillPDF, parsePDF, serializePDF)

data Options = Options
  { fdfInput  :: FilePath
  , pdfInput  :: FilePath
  , pdfOutput :: FilePath
  }

optionsParser :: OptsAp.Parser Options
optionsParser = Options
  <$> OptsAp.strArgument
        (OptsAp.metavar "<input.fdf>"
         <> OptsAp.help "FDF file containing form field values")
  <*> OptsAp.strArgument
        (OptsAp.metavar "<template.pdf>"
         <> OptsAp.help "PDF template file whose AcroForm fields are to be filled")
  <*> (OptsAp.strArgument
         (OptsAp.metavar "<output.pdf>"
          <> OptsAp.help "Output PDF file (default: write to stdout)")
       <|> pure "-")

throwError :: String -> IO a
throwError = ioError . userError

-- | Read a file, or read from stdin if the path is @"-"@.
readFileOrStdin :: FilePath -> IO ByteString.ByteString
readFileOrStdin "-" = do
  hSetBinaryMode stdin True
  ByteString.getContents
readFileOrStdin path = ByteString.readFile path

main :: IO ()
main = do
  opts <- OptsAp.execParser $
    OptsAp.info (optionsParser OptsAp.<**> OptsAp.helper)
      (OptsAp.fullDesc
       <> OptsAp.progDesc "Fill AcroForm fields of a PDF template with values from an FDF file"
       <> OptsAp.header "fdf-to-pdf - fill PDF from FDF")
  when (fdfInput opts == "-" && pdfInput opts == "-") $
    throwError "Only one of the two input arguments may be \"-\" (stdin)"
  fdfBytes <- readFileOrStdin (fdfInput opts)
  pdfBytes <- readFileOrStdin (pdfInput opts)
  fdf <- case FDF.parse fdfBytes of
    Left err  -> throwError $ "Error parsing FDF: " <> err
    Right fdf -> return fdf
  pdf <- case parsePDF pdfBytes of
    Left err  -> throwError $ "Error parsing PDF: " <> err
    Right pdf -> return pdf
  case fillPDF fdf pdf of
    Left err     -> throwError $ "Error filling PDF: " <> err
    Right filled ->
      if pdfOutput opts == "-"
        then ByteString.putStr (serializePDF filled)
        else ByteString.writeFile (pdfOutput opts) (serializePDF filled)
