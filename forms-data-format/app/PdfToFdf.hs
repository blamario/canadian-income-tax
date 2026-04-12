{-# LANGUAGE Haskell2010, ImportQualifiedPost, OverloadedStrings #-}

-- | Convert a PDF file with AcroForm fields into an FDF file.

module Main (main) where

import Control.Applicative ((<|>))
import Data.ByteString qualified as ByteString
import Options.Applicative qualified as OptsAp

import Text.FDF qualified as FDF
import Text.FDF.PDF (PDF (form), parsePDF)

data Options = Options
  { input  :: FilePath
  , output :: FilePath
  }

optionsParser :: OptsAp.Parser Options
optionsParser = Options
  <$> OptsAp.strArgument
        (OptsAp.metavar "<input.pdf>"
         <> OptsAp.help "Input PDF file (use - for stdin)")
  <*> (OptsAp.strArgument
         (OptsAp.metavar "<output.fdf>"
          <> OptsAp.help "Output FDF file (default: write to stdout)")
       <|> pure "-")

main :: IO ()
main = do
  opts <- OptsAp.execParser $
    OptsAp.info (optionsParser OptsAp.<**> OptsAp.helper)
      (OptsAp.fullDesc
       <> OptsAp.progDesc "Extract AcroForm field data from a PDF into an FDF file"
       <> OptsAp.header "pdf-to-fdf - convert PDF to FDF")
  pdfBytes <- if input opts == "-"
                then ByteString.getContents
                else ByteString.readFile (input opts)
  case parsePDF pdfBytes of
    Left err  -> ioError (userError $ "Error reading PDF: " <> err)
    Right pdf -> do
      let fdfBytes = FDF.serialize (form pdf)
      if output opts == "-"
        then ByteString.putStr fdfBytes
        else ByteString.writeFile (output opts) fdfBytes
