{-# LANGUAGE Haskell2010, ImportQualifiedPost, OverloadedStrings #-}

-- | Convert a PDF file with AcroForm fields into an FDF file containing
-- the text labels drawn near each form field, rather than their values.

module Main (main) where

import Control.Applicative ((<|>))
import Data.ByteString qualified as ByteString
import Options.Applicative qualified as OptsAp

import Text.FDF (FDF (..), Field (..), FieldContent (..))
import Text.FDF qualified as FDF
import Text.FDF.PDF (parsePDF, fieldLabels)

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
       <> OptsAp.progDesc "Extract text labels near AcroForm fields from a PDF into an FDF file"
       <> OptsAp.header "pdf-to-labels - extract form field labels from PDF")
  pdfBytes <- if input opts == "-"
                then ByteString.getContents
                else ByteString.readFile (input opts)
  pdf <- case parsePDF pdfBytes of
    Left err  -> ioError (userError $ "Error reading PDF: " <> err)
    Right pdf -> return pdf
  labels <- case fieldLabels pdf of
    Left err  -> ioError (userError $ "Error extracting labels: " <> err)
    Right ls  -> return ls
  let fdfBody = case labels of
        [f] -> f
        _   -> Field { name = "", content = Children labels }
      fdf = FDF
        "1 0 obj\n"
        fdfBody
        "endobj\ntrailer\n\n<<\n/Root 1 0 R\n>>\n"
      fdfBytes = FDF.serialize fdf
  if output opts == "-"
    then ByteString.putStr fdfBytes
    else ByteString.writeFile (output opts) fdfBytes
