{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | IO functions for invoking PDFTk as an external process

module Tax.PDFtk where

import Data.ByteString.Lazy qualified as Lazy
import System.Process.Typed (ExitCode (ExitFailure, ExitSuccess), byteStringInput, readProcess, setStdin, shell)

-- | Convert a PDF file to an FDF bytestring via @pdftk generate_fdf@
pdfFile2fdf :: FilePath -> IO (Either String Lazy.ByteString)
pdfFile2fdf pdfPath = do
   (exitCode, fdf, errors) <- readProcess (shell $ "pdftk " <> pdfPath <> " generate_fdf output -")
   case exitCode of
      ExitSuccess -> pure (Right fdf)
      ExitFailure n -> pure (Left $ "Error converting PDF to FDF (exit code " <> show n <> ").\n" <> show errors)

-- | Convert a PDF bytestring to FDF via @pdftk generate_fdf@
pdf2fdf :: Lazy.ByteString -> IO (Either String Lazy.ByteString)
pdf2fdf pdf = do
   (exitCode, fdf, errors) <- readProcess (setStdin (byteStringInput pdf) $ shell "pdftk - generate_fdf output -")
   case exitCode of
      ExitSuccess -> pure (Right fdf)
      ExitFailure n -> pure (Left $ "Error converting PDF to FDF (exit code " <> show n <> ").\n" <> show errors)

-- | Given a PDF file, convert an FDF bytestring to filled PDF via @pdftk fill_form@
fdf2pdf :: FilePath -> Lazy.ByteString -> IO (Either String Lazy.ByteString)
fdf2pdf pdfPath fdf = do
   (exitCode, pdf, errors)
      <- readProcess (setStdin (byteStringInput fdf) $ shell $ "pdftk " <> pdfPath <> " fill_form - output -")
   case exitCode of
      ExitSuccess -> pure (Right pdf)
      ExitFailure n -> pure (Left $ "Error converting FDF to PDF (exit code " <> show n <> ").\n" <> show errors)
