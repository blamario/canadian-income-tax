{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.PDFtk where

import Data.ByteString.Lazy qualified as Lazy
import System.Process.Typed (ExitCode (ExitFailure, ExitSuccess), byteStringInput, readProcess, setStdin, shell)

pdfFile2fdf :: FilePath -> IO (Either String Lazy.ByteString)
pdfFile2fdf pdfPath = do
   (exitCode, fdf, errors) <- readProcess (shell $ "pdftk " <> pdfPath <> " generate_fdf output -")
   case exitCode of
      ExitSuccess -> pure (Right fdf)
      ExitFailure n -> error ("Error converting PDF to FDF (exit code " <> show n <> ").\n" <> show errors)

pdf2fdf :: Lazy.ByteString -> IO (Either String Lazy.ByteString)
pdf2fdf pdf = do
   (exitCode, fdf, errors) <- readProcess (setStdin (byteStringInput pdf) $ shell "pdftk - generate_fdf output -")
   case exitCode of
      ExitSuccess -> pure (Right fdf)
      ExitFailure n -> error ("Error converting PDF to FDF (exit code " <> show n <> ").\n" <> show errors)

fdf2pdf :: FilePath -> Lazy.ByteString -> IO (Either String Lazy.ByteString)
fdf2pdf pdfPath fdf = do
   (exitCode, pdf, errors)
      <- readProcess (setStdin (byteStringInput fdf) $ shell $ "pdftk " <> pdfPath <> " fill_form - output -")
   case exitCode of
      ExitSuccess -> pure (Right pdf)
      ExitFailure n -> error ("Error converting FDF to PDF (exit code " <> show n <> ").\n" <> show errors)
