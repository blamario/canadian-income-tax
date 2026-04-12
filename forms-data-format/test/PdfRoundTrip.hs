{-# LANGUAGE OverloadedStrings #-}
-- | Integration tests for the PDF round-trip: parsePDF ∘ fillPDF ≈ id.
module Main (main) where

import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.IORef (IORef, newIORef, modifyIORef, readIORef)
import Data.List (foldl')
import System.Exit (exitFailure, exitSuccess)

import Text.FDF (FDF (..), Field (..), FieldContent (..))
import qualified Text.FDF as FDF
import Text.FDF.PDF (PDF (..), parsePDF, fillPDF, serializePDF, fieldLabels)

-- ---------------------------------------------------------------------------
-- Minimal test PDF construction

-- | Build a minimal valid PDF-1.4 byte string from a list of object bodies.
-- Objects are numbered starting from 1 with generation 0.
makePDF :: [BS.ByteString] -> BS.ByteString
makePDF contents =
  let header  = "%PDF-1.4\n"
      objects = zip [1..] contents
      (body, offsets) = foldl' addObj (header, []) objects
      xrefOff = BS.length body
      nObjs   = length objects
      xref    = BSC.pack $
                  "xref\n0 " <> show (nObjs + 1) <> "\n"
                  <> "0000000000 65535 f\r\n"
                  <> concatMap (\off -> padDec10 off <> " 00000 n\r\n") (reverse offsets)
      trailer = BSC.pack $
                  "trailer\n<< /Size " <> show (nObjs + 1)
                  <> " /Root 1 0 R >>\nstartxref\n"
                  <> show xrefOff <> "\n%%EOF\n"
  in body <> xref <> trailer
  where
    addObj (acc, offs) (n, content) =
      let objBS = BSC.pack (show (n :: Int) <> " 0 obj\n")
                  <> content <> "\nendobj\n"
      in (acc <> objBS, BS.length acc : offs)
    padDec10 n =
      let s = show n in replicate (10 - length s) '0' <> s

-- | A minimal PDF with a single text field @TextField1@ with value @Hello@.
simplePDF :: BS.ByteString
simplePDF = makePDF
  [ "<< /Type /Catalog /Pages 2 0 R /AcroForm 5 0 R >>"
  , "<< /Type /Pages /Kids [ 3 0 R ] /Count 1 >>"
  , "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Annots [ 6 0 R ] >>"
  , "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>"
  , "<< /Fields [ 6 0 R ] /DR << /Font << /Helv 4 0 R >> >> >>"
  , "<< /Type /Annot /Subtype /Widget /FT /Tx /T (TextField1) /V (Hello) /Rect [100 700 400 720] /P 3 0 R /DA (/Helv 12 Tf 0 g) >>"
  ]

-- | Like 'simplePDF' but the field @\/Rect@ uses fractional coordinates
-- whose @show@ representation would involve scientific notation
-- (e.g. @0.05@ → @\"5.0e-2\"@).  This exercises the fix for the
-- \"Unexpected character: e\" bug in @serializeValue@ / @parseNumOrRef@.
floatRectPDF :: BS.ByteString
floatRectPDF = makePDF
  [ "<< /Type /Catalog /Pages 2 0 R /AcroForm 5 0 R >>"
  , "<< /Type /Pages /Kids [ 3 0 R ] /Count 1 >>"
  , "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Annots [ 6 0 R ] >>"
  , "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>"
  , "<< /Fields [ 6 0 R ] /DR << /Font << /Helv 4 0 R >> >> >>"
  -- /Rect uses 0.05 – a value whose show representation is "5.0e-2"
  , "<< /Type /Annot /Subtype /Widget /FT /Tx /T (FloatField) /V (Hi) /Rect [0.05 0.05 200.05 12.05] /P 3 0 R /DA (/Helv 12 Tf 0 g) >>"
  ]

-- | A minimal PDF with a radio button group (initially unselected: @\/V \/@)
-- and a text field.  The radio group's child widget annotations have no @\/T@
-- key, which is the pattern that caused the original bug.
radioPDF :: BS.ByteString
radioPDF = makePDF
  [ "<< /Type /Catalog /Pages 2 0 R /AcroForm 5 0 R >>"
  , "<< /Type /Pages /Kids [ 3 0 R ] /Count 1 >>"
  , "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Annots [ 7 0 R 8 0 R 9 0 R ] >>"
  , "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>"
  , "<< /Fields [ 6 0 R 9 0 R ] /DR << /Font << /Helv 4 0 R >> >> >>"
  -- Radio group: /V / means empty-name selection (unselected)
  , "<< /FT /Btn /Ff 49152 /T (RadioGroup) /V / /Kids [ 7 0 R 8 0 R ] >>"
  -- Widget annotations without /T (anonymous)
  , "<< /Type /Annot /Subtype /Widget /Parent 6 0 R /Rect [100 700 120 720] /P 3 0 R /AS /Off >>"
  , "<< /Type /Annot /Subtype /Widget /Parent 6 0 R /Rect [150 700 170 720] /P 3 0 R /AS /Off >>"
  -- Text field
  , "<< /Type /Annot /Subtype /Widget /FT /Tx /T (TextField1) /V (InitVal) /Rect [100 650 400 670] /P 3 0 R /DA (/Helv 12 Tf 0 g) >>"
  ]

-- | A minimal PDF with an AcroForm that has an empty @\/Fields@ array
-- (i.e. no form fields at all).
noFieldsPDF :: BS.ByteString
noFieldsPDF = makePDF
  [ "<< /Type /Catalog /Pages 2 0 R /AcroForm 3 0 R >>"
  , "<< /Type /Pages /Kids [] /Count 0 >>"
  , "<< /Fields [] >>"
  ]

-- | A PDF with page content stream text that labels form fields.
-- "First Name:" is drawn near the @FirstName@ field,
-- "Email:" is drawn near the @Email@ field.
labelledPDF :: BS.ByteString
labelledPDF =
  let streamData = "BT\n/Helv 12 Tf\n1 0 0 1 100 750 Tm\n(First Name:) Tj\n1 0 0 1 100 650 Tm\n(Email:) Tj\nET"
      streamObj  = "<< /Length " <> BSC.pack (show (BS.length streamData))
                    <> " >>\nstream\n" <> streamData <> "\nendstream"
  in makePDF
    [ "<< /Type /Catalog /Pages 2 0 R /AcroForm 6 0 R >>"
    , "<< /Type /Pages /Kids [ 3 0 R ] /Count 1 >>"
    , "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Contents 4 0 R /Annots [ 7 0 R 8 0 R ] /Resources << /Font << /Helv 5 0 R >> >> >>"
    , streamObj
    , "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>"
    , "<< /Fields [ 7 0 R 8 0 R ] /DR << /Font << /Helv 5 0 R >> >> >>"
    , "<< /Type /Annot /Subtype /Widget /FT /Tx /T (FirstName) /V () /Rect [100 730 400 745] /P 3 0 R /DA (/Helv 12 Tf 0 g) >>"
    , "<< /Type /Annot /Subtype /Widget /FT /Tx /T (Email) /V () /Rect [100 630 400 645] /P 3 0 R /DA (/Helv 12 Tf 0 g) >>"
    ]

-- ---------------------------------------------------------------------------
-- Test runner

type FailRef = IORef [String]

assertM :: FailRef -> String -> Bool -> IO ()
assertM ref msg ok = unless ok $ modifyIORef ref (msg :)

runTest :: FailRef -> String -> IO () -> IO ()
runTest failRef testName action = do
  prevFails <- readIORef failRef
  action
  newFails <- readIORef failRef
  if length newFails == length prevFails
    then putStrLn $ "  PASS  " <> testName
    else do
      let newMsgs = take (length newFails - length prevFails) newFails
      putStrLn $ "  FAIL  " <> testName
      mapM_ (\m -> putStrLn $ "        " <> m) (reverse newMsgs)

-- | Build a minimal FDF value to use as fill input.
makeFillFDF :: Field -> FDF
makeFillFDF f = FDF "1 0 obj\n" f "endobj\ntrailer\n\n<<\n/Root 1 0 R\n>>\n"

-- | Extract the 'FieldContent' from a 'PDF''s form body.
formContent :: PDF -> FieldContent
formContent = content . body . form

-- ---------------------------------------------------------------------------
-- Tests

-- | Parsing simplePDF should produce a single text field with value "Hello".
testParseSimple :: FailRef -> IO ()
testParseSimple ref =
  case parsePDF simplePDF of
    Left err  -> modifyIORef ref (("parsePDF simplePDF: " <> err) :)
    Right pdf -> do
      let fdf = form pdf
      assertM ref "simplePDF: field name should be TextField1" $
        name (body fdf) == "TextField1"
      assertM ref "simplePDF: field value should be Hello" $
        content (body fdf) == FieldValue "Hello"

-- | Parsing radioPDF should produce a radio group field (with /V /)
-- and a text field.
testParseRadio :: FailRef -> IO ()
testParseRadio ref =
  case parsePDF radioPDF of
    Left err  -> modifyIORef ref (("parsePDF radioPDF: " <> err) :)
    Right pdf ->
      case formContent pdf of
        Children kids -> do
          assertM ref ("Expected 2 top-level children, got " <> show (length kids))
                      (length kids == 2)
          case kids of
            [radioField, textField] -> do
              assertM ref "radio group name" (name radioField == "RadioGroup")
              assertM ref "radio group /V /" (content radioField == FieldNameValue "")
              assertM ref "text field name" (name textField == "TextField1")
              assertM ref "text field /V (InitVal)" (content textField == FieldValue "InitVal")
            _ -> return ()
        other ->
          modifyIORef ref (("Expected Children, got: " <> show other) :)

-- | fillPDF should update a text field value and parsePDF on the result
-- should return the new value.
testFillSimple :: FailRef -> IO ()
testFillSimple ref =
  let fdf = makeFillFDF Field { name = "TextField1", content = FieldValue "World" }
  in case parsePDF simplePDF of
       Left err     -> modifyIORef ref (("parsePDF simplePDF: " <> err) :)
       Right pdf    ->
         case fillPDF fdf pdf of
           Left err     -> modifyIORef ref (("fillPDF simplePDF: " <> err) :)
           Right filled ->
              assertM ref "filled value should be 'World'" $
                formContent filled == FieldValue "World"

-- | fillPDF on a radio-button PDF should allow updating the text field
-- and the result should round-trip correctly.
testFillRadio :: FailRef -> IO ()
testFillRadio ref =
  let fdf = makeFillFDF Field { name = "TextField1", content = FieldValue "Filled" }
  in case parsePDF radioPDF of
       Left err     -> modifyIORef ref (("parsePDF radioPDF: " <> err) :)
       Right pdf    ->
         case fillPDF fdf pdf of
           Left err     -> modifyIORef ref (("fillPDF radioPDF: " <> err) :)
           Right filled ->
              case formContent filled of
                Children kids ->
                  let textFields = filter (\k -> name k == "TextField1") kids
                  in assertM ref "TextField1 should be 'Filled'" $
                       all (\k -> content k == FieldValue "Filled") textFields
                FieldValue v ->
                  assertM ref "single field should be 'Filled'" (v == "Filled")
                other ->
                  modifyIORef ref (("Unexpected body: " <> show other) :)

-- | The empty-name radio button value @\/V \/@ must survive a fill-and-parse
-- round-trip (regression for the \"Empty name\" / \"Unexpected character: e\" bug).
testEmptyNameRoundTrip :: FailRef -> IO ()
testEmptyNameRoundTrip ref =
  let fdf = makeFillFDF Field { name = "TextField1", content = FieldValue "X" }
  in case parsePDF radioPDF of
       Left err     -> modifyIORef ref (("parsePDF radioPDF: " <> err) :)
       Right pdf    ->
         case fillPDF fdf pdf of
           Left err     -> modifyIORef ref (("fillPDF radioPDF: " <> err) :)
           Right filled ->
              case formContent filled of
                Children kids ->
                  let radioKids = filter (\k -> name k == "RadioGroup") kids
                  in assertM ref "radio /V / should survive round-trip" $
                       all (\k -> content k == FieldNameValue "") radioKids
                other ->
                  modifyIORef ref (("Unexpected body: " <> show other) :)

-- | A fill-and-parse round-trip on a PDF whose field annotation has
-- fractional /Rect coordinates (e.g. 0.05) whose @show@ representation
-- includes an @e@ exponent.  Before the fix this caused
-- \"Unexpected character: e\" when pdf-to-fdf read the filled PDF.
testFloatRectRoundTrip :: FailRef -> IO ()
testFloatRectRoundTrip ref =
  let fdf = makeFillFDF Field { name = "FloatField", content = FieldValue "OK" }
  in case parsePDF floatRectPDF of
       Left err     -> modifyIORef ref (("parsePDF floatRectPDF: " <> err) :)
       Right pdf    ->
         case fillPDF fdf pdf of
           Left err     -> modifyIORef ref (("fillPDF floatRectPDF: " <> err) :)
           Right filled ->
              assertM ref "FloatField value should be 'OK'" $
                formContent filled == FieldValue "OK"

-- | A minimal PDF whose text field value contains raw non-ASCII bytes that
-- are NOT valid UTF-8.  Before the fix, the @ByteStringUTF8@-based parser's
-- 'takeCharsWhile1' would stop at these bytes because they have no character
-- representation, causing the parse to fail.
binaryStringPDF :: BS.ByteString
binaryStringPDF = makePDF
  [ "<< /Type /Catalog /Pages 2 0 R /AcroForm 5 0 R >>"
  , "<< /Type /Pages /Kids [ 3 0 R ] /Count 1 >>"
  , "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Annots [ 6 0 R ] >>"
  , "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>"
  , "<< /Fields [ 6 0 R ] /DR << /Font << /Helv 4 0 R >> >> >>"
  -- /V contains raw bytes 0x80, 0xFE which are invalid UTF-8
  , "<< /Type /Annot /Subtype /Widget /FT /Tx /T (BinField) /V (A\x80\xfeZ) /Rect [100 700 400 720] /P 3 0 R /DA (/Helv 12 Tf 0 g) >>"
  ]

-- | Parsing a PDF whose literal string field contains raw non-ASCII bytes
-- (invalid UTF-8) should succeed.  This is a regression test for the
-- 'takeCharsWhile1' → 'takeWhile1' fix in 'pdfLiteralContent'.
testBinaryStringParse :: FailRef -> IO ()
testBinaryStringParse ref =
  case parsePDF binaryStringPDF of
    Left err  -> modifyIORef ref (("parsePDF binaryStringPDF: " <> err) :)
    Right pdf -> do
      assertM ref "binaryStringPDF: field name should be BinField" $
        name (body $ form pdf) == "BinField"
      assertM ref "binaryStringPDF: field value should contain raw bytes" $
        content (body $ form pdf) == FieldValue "A\x80\xfeZ"

-- | fillPDF should be idempotent: applying the same FDF twice produces the
-- same byte output as applying it once.
testFillIdempotent :: FailRef -> IO ()
testFillIdempotent ref =
  let fdf = makeFillFDF Field { name = "TextField1", content = FieldValue "World" }
  in case parsePDF simplePDF of
       Left err     -> modifyIORef ref (("parsePDF (first): " <> err) :)
       Right pdf    ->
         case fillPDF fdf pdf of
           Left err     -> modifyIORef ref (("fillPDF (first): " <> err) :)
           Right filled1 ->
             case fillPDF fdf filled1 of
               Left err      -> modifyIORef ref (("fillPDF (second): " <> err) :)
               Right filled2 ->
                 assertM ref "fillPDF should be idempotent (same bytes on second call)" $
                   serializePDF filled1 == serializePDF filled2

-- | Parsing a PDF with no form fields should succeed with an empty form.
testParseNoFields :: FailRef -> IO ()
testParseNoFields ref =
  case parsePDF noFieldsPDF of
    Left err  -> modifyIORef ref (("parsePDF noFieldsPDF: " <> err) :)
    Right pdf ->
      assertM ref "noFieldsPDF: form body should be Children []" $
        formContent pdf == Children []

-- | Filling a form-less PDF with an empty FDF should succeed and return
-- the same empty form.
testFillNoFields :: FailRef -> IO ()
testFillNoFields ref =
  let emptyFDF = makeFillFDF Field { name = "", content = Children [] }
  in case parsePDF noFieldsPDF of
       Left err  -> modifyIORef ref (("parsePDF noFieldsPDF: " <> err) :)
       Right pdf ->
         case fillPDF emptyFDF pdf of
           Left err    -> modifyIORef ref (("fillPDF noFieldsPDF: " <> err) :)
           Right filled ->
             assertM ref "filled noFieldsPDF: form body should be Children []" $
               formContent filled == Children []

-- | fieldLabels should return a list of Fields mirroring the form hierarchy
-- where leaf values contain nearby page text.
testFieldLabels :: FailRef -> IO ()
testFieldLabels ref =
  case parsePDF labelledPDF of
    Left err  -> modifyIORef ref (("parsePDF labelledPDF: " <> err) :)
    Right pdf ->
      case fieldLabels pdf of
        Left err -> modifyIORef ref (("fieldLabels: " <> err) :)
        Right fields -> do
          let findField n = [f | f <- fields, name f == n]
          case findField "FirstName" of
            [f] -> assertM ref "fieldLabels: FirstName label should be 'First Name:'" $
                     content f == FieldValue "First Name:"
            _   -> modifyIORef ref ("fieldLabels: FirstName field not found" :)
          case findField "Email" of
            [f] -> assertM ref "fieldLabels: Email label should be 'Email:'" $
                     content f == FieldValue "Email:"
            _   -> modifyIORef ref ("fieldLabels: Email field not found" :)

-- | fieldLabels on a PDF with no form fields should return an empty list.
testFieldLabelsNoFields :: FailRef -> IO ()
testFieldLabelsNoFields ref =
  case parsePDF noFieldsPDF of
    Left err  -> modifyIORef ref (("parsePDF noFieldsPDF: " <> err) :)
    Right pdf ->
      case fieldLabels pdf of
        Left err -> modifyIORef ref (("fieldLabels noFields: " <> err) :)
        Right fields ->
          assertM ref "fieldLabels: empty list for no-field PDF" $
            null fields

-- | A minimal PDF with a single text field and an @\/XFA@ entry in the
-- AcroForm dictionary.  This tests that @fillPDF@ strips the @\/XFA@ key
-- so that viewers fall back to AcroForm data instead of stale XFA data.
xfaPDF :: BS.ByteString
xfaPDF = makePDF
  [ "<< /Type /Catalog /Pages 2 0 R /AcroForm 5 0 R >>"
  , "<< /Type /Pages /Kids [ 3 0 R ] /Count 1 >>"
  , "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Annots [ 6 0 R ] >>"
  , "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>"
  , "<< /Fields [ 6 0 R ] /DR << /Font << /Helv 4 0 R >> >> /XFA (dummy) >>"
  , "<< /Type /Annot /Subtype /Widget /FT /Tx /T (TextField1) /V (Hello) /Rect [100 700 400 720] /P 3 0 R /DA (/Helv 12 Tf 0 g) >>"
  ]

-- | fillPDF must strip the @\/XFA@ entry from the AcroForm dictionary.
-- When both AcroForm and XFA data are present, viewers may ignore the
-- AcroForm field values, displaying stale empty XFA data for some fields.
testFillStripsXFA :: FailRef -> IO ()
testFillStripsXFA ref =
  let fdf = makeFillFDF Field { name = "TextField1", content = FieldValue "World" }
  in case parsePDF xfaPDF of
       Left err     -> modifyIORef ref (("parsePDF xfaPDF: " <> err) :)
       Right pdf    ->
         case fillPDF fdf pdf of
           Left err     -> modifyIORef ref (("fillPDF xfaPDF: " <> err) :)
           Right filled -> do
             -- The field value should be correctly updated
             assertM ref "xfaPDF: filled value should be 'World'" $
               formContent filled == FieldValue "World"
             -- The incremental update (appended after the original bytes)
             -- should contain a new AcroForm object without /XFA.
             let outBytes = serializePDF filled
                 updateBytes = BS.drop (BS.length xfaPDF) outBytes
             assertM ref "xfaPDF: incremental update should not contain /XFA" $
               not (BS.isInfixOf "/XFA" updateBytes)

-- | FDF round-trip for non-ASCII values containing @)@.  Before the
-- 'escapeRawBytes' fix, @serializeValue@ would produce UTF-16BE bytes
-- containing a bare @0x29@ byte (the `)` encoding), which the parser would
-- mistake for the string terminator, resulting in "Invalid UTF-16BE stream".
testFdfUtf16RoundTrip :: FailRef -> IO ()
testFdfUtf16RoundTrip ref =
  let val = "caf\233 (aide)"  -- non-ASCII é plus parentheses
      fdf = makeFillFDF Field { name = "T", content = FieldValue val }
      fdfBytes = FDF.serialize fdf
  in case FDF.parse fdfBytes of
       Left err ->
         modifyIORef ref (("FDF utf16 round-trip parse failed: " <> err) :)
       Right fdf' ->
         assertM ref "FDF utf16 round-trip: value should survive" $
           content (body fdf') == FieldValue val

-- ---------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  failRef <- newIORef []
  let run name t = runTest failRef name (t failRef)
  run "parse simple text field PDF"          testParseSimple
  run "parse radio button PDF"               testParseRadio
  run "parse PDF with no form fields"        testParseNoFields
  run "fill form-less PDF with empty FDF"    testFillNoFields
  run "fill simple text field"               testFillSimple
  run "fill radio PDF (text field only)"     testFillRadio
  run "empty-name /V / survives round-trip"  testEmptyNameRoundTrip
  run "float /Rect coords survive round-trip" testFloatRectRoundTrip
  run "binary bytes in literal string"        testBinaryStringParse
  run "fillPDF is idempotent"                testFillIdempotent
  run "fillPDF strips /XFA from AcroForm"   testFillStripsXFA
  run "fieldLabels maps text to fields"      testFieldLabels
  run "fieldLabels on no-field PDF"          testFieldLabelsNoFields
  run "FDF UTF-16BE round-trip with parens"  testFdfUtf16RoundTrip
  failures <- readIORef failRef
  if null failures
    then do
      putStrLn "\nAll tests passed."
      exitSuccess
    else do
      putStrLn $ "\n" <> show (length failures) <> " assertion(s) failed."
      exitFailure
