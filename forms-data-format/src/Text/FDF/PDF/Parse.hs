{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parsing of PDF values using 'Text.Grampa.PEG.Backtrack'.
--
-- References: PDF 32000-1:2008 (PDF 1.7 specification):
--
-- * §7.3 – Objects (booleans, numbers, strings, names, arrays, dictionaries)

module Text.FDF.PDF.Parse (
  PDFParser, runParser,
  parseDict, parseIndirectObject, parseValue,
  pdfDict, pdfUnsignedInt,
  hexDigit, dropWS, isPDFWS, skipWS
) where

import Control.Applicative ((<|>), empty, many, optional)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Char (isDigit, isHexDigit, isSpace, ord)
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid.Instances.ByteString.UTF8 (ByteStringUTF8 (ByteStringUTF8))
import Data.Scientific (Scientific, toRealFloat)
import Data.Word (Word8)
import Rank2 qualified
import Text.Grampa (InputParsing (string, anyToken, satisfy), InputCharParsing (..),
                    ParseFailure (..), FailureDescription (..))
import Text.Grampa.Combinators (concatMany, moptional, upto)
import Text.Grampa.PEG.Backtrack qualified as PEG
import Text.Parser.Char qualified as Char
import Text.Read (readMaybe)

import Text.FDF.PDF.Types

-- ---------------------------------------------------------------------------
-- Parser type

-- | Backtracking PEG parser for PDF value fragments.  The input stream is
-- 'ByteStringUTF8', which provides a safe character interface (via
-- 'takeCharsWhile' etc.) for the ASCII-structured PDF syntax.
type PDFParser = PEG.Parser (Rank2.Only PDFValue) ByteStringUTF8

-- | Run a 'PDFParser', returning the result and the remaining (unconsumed)
-- input on success, or an error message on failure.
runParser :: PDFParser a -> ByteString -> Either String (a, ByteString)
runParser p input = case PEG.applyParser p (ByteStringUTF8 input) of
  PEG.Parsed v (ByteStringUTF8 rest) -> Right (v, rest)
  PEG.NoParse (ParseFailure _ (FailureDescription descs lits) errs) ->
    Left $ case descs ++ map (BSC.unpack . unwrapBS) lits ++ errs of
      []   -> "Parse failure"
      msgs -> "Expected: " ++ intercalate ", " msgs

-- | Unwrap 'ByteStringUTF8' to the underlying 'ByteString'.
unwrapBS :: ByteStringUTF8 -> ByteString
unwrapBS (ByteStringUTF8 bs) = bs

-- ---------------------------------------------------------------------------
-- Exported parsing entry points

type ParseResult a = Either String (a, ByteString)

-- | Parse the indirect object (@N G obj ... endobj@) at the given offset.
parseIndirectObject :: ByteString -> Int64 -> Either String PDFValue
parseIndirectObject bs off = fmap fst $ (`runParser` BS.drop (fromIntegral off) bs) $
  skipWS *> takeCharsWhile isDigit           -- skip obj number
  *> skipWS *> takeCharsWhile isDigit        -- skip generation
  *> skipWS *> string "obj"
  *> skipWS *> pdfValue

-- | Parse any PDF value, skipping leading whitespace.
parseValue :: ByteString -> ParseResult PDFValue
parseValue bs = runParser pdfValue bs

-- | Parse a PDF dictionary (@\<\< ... \>\>@), skipping leading whitespace.
parseDict :: ByteString -> ParseResult (Map ByteString PDFValue)
parseDict bs = runParser pdfDict bs

-- ---------------------------------------------------------------------------
-- Character predicates

-- | Is a character PDF whitespace?
isPDFWS :: Char -> Bool
isPDFWS c = c `elem` (" \t\r\n\f\0" :: String)

-- | Is a character valid inside a PDF name token?
isNameChar :: Char -> Bool
isNameChar c = not (isSpace c) && c `notElem` ("/()<>[]{}%\0" :: String)

-- | Is a character an octal digit (@0@–@7@)?
isOctChar :: Char -> Bool
isOctChar c = c >= '0' && c <= '7'

-- ---------------------------------------------------------------------------
-- Core PDF value parser

-- | Parser for any PDF value; skips leading whitespace.
pdfValue :: PDFParser PDFValue
pdfValue = skipWS *>
  (   PDFNull       <$  string "null"
  <|> PDFBool True  <$  string "true"
  <|> PDFBool False <$  string "false"
  -- PDF names may be empty (e.g. empty-selection state serialised as @\/@).
  <|> PDFName       <$> (string "/" *> (unwrapBS <$> takeCharsWhile isNameChar))
  <|> PDFString     <$> pdfLiteralString
  -- Try dict (<<) before hex string (<).
  <|> PDFDict       <$> pdfDict
  <|> PDFString     <$> pdfHexString
  <|> PDFArray      <$> pdfArray
  <|> pdfNumOrRef
  )

-- | Skip zero or more PDF whitespace characters.
skipWS :: PDFParser ()
skipWS = void $ takeCharsWhile isPDFWS

-- ---------------------------------------------------------------------------
-- Literal string

-- | Parse a PDF literal string, consuming the opening @(@ and closing @)@.
pdfLiteralString :: PDFParser ByteString
pdfLiteralString = string "(" *> (unwrapBS <$> pdfLiteralContent) <* string ")"

-- | Parse the content of a literal string up to the matching closing @)@.
-- Handles nested parentheses and escape sequences.
pdfLiteralContent :: PDFParser ByteStringUTF8
pdfLiteralContent = concatMany litChunk
  where
    litChunk =
          litEscape
      <|> litNested
      -- Consume a run of valid UTF-8 characters that are not delimiters.
      <|> takeCharsWhile1 isRegularLitChar
      -- Fall back to consuming a single non-character factor (invalid or
      -- incomplete UTF-8 byte sequence).  PDF literal strings can contain
      -- arbitrary binary data, so we must handle bytes that do not form
      -- valid characters.  Non-character factors always have their first
      -- byte >= 0x80, so they can never be a delimiter.
      <|> satisfy isNonCharFactor
    -- A nested pair @(...)@ is kept verbatim in the string value.
    litNested = string "(" <> pdfLiteralContent <> string ")"
    -- Any character except @(@, @)@, or @\@ is a regular literal character.
    isRegularLitChar c = c /= '(' && c /= ')' && c /= '\\'
    -- A non-character factor is a ByteStringUTF8 whose first byte is >= 0x80
    -- but does not form a valid UTF-8 character.  Since takeCharsWhile1 above
    -- already consumed all valid characters, this only matches invalid byte
    -- sequences.
    isNonCharFactor (ByteStringUTF8 b) = not (BS.null b) && BS.head b >= 0x80

-- | Parse a backslash escape sequence inside a literal string.
litEscape :: PDFParser ByteStringUTF8
litEscape = string "\\" *>
  (   "\n"  <$ string "n"
  <|> "\r"  <$ string "r"
  <|> "\t"  <$ string "t"
  <|> "\b"  <$ string "b"
  <|> "\f"  <$ string "f"
  <|> "("   <$ string "("
  <|> ")"   <$ string ")"
  <|> "\\"  <$ string "\\"
  -- Line continuation: \r\n or \r or \n → empty string
  <|> ""    <$ (string "\r" *> optional (string "\n"))
  <|> ""    <$ string "\n"
  <|> ByteStringUTF8 <$> pdfOctalEscape
  -- Any other character after backslash is kept as-is (PDF §7.3.4.2).
  <|> anyToken
  )

-- | Parse 1–3 octal digits and return the corresponding byte value.
pdfOctalEscape :: PDFParser ByteString
pdfOctalEscape = evalOctal <$> octalParser
  where
    evalOctal = BS.singleton . fromIntegral . foldl' (\acc d -> 8*acc + d) 0
    octalParser = (:) <$> octDigitVal <*> upto 2 octDigitVal

-- | Parse a single octal digit, returning its numeric value.
octDigitVal :: PDFParser Int
octDigitVal = fmap (\c-> ord c - ord '0') (Char.satisfy isOctChar)

-- ---------------------------------------------------------------------------
-- Hex string

-- | Parse a PDF hex string, consuming the opening @<@ and closing @>@.
pdfHexString :: PDFParser ByteString
pdfHexString = string "<" *> pdfHexBody <* string ">"

-- | Parse the body of a hex string (without delimiters).
pdfHexBody :: PDFParser ByteString
pdfHexBody = BS.pack <$> many hexBytePair <* skipWS
  where
    hexBytePair = toHexByte
                    <$> (skipWS *> hexNibble)
                    <*> (skipWS *> (hexNibble <|> pure 0))
    toHexByte h1 h2 = fromIntegral (h1*16 + h2) :: Word8
    hexNibble = hexDigit <$> Char.satisfy isHexDigit

-- ---------------------------------------------------------------------------
-- Array

-- | Parse a PDF array, consuming the opening @[@ and closing @]@.
pdfArray :: PDFParser [PDFValue]
pdfArray = string "[" *> many pdfValue <* skipWS <* string "]"

-- ---------------------------------------------------------------------------
-- Dictionary

-- | Parse a PDF dictionary (@\<\< ... \>\>@).
pdfDict :: PDFParser (Map ByteString PDFValue)
pdfDict = string "<<" *> pdfDictBody <* string ">>"

-- | Parse the body of a dictionary (without the closing @>>@).
pdfDictBody :: PDFParser (Map ByteString PDFValue)
pdfDictBody = Map.fromList <$> many pdfEntry <* skipWS
  where
    pdfEntry = do
      _ <- skipWS *> string "/"
      -- Dictionary keys must be non-empty names.
      name  <- unwrapBS <$> takeCharsWhile1 isNameChar
      value <- pdfValue
      return (name, value)

-- ---------------------------------------------------------------------------
-- Numbers and indirect references

-- | Parse an unsigned decimal integer (one or more digits).
pdfUnsignedInt :: PDFParser Int
pdfUnsignedInt = fmap (maybe 0 fst . BSC.readInt . unwrapBS) (takeCharsWhile1 isDigit)

-- | Parse an indirect object reference (@N G R@), a real number, or an integer.
pdfNumOrRef :: PDFParser PDFValue
pdfNumOrRef = pdfRef <|> pdfNum

-- | Try to parse an indirect object reference (@N G R@).
-- Object and generation numbers must be unsigned (positive) integers.
pdfRef :: PDFParser PDFValue
pdfRef = PDFRef <$> pdfUnsignedInt <* skipWS <*> pdfUnsignedInt <* skipWS <* string "R"

-- | Parse a signed integer or real number (with optional scientific exponent).
-- Collects the raw number text and delegates to 'readMaybe' for interpretation.
pdfNum :: PDFParser PDFValue
pdfNum = do
  numStr <- BSC.unpack . unwrapBS <$>
    (  moptional (satisfyCharInput (\c -> c == '-' || c == '+'))
    <> takeCharsWhile1 isDigit
    <> moptional (string "." <> takeCharsWhile isDigit)
    <> moptional (  satisfyCharInput (\c -> c == 'e' || c == 'E')
                 <> moptional (satisfyCharInput (\c -> c == '+' || c == '-'))
                 <> takeCharsWhile1 isDigit)
    )
  if any (\c -> c == '.' || c == 'e' || c == 'E') numStr
    then case readMaybe numStr of
           Just s  -> pure (PDFReal (toRealFloat (s :: Scientific)))
           Nothing -> empty
    else case BSC.readInt (BSC.pack numStr) of
           Just (n, _) -> pure (PDFInt n)
           Nothing     -> empty

-- ---------------------------------------------------------------------------
-- utility helpers

-- | Drop PDF whitespace (space, tab, CR, LF, FF, NUL) from the front.
dropWS :: ByteString -> ByteString
dropWS = BSC.dropWhile isPDFWS

-- | Convert a hex digit character to its integer value (0–15).
hexDigit :: Char -> Int
hexDigit c
  | c >= '0' && c <= '9' = ord c - ord '0'
  | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
  | c >= 'A' && c <= 'F' = ord c - ord 'A' + 10
  | otherwise             = 0
