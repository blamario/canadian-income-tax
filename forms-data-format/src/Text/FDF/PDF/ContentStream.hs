{-# LANGUAGE OverloadedStrings #-}

-- | Minimal parser for PDF page content streams.
--
-- Only text-positioning and text-showing operators are tracked; all other
-- operators are silently skipped.  This is sufficient for extracting
-- positioned text fragments that can later be associated with nearby form
-- fields.
--
-- References: PDF 32000-1:2008 (PDF 1.7 specification):
--
-- * §8.2 – Graphics state parameters
-- * §9.3 – Text state parameters and operators
-- * §9.4 – Text objects (@BT@ / @ET@)

module Text.FDF.PDF.ContentStream (
  TextFragment (..),
  extractTextFragments,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isDigit, isSpace)
import Data.List (mapAccumL)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

-- ---------------------------------------------------------------------------
-- Types

-- | A positioned piece of text extracted from a page content stream.
data TextFragment = TextFragment
  { fragmentText :: Text
    -- ^ The decoded text content.
  , fragmentX    :: Double
    -- ^ X coordinate (from the text matrix translation component).
  , fragmentY    :: Double
    -- ^ Y coordinate (from the text matrix translation component).
  , fragmentSize :: Double
    -- ^ Approximate font size active when the text was drawn.
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Content stream operand types (internal)

data Operand
  = OpNum   Double
  | OpName  ByteString
  | OpStr   ByteString
  | OpArray [Operand]
  deriving (Show)

-- | Minimal text state threaded through the content stream.
data TextState = TextState
  { tsX    :: Double   -- ^ current text line x
  , tsY    :: Double   -- ^ current text line y
  , tsSize :: Double   -- ^ current font size (from Tf)
  , tsLead :: Double   -- ^ text leading (from TL / TD)
  }

defaultTS :: TextState
defaultTS = TextState 0 0 12 0

-- ---------------------------------------------------------------------------
-- Public API

-- | Parse a (decompressed) page content stream and return all text fragments
-- with their positions.  Unknown operators are silently skipped; parse errors
-- in the stream are not fatal — the function returns whatever fragments it
-- managed to extract.
extractTextFragments :: ByteString -> [TextFragment]
extractTextFragments = concat . snd . mapAccumL step (defaultTS, []) . tokenize

-- | Tokenize a content stream into a list of 'Token's.
tokenize :: ByteString -> [Token]
tokenize = go . dropCSWS
  where
    go bs = case nextToken bs of
      Nothing          -> []
      Just (tok, rest) -> tok : go rest

-- ---------------------------------------------------------------------------
-- Stream interpreter

-- | Process one token, threading @(TextState, [Operand])@ as accumulator.
-- The operand stack is kept in reverse order (last pushed operand at head).
step :: (TextState, [Operand]) -> Token -> ((TextState, [Operand]), [TextFragment])
step (ts, stk) (TokOperand v)    = ((ts, v : stk), [])
step (ts, stk) (TokOperator op)  = let (ts', stk', frags) = handleOp ts stk op
                                    in ((ts', stk'), frags)
step (ts, stk) TokSkip           = ((ts, stk), [])

-- | Interpret one operator, consuming the operand stack (in reverse order)
-- and returning the updated 'TextState', the remaining stack, and any
-- emitted 'TextFragment's.  Each operator pops only the operands it needs,
-- like Forth stack operations.
handleOp :: TextState -> [Operand] -> ByteString -> (TextState, [Operand], [TextFragment])
handleOp ts stk "BT" =
  -- Begin text object
  (ts, stk, [])
handleOp ts stk "ET" =
  -- End text object
  (ts, stk, [])
handleOp ts stk "Tf" =
  -- /FontName fontSize Tf
  case stk of
    OpNum s : _ : rest -> (ts { tsSize = s }, rest, [])
    _                  -> (ts, stk, [])
handleOp ts stk "TL" =
  -- leading TL
  case stk of
    OpNum l : rest -> (ts { tsLead = l }, rest, [])
    _              -> (ts, stk, [])
handleOp ts stk "Tm" =
  -- a b c d e f Tm — set text matrix
  case stk of
    OpNum f : OpNum e : _ : _ : _ : _ : rest -> (ts { tsX = e, tsY = f }, rest, [])
    _                                        -> (ts, stk, [])
handleOp ts stk "Td" =
  -- tx ty Td — relative move
  case stk of
    OpNum ty : OpNum tx : rest -> (ts { tsX = tsX ts + tx, tsY = tsY ts + ty }, rest, [])
    _                          -> (ts, stk, [])
handleOp ts stk "TD" =
  -- tx ty TD — like Td but also sets TL = -ty
  case stk of
    OpNum ty : OpNum tx : rest -> (ts { tsX = tsX ts + tx, tsY = tsY ts + ty, tsLead = negate ty }, rest, [])
    _                          -> (ts, stk, [])
handleOp ts stk "T*" =
  -- Move to start of next line (0 -TL Td)
  (ts { tsY = tsY ts - tsLead ts }, stk, [])
handleOp ts stk "Tj" =
  -- (string) Tj — show text
  case stk of
    OpStr s : rest -> (ts, rest, [mkFrag ts s])
    _              -> (ts, stk, [])
handleOp ts stk "TJ" =
  -- [ (str) kern ... ] TJ — show text with kerning
  case stk of
    OpArray elems : rest ->
      let strs = [s | OpStr s <- elems]
      in case strs of
           [] -> (ts, rest, [])
           _  -> (ts, rest, [mkFrag ts (BS.concat strs)])
    _ -> (ts, stk, [])
handleOp ts stk "'" =
  -- (string) ' — T* then Tj
  let ts' = ts { tsY = tsY ts - tsLead ts }
  in case stk of
       OpStr s : rest -> (ts', rest, [mkFrag ts' s])
       _              -> (ts', stk, [])
handleOp ts stk "\"" =
  -- aw ac (string) " — set word/char spacing, T*, Tj
  let ts' = ts { tsY = tsY ts - tsLead ts }
  in case stk of
       OpStr s : _ : _ : rest -> (ts', rest, [mkFrag ts' s])
       _                      -> (ts', stk, [])
handleOp ts _stk _ =
  -- Unknown operator: discard operand stack
  (ts, [], [])

mkFrag :: TextState -> ByteString -> TextFragment
mkFrag ts raw = TextFragment
  { fragmentText = decodePDFTextBytes raw
  , fragmentX    = tsX ts
  , fragmentY    = tsY ts
  , fragmentSize = tsSize ts
  }

-- | Decode raw content-stream text bytes to 'Text'.
-- Content-stream text operands are font-encoded bytes, not PDF string objects,
-- so the UTF-16BE BOM convention does not apply.  We always decode as Latin-1
-- (the closest single-byte approximation when no font encoding info is
-- available).
decodePDFTextBytes :: ByteString -> Text
decodePDFTextBytes = Text.decodeLatin1

-- ---------------------------------------------------------------------------
-- Tokeniser

data Token
  = TokOperand Operand
  | TokOperator ByteString
  | TokSkip        -- ^ consumed whitespace / comment / unknown

nextToken :: ByteString -> Maybe (Token, ByteString)
nextToken bs =
      let bs' = dropCSWS bs
      in if BS.null bs' then Nothing
         else Just (tokenAt bs')

tokenAt :: ByteString -> (Token, ByteString)
tokenAt bs = case BSC.head bs of
  '(' -> case parseLiteralString (BS.tail bs) 0 of
           Just (s, rest) -> (TokOperand (OpStr s), rest)
           Nothing        -> (TokSkip, BS.tail bs)
  '<' | BS.length bs > 1 && BSC.index bs 1 == '<' ->
        -- Skip inline dict << ... >> (e.g. inline images)
        (TokSkip, skipPastDoubleAngle (BS.drop 2 bs))
      | otherwise -> case parseHexString (BS.tail bs) of
           Just (s, rest) -> (TokOperand (OpStr s), rest)
           Nothing        -> (TokSkip, BS.tail bs)
  '/' -> let (name, rest) = BSC.span isNameChar (BS.tail bs)
         in (TokOperand (OpName name), rest)
  '[' -> let (elems, rest) = parseArray (BS.tail bs)
         in (TokOperand (OpArray elems), rest)
  '%' -> -- Comment: skip to end of line
         let rest = BSC.dropWhile (\c -> c /= '\n' && c /= '\r') bs
         in (TokSkip, dropCSWS rest)
  c | isDigit c || c == '-' || c == '+' || c == '.' ->
        case parseNumber bs of
          Just (n, rest) -> (TokOperand (OpNum n), rest)
          Nothing        -> (TokSkip, BS.tail bs)
    | isAlpha c ->
        let (op, rest) = BSC.span isAlpha bs
        in (TokOperator op, rest)
    | otherwise -> (TokSkip, BS.tail bs)
  where
    isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '*' || c == '\''  || c == '"'
    isNameChar c = not (isSpace c) && c `notElem` ("/()<>[]{}%" :: String)

-- ---------------------------------------------------------------------------
-- Operand parsers

-- | Parse a literal string @(...)@ handling nested parentheses and escapes.
parseLiteralString :: ByteString -> Int -> Maybe (ByteString, ByteString)
parseLiteralString bs depth = go bs depth []
  where
    go inp d acc
      | BS.null inp = Nothing
      | otherwise = case BSC.head inp of
          ')' | d == 0 -> Just (BS.pack (reverse acc), BS.tail inp)
              | otherwise -> go (BS.tail inp) (d - 1) (fromIntegral (fromEnum ')') : acc)
          '(' -> go (BS.tail inp) (d + 1) (fromIntegral (fromEnum '(') : acc)
          '\\' | BS.length inp > 1 ->
                  let c2 = BSC.index inp 1
                      rest2 = BS.drop 2 inp
                  in case c2 of
                       'n'  -> go rest2 d (10 : acc)
                       'r'  -> go rest2 d (13 : acc)
                       't'  -> go rest2 d (9  : acc)
                       'b'  -> go rest2 d (8  : acc)
                       'f'  -> go rest2 d (12 : acc)
                       '\\' -> go rest2 d (fromIntegral (fromEnum '\\') : acc)
                       '('  -> go rest2 d (fromIntegral (fromEnum '(')  : acc)
                       ')'  -> go rest2 d (fromIntegral (fromEnum ')')  : acc)
                       _ | c2 >= '0' && c2 <= '7' ->
                             let (octs, _) = BSC.span (\c -> c >= '0' && c <= '7') (BS.tail inp)
                                 octStr = BS.take 3 octs
                                 val = BSC.foldl' (\a c -> a * 8 + fromIntegral (fromEnum c - fromEnum '0')) 0 octStr
                                 remaining = BS.drop (BS.length octStr) inp
                             in go remaining d (fromIntegral (val :: Int) : acc)
                         | otherwise -> go rest2 d (fromIntegral (fromEnum c2) : acc)
               | otherwise -> go (BS.tail inp) d acc
          c -> go (BS.tail inp) d (fromIntegral (fromEnum c) : acc)

-- | Parse a hex string @\<...\>@.
parseHexString :: ByteString -> Maybe (ByteString, ByteString)
parseHexString = go []
  where
    go acc bs
      | BS.null bs = Nothing
      | otherwise = case BSC.head bs of
          '>' -> Just (BS.pack (reverse acc), BS.tail bs)
          c | isHexDigit c ->
                let hi = hexVal c
                    rest1 = BS.tail bs
                    rest1' = BSC.dropWhile isSpace rest1
                in if BS.null rest1'
                   then Just (BS.pack (reverse (fromIntegral (hi * 16) : acc)), rest1')
                   else let c2 = BSC.head rest1'
                        in if c2 == '>'
                           then Just (BS.pack (reverse (fromIntegral (hi * 16) : acc)), BS.tail rest1')
                           else if isHexDigit c2
                                then go (fromIntegral (hi * 16 + hexVal c2) : acc) (BS.tail rest1')
                                else go acc (BS.tail rest1')
            | isSpace c -> go acc (BS.tail bs)
            | otherwise -> Nothing
    isHexDigit c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
    hexVal c
      | c >= '0' && c <= '9' = fromIntegral (fromEnum c - fromEnum '0')
      | c >= 'a' && c <= 'f' = fromIntegral (fromEnum c - fromEnum 'a' + 10)
      | c >= 'A' && c <= 'F' = fromIntegral (fromEnum c - fromEnum 'A' + 10)
      | otherwise             = 0 :: Int

-- | Parse a number (integer or real).
parseNumber :: ByteString -> Maybe (Double, ByteString)
parseNumber bs =
  let (numStr, rest) = BSC.span (\c -> isDigit c || c == '.' || c == '-' || c == '+' || c == 'e' || c == 'E') bs
  in if BS.null numStr
     then Nothing
     else case BSC.readInt numStr of
       Just (n, r) | BS.null r || not (BSC.head r == '.' || BSC.head r == 'e' || BSC.head r == 'E')
                   -> Just (fromIntegral n, rest)
       _           -> case reads (BSC.unpack numStr) of
                        [(d, _)]  -> Just (d, rest)
                        _         -> Nothing

-- | Parse an array @[...]@ returning operand elements.
parseArray :: ByteString -> ([Operand], ByteString)
parseArray = go []
  where
    go acc bs
      | BS.null bs = (reverse acc, bs)
      | otherwise  =
          let bs' = dropCSWS bs
          in if BS.null bs' then (reverse acc, bs')
             else case BSC.head bs' of
               ']' -> (reverse acc, BS.tail bs')
               _   -> case nextToken bs' of
                        Just (TokOperand v, rest) -> go (v : acc) rest
                        Just (_, rest)            -> go acc rest
                        Nothing                   -> (reverse acc, bs')

-- | Skip past a matching @>>@.
skipPastDoubleAngle :: ByteString -> ByteString
skipPastDoubleAngle bs = case BS.breakSubstring ">>" bs of
  (_, rest) | BS.length rest >= 2 -> BS.drop 2 rest
            | otherwise           -> BS.empty

-- | Drop PDF whitespace and comments.
dropCSWS :: ByteString -> ByteString
dropCSWS bs =
  let bs' = BSC.dropWhile isSpace bs
  in if not (BS.null bs') && BSC.head bs' == '%'
     then dropCSWS (BSC.dropWhile (\c -> c /= '\n' && c /= '\r') bs')
     else bs'
