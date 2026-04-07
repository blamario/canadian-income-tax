{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parsing of PDF XRefs
--
-- References: PDF 32000-1:2008 (PDF 1.7 specification):
--
-- * §7.5 – File structure (cross-reference tables and streams, incremental updates)

module Text.FDF.PDF.XRef (parseXRefChain) where

import Control.Monad (void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isDigit)
import Data.Int (Int64)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Monoid.Instances.ByteString.UTF8 (ByteStringUTF8 (ByteStringUTF8))
import Data.Traversable (for)
import Text.Grampa (InputParsing (getInput, string), InputCharParsing (..), (<?>), (<<|>))
import Text.Grampa.Combinators (concatMany)

import Text.FDF.PDF.Decompress (decompressStream)
import Text.FDF.PDF.Parse (PDFParser, runParser, dropWS, parseDict, pdfDict, pdfUnsignedInt, skipWS)
import Text.FDF.PDF.Types

-- | Parse the full chain of cross-reference tables, following @/Prev@ and
-- @/XRefStm@ links.
--
-- Hybrid-reference PDFs (created by Adobe Acrobat for backward compatibility)
-- have a traditional @xref@ table *and* a @/XRefStm@ entry in the trailer
-- that points to a cross-reference stream containing additional objects.
-- Both sources are merged, with the main section taking precedence.
parseXRefChain :: ByteString -> Int64 -> Either String (XRef, Map ByteString PDFValue)
parseXRefChain bs off = do
  (xref, trailer) <- parseOneXRef bs off
  -- Merge any /XRefStm cross-reference stream (hybrid-reference PDFs).
  xref' <- case Map.lookup "XRefStm" trailer of
              Just (PDFInt stmOff) ->
                case parseXRefStream bs (fromIntegral stmOff) of
                  Left err        -> Left ("/XRefStm parse error: " <> err)
                  Right (stmXref, _) ->
                    -- Main xref takes precedence over the hybrid stream.
                    Right (IntMap.union xref stmXref)
              _ -> return xref
  -- Follow the /Prev chain to older xref sections.
  case Map.lookup "Prev" trailer of
    Just (PDFInt prev) -> do
      (prevXRef, _) <- parseXRefChain bs (fromIntegral prev)
      -- Newer (current) entries take precedence over older ones.
      return (IntMap.union xref' prevXRef, trailer)
    _ -> return (xref', trailer)

-- | Parse a single cross-reference section (either traditional table or
-- cross-reference stream) and return the XRef map plus trailer dictionary.
parseOneXRef :: ByteString -> Int64 -> Either String (XRef, Map ByteString PDFValue)
parseOneXRef bs off = do
  let chunk = BS.drop (fromIntegral off) bs
  if "xref" `BS.isPrefixOf` chunk
    then fst <$> runParser traditionalXRef chunk
    else parseXRefStream bs off

-- ---------------------------------------------------------------------------
-- Traditional (table-based) cross-reference section

-- | Parse a traditional (table-based) cross-reference section and its trailer.
traditionalXRef :: PDFParser (XRef, Map ByteString PDFValue)
traditionalXRef = (,) <$> (string "xref" *> skipWS *> pdfSubsections) <*> (string "trailer" *> skipWS *> pdfDict)

-- | Zero or more xref subsections, stopping at "trailer".
pdfSubsections :: PDFParser XRef
pdfSubsections = concatMany (skipWS *> pdfSubsection)

--- | One xref subsection: @firstObj count\n@ followed by entries.
pdfSubsection :: PDFParser XRef
pdfSubsection = do
  firstObj <- pdfUnsignedInt <?> "object number in xref subsection"
  skipWS
  objectCount <- pdfUnsignedInt <?> "count in xref subsection"
  skipLineEnd
  IntMap.fromList . mapMaybe sequence
    <$> for [firstObj .. firstObj+objectCount-1] (\i-> (,) i <$> xrefEntry)

-- | One 20-byte xref entry.  Returns 'Nothing' for free entries.
xrefEntry :: PDFParser (Maybe XRefEntry)
xrefEntry = do
  ByteStringUTF8 bs <- getInput
  let entry = BS.take 20 bs
  _ <- string (ByteStringUTF8 entry)
  pure (parseXRefEntry entry)

-- | Parse one 20-byte xref entry.  Returns 'Nothing' for free entries.
parseXRefEntry :: ByteString -> Maybe XRefEntry
parseXRefEntry entry
  | BS.length entry >= 18 && BSC.index entry 17 == 'f' = Nothing
  | otherwise = case BSC.readInt (BS.take 10 entry) of
      Just (n, _) -> Just (XRefOffset (fromIntegral n))
      Nothing     -> Nothing  -- malformed entry, skip

-- ---------------------------------------------------------------------------
-- Stream parsing and decompression

-- | Parse an indirect stream object at the given byte offset in the file.
-- Returns the stream dictionary and the raw (possibly compressed) stream bytes.
parseStreamAt :: ByteString -> Int64 -> Either String (Map ByteString PDFValue, ByteString)
parseStreamAt bs off = do
  let chunk = dropWS (BS.drop (fromIntegral off) bs)
  -- Skip "N G obj"
  let (_, r1) = BSC.span isDigit chunk
      (_, r2) = BSC.span isDigit (dropWS r1)
      r3      = dropWS r2
  after <- case BSC.stripPrefix "obj" r3 of
    Just r  -> Right (dropWS r)
    Nothing -> Left ("Expected 'obj' at offset " <> show off)
  (dict, rest) <- parseDict after
  -- The next token should be "stream".
  let rest' = dropWS rest
  case BS.stripPrefix "stream" rest' of
    Nothing       -> Left ("Expected 'stream' keyword at offset " <> show off)
    Just afterKW  -> do
      -- Skip exactly one EOL (CR, LF, or CRLF) after the keyword.
      let streamStart = case BSC.uncons afterKW of
            Just ('\r', r) -> case BSC.uncons r of
                                Just ('\n', r') -> r'
                                _               -> r
            Just ('\n', r) -> r
            _              -> afterKW
      -- /Length must be a direct integer in xref streams; may be indirect
      -- in object streams (resolved after xref is built, see loadFromObjStream).
      len <- case Map.lookup "Length" dict of
               Just (PDFInt n) -> Right n
               Just _          -> Left "Stream /Length is not a direct integer"
               Nothing         -> Left "Stream dict missing /Length"
      Right (dict, BS.take len streamStart)

-- ---------------------------------------------------------------------------
-- Cross-reference stream (PDF 1.5+)

-- | Parse a cross-reference stream at the given byte offset.
-- The xref stream dictionary doubles as the trailer dictionary.
parseXRefStream :: ByteString -> Int64 -> Either String (XRef, Map ByteString PDFValue)
parseXRefStream bs off = do
  (dict, rawBytes) <- parseStreamAt bs off
  -- Decompress stream data if needed.
  streamBytes <- decompressStream dict rawBytes
  -- /W field: widths (in bytes) of the three entry fields.
  wArr <- case Map.lookup "W" dict of
    Just (PDFArray ws) -> Right ws
    _ -> Left "XRef stream missing /W"
  ws <- mapM toInt wArr
  case ws of
    [w1, w2, w3] -> do
      -- /Size: total number of object slots.
      size <- toInt =<< maybe (Left "XRef stream missing /Size") Right (Map.lookup "Size" dict)
      -- /Index: subsection pairs [firstObj count ...]; default is [0 /Size].
      let subsections = case Map.lookup "Index" dict of
                          Just (PDFArray idxArr) -> pairsOf idxArr
                          _                      -> [(0, size)]
      let xref = parseXRefStreamEntries w1 w2 w3 subsections streamBytes
      return (xref, dict)
    _ -> Left ("/W in XRef stream must have exactly 3 elements, got " <> show (length ws))

-- | Convert a list of PDFValues into pairs of Ints, used to decode the
-- /Index array.
pairsOf :: [PDFValue] -> [(Int, Int)]
pairsOf (PDFInt a : PDFInt b : rest) = (a, b) : pairsOf rest
pairsOf _                            = []

-- | Parse all entries from a cross-reference stream.
--
-- Per PDF spec (ISO 32000 §7.5.8.2): when a field width is 0 the field is
-- absent and defaults to its specification-defined value:
--
--   * field 1 (type): default 1 (in-use / offset entry)
--   * field 2 (byte offset or object stream number): default 0
--   * field 3 (generation number or object stream index): default 0
--
-- Fields 2 and 3 naturally default to 0 because 'readBEBytes' returns 0 for
-- a zero-width field.  Only field 1 needs an explicit check since its default
-- (1) differs from the zero returned by 'readBEBytes'.
parseXRefStreamEntries
  :: Int            -- ^ w1: width of type field (bytes); 0 means default type 1
  -> Int            -- ^ w2: width of field 2 (bytes)
  -> Int            -- ^ w3: width of field 3 (bytes)
  -> [(Int, Int)]   -- ^ subsections as (firstObj, count) pairs
  -> ByteString     -- ^ decompressed stream bytes
  -> XRef
parseXRefStreamEntries w1 w2 w3 subsections streamBytes =
  go 0 subsections IntMap.empty
  where
    entrySize = w1 + w2 + w3
    go _   []                        acc = acc
    go pos ((firstObj, count) : rest) acc =
      let newEntries =
            [ let typ = if w1 == 0 then 1 else readBEBytes w1 entryBytes
              in case typ of
                0 -> Nothing   -- free
                1 -> Just (firstObj + i,
                           XRefOffset (fromIntegral (readBEBytes w2 (BS.drop w1 entryBytes))))
                2 -> Just (firstObj + i,
                           XRefObjStm (readBEBytes w2 (BS.drop w1 entryBytes))
                                      (readBEBytes w3 (BS.drop (w1 + w2) entryBytes)))
                _ -> Nothing   -- unknown type, skip
            | i <- [0 .. count - 1]
            , let entryBytes = BS.drop (pos + i * entrySize) streamBytes
            ]
          acc' = foldl' insertEntry acc newEntries
      in go (pos + count * entrySize) rest acc'
    insertEntry m Nothing       = m
    insertEntry m (Just (k, v)) = IntMap.insert k v m

-- | Read @n@ bytes as a big-endian unsigned integer.
readBEBytes :: Int -> ByteString -> Int
readBEBytes n bs = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0 (BS.take n bs)

-- | Skip a line ending (CR, LF, or CRLF) from the front.
skipLineEnd :: PDFParser ()
skipLineEnd = takeCharsWhile (== ' ') *> void (string "\r\n" <<|> string "\r" <<|> string "\n")
