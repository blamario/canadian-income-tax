{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | PDF stream decompression
--
-- References: PDF 32000-1:2008 (PDF 1.7 specification):
--
-- * §7.5 – File structure (cross-reference tables and streams, incremental updates)

module Text.FDF.PDF.Decompress (decompressStream) where

import qualified Codec.Compression.Zlib as Zlib
import Control.Exception (SomeException, evaluate, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word8)
import System.IO.Unsafe (unsafePerformIO)

import Text.FDF.PDF.Types

-- | Apply predictor un-filtering as specified by the @/DecodeParms@ entry in
-- @dict@.  Only PNG predictors (Predictor 10–15) are handled; other values
-- (including TIFF Predictor 2) are passed through unchanged.
--
-- PNG predictors are heavily used in PDF cross-reference streams: Adobe
-- Acrobat writes @\/FlateDecode@ streams with @\/Predictor 12@ (PNG Up),
-- which prepends a 1-byte filter-type indicator to every data row.
-- Without un-predicting the decompressed bytes, xref entries are read with
-- completely wrong type/offset values.
applyDecodeParms :: Map ByteString PDFValue -> ByteString -> Either String ByteString
applyDecodeParms dict bs =
  let params = case Map.lookup "DecodeParms" dict of
                 Just (PDFDict d)             -> d
                 Just (PDFArray (PDFDict d:_)) -> d
                 _                            -> Map.empty
  in case Map.lookup "Predictor" params of
       Just (PDFInt p) | p >= 10 ->
         let cols = case Map.lookup "Columns" params of
                      Just (PDFInt c) -> c
                      _               -> 1  -- PDF spec default
         in applyPNGFilters cols bs
       _ -> Right bs

-- | Decode PNG row filters from a decompressed byte stream.
--
-- After FlateDecode decompression, PDF streams using a PNG predictor
-- (Predictor 10–15) have each data row prefixed with a 1-byte filter-type
-- indicator.  The row width (not counting that prefix byte) is @cols@ bytes.
-- This function strips the prefix bytes and applies the corresponding inverse
-- filter to reconstruct the original row data.
applyPNGFilters :: Int -> ByteString -> Either String ByteString
applyPNGFilters cols decompressed
  | cols <= 0 = Left "PNG predictor: /Columns must be positive"
  | BS.length decompressed `mod` stride /= 0
      = Left ("PNG predictor: data length " <> show (BS.length decompressed)
              <> " not a multiple of row stride " <> show stride)
  | otherwise = Right result
  where
    stride  = cols + 1
    numRows = BS.length decompressed `div` stride
    (result, _) =
      foldl' step (BS.empty, BS.replicate cols 0) [0 .. numRows - 1]
    step (acc, prev) i =
      let pos     = i * stride
          filt    = BS.index decompressed pos
          rawRow  = BS.take cols (BS.drop (pos + 1) decompressed)
          decoded = unfilterRow filt rawRow prev
      in (acc <> decoded, decoded)

-- | Apply the inverse of a single PNG scanline filter.
-- @bpp@ (bytes per pixel) is assumed to be 1, which is always the case for
-- PDF cross-reference stream entries (single-byte components).
unfilterRow :: Word8 -> ByteString -> ByteString -> ByteString
unfilterRow filt rawRow prevRow =
  case filt of
    0 -> rawRow   -- None
    1 ->          -- Sub: decoded[i] = raw[i] + decoded[i-1]
      snd $ BS.mapAccumL subStep 0 rawRow
    2 ->          -- Up: decoded[i] = raw[i] + prev[i]
      BS.pack $ BS.zipWith addMod rawRow prevRow
    3 ->          -- Average: decoded[i] = raw[i] + floor((decoded[i-1] + prev[i]) / 2)
      snd $ foldl' avgStep (0, BS.empty) (zip [0..] (BS.unpack rawRow))
    4 ->          -- Paeth: decoded[i] = raw[i] + paeth(decoded[i-1], prev[i], prev[i-1])
      snd $ foldl' paethStep (0, BS.empty) (zip [0..] (BS.unpack rawRow))
    _ -> rawRow   -- Unknown, treat as None
  where
    subStep :: Word8 -> Word8 -> (Word8, Word8)
    subStep prev b = let d = prev + b in (d, d)  -- Word8 wraps at 256

    addMod :: Word8 -> Word8 -> Word8
    addMod r p = r + p  -- Word8 addition wraps naturally

    getPrev i
      | i < BS.length prevRow = fromIntegral (BS.index prevRow i)
      | otherwise              = 0 :: Int

    avgStep :: (Int, ByteString) -> (Int, Word8) -> (Int, ByteString)
    avgStep (prevDec, acc) (i, b) =
      let a  = prevDec
          pv = getPrev i
          d  = fromIntegral (fromIntegral b + (a + pv) `div` 2 :: Int)
      in (fromIntegral d, BS.snoc acc d)

    paethStep :: (Int, ByteString) -> (Int, Word8) -> (Int, ByteString)
    paethStep (prevDec, acc) (i, b) =
      let a  = prevDec
          bv = getPrev i
          cv = if i > 0 then getPrev (i - 1) else 0
          pr = paethPredictor a bv cv
          d  = fromIntegral (fromIntegral b + pr :: Int)
      in (fromIntegral d, BS.snoc acc d)

    paethPredictor :: Int -> Int -> Int -> Int
    paethPredictor a b c =
      let p  = a + b - c
          pa = abs (p - a)
          pb = abs (p - b)
          pc = abs (p - c)
      in if pa <= pb && pa <= pc then a
         else if pb <= pc        then b
         else                         c

-- | Decompress stream bytes according to the /Filter and /DecodeParms entries
-- in the stream dict.  Only /FlateDecode (zlib) is currently supported as a
-- compression filter; other filters return an error.  PNG predictors
-- (Predictor 10–15, as commonly used in PDF cross-reference streams) are
-- applied after decompression.  If there is no /Filter, the bytes are
-- returned unchanged.
decompressStream :: Map ByteString PDFValue -> ByteString -> Either String ByteString
decompressStream dict rawBytes = do
  decompressed <- case Map.lookup "Filter" dict of
    Nothing                                 -> Right rawBytes
    Just (PDFName "FlateDecode")            -> zlibDecompress rawBytes
    Just (PDFArray [PDFName "FlateDecode"]) -> zlibDecompress rawBytes
    Just f -> Left ("Unsupported stream filter: " <> show f)
  applyDecodeParms dict decompressed
  where
    -- 'Zlib.decompress' operates on lazy ByteStrings and throws a
    -- 'DecompressError' exception on invalid data.  We catch it inside
    -- 'unsafePerformIO' so the rest of the module can stay in Either.
    -- This is referentially transparent: the same compressed bytes always
    -- produce the same result (or the same error).
    zlibDecompress bs = unsafePerformIO $ do
      result <- try (evaluate (LBS.toStrict (Zlib.decompress (LBS.fromStrict bs))))
      return $ case (result :: Either SomeException ByteString) of
        Right decompressed -> Right decompressed
        Left  e            -> Left ("Stream decompression error: " <> show e)
