{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parsing of PDF XRefs
--
-- References: PDF 32000-1:2008 (PDF 1.7 specification):
--
-- * §7.5 – File structure (cross-reference tables and streams, incremental updates)

module Text.FDF.PDF.Serialize (applyUpdate, appendIncrementalUpdate) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.Char (intToDigit)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Numeric (showFFloat)

import Text.FDF.PDF.Decrypt (Encryptor)
import Text.FDF.PDF.Types

-- ---------------------------------------------------------------------------
-- Applying updates

type UpdateAcc = Either String ([(ObjRef, Map ByteString PDFValue)], Int)

-- | Add a modified field entry for one leaf-value update.
applyUpdate
  :: Map (NonEmpty Text) (ObjRef, Map ByteString PDFValue)
  -> UpdateAcc
  -> (NonEmpty Text, Text)
  -> UpdateAcc
applyUpdate _pathMap (Left err) _ = Left err
applyUpdate pathMap (Right (objs, maxN)) (path, newVal) =
  case Map.lookup path pathMap of
    Nothing        -> Right (objs, maxN)  -- field not in PDF, skip
    Just (ref, d)  ->
      -- Preserve the original /V type: name fields stay as PDFName, strings as PDFString.
      let pdfVal = case Map.lookup "V" d of
                     Just (PDFName _) -> PDFName (Text.encodeUtf8 newVal)
                     _                -> PDFString (encodePDFStringValue newVal)
      in if Map.lookup "V" d == Just pdfVal
           then Right (objs, maxN)  -- already at the desired value; skip (idempotency)
           else let newDict = Map.insert "V" pdfVal d
                in Right ((ref, newDict) : objs, maxN)

-- ---------------------------------------------------------------------------
-- Incremental update writer

-- | Append new object versions and an updated xref/trailer to @pdfBytes@.
appendIncrementalUpdate
  :: Encryptor
  -> ByteString
  -> Int64                                           -- previous xref offset
  -> Map ByteString PDFValue                         -- original trailer
  -> [(ObjRef, Map ByteString PDFValue)]             -- updated objects
  -> ByteString
appendIncrementalUpdate enc pdfBytes prevXrefOff origTrailer updatedObjs =
  let baseLen  = fromIntegral (BS.length pdfBytes)
      -- Serialize each updated object and record its new offset.
      (objBlocks, offsets) = buildObjBlocks enc baseLen updatedObjs
      -- Build new xref section.
      newXrefOff           = baseLen + fromIntegral (LBS.length (BB.toLazyByteString (mconcat objBlocks)))
      newXref              = buildXRefSection offsets
      -- Build new trailer.
      origSize = fromMaybe 0 $ do
                   PDFInt n <- Map.lookup "Size" origTrailer
                   return n
      newSize  = origSize   -- we reuse old object numbers
      newTrailer = buildTrailerSection newSize prevXrefOff origTrailer
  in LBS.toStrict $ BB.toLazyByteString $
       BB.byteString pdfBytes
       <> mconcat objBlocks
       <> BB.byteString newXref
       <> BB.byteString newTrailer
       <> "startxref\n"
       <> BB.int64Dec newXrefOff <> "\n"
       <> "%%EOF\n"

-- | Serialize updated objects and return (BB blocks, (objRef, offset) list).
buildObjBlocks
  :: Encryptor
  -> Int64
  -> [(ObjRef, Map ByteString PDFValue)]
  -> ([BB.Builder], [(ObjRef, Int64)])
buildObjBlocks enc startOff objs =
  let go off [] = ([], [])
      go off ((ref@(n,g), dict) : rest) =
        let block   = serializeObj n g (encryptPDFValues enc n g dict)
            blockBS = LBS.toStrict (BB.toLazyByteString block)
            len     = fromIntegral (BS.length blockBS)
            (blocks, offsets) = go (off + len) rest
        in (block : blocks, (ref, off) : offsets)
  in go startOff objs

serializeObj :: Int -> Int -> Map ByteString PDFValue -> BB.Builder
serializeObj n g dict =
     BB.intDec n <> " " <> BB.intDec g <> " obj\n"
  <> serializeDict dict <> "\n"
  <> "endobj\n"

serializeDict :: Map ByteString PDFValue -> BB.Builder
serializeDict d =
  "<<\n" <> Map.foldlWithKey' go mempty d <> ">>"
  where
    go acc k v =
      acc <> "/" <> BB.byteString k <> " " <> serializeValue v <> "\n"

serializeValue :: PDFValue -> BB.Builder
serializeValue = \case
  PDFNull         -> "null"
  PDFBool True    -> "true"
  PDFBool False   -> "false"
  PDFInt n        -> BB.intDec n
  PDFReal r       -> BB.string7 (showFFloat Nothing r "")
  PDFName nm      -> "/" <> BB.byteString nm
  PDFString bs    -> serializePDFString bs
  PDFArray vs     -> "[" <> foldMap (\v -> serializeValue v <> " ") vs <> "]"
  PDFDict d       -> serializeDict d
  PDFRef n g      -> BB.intDec n <> " " <> BB.intDec g <> " R"

-- | Serialize a raw string value using parentheses notation (ASCII) or
-- angle-bracket hex notation (non-ASCII / binary).
serializePDFString :: ByteString -> BB.Builder
serializePDFString bs
  | isAsciiSafe bs = "(" <> BB.byteString (escapeLiteral bs) <> ")"
  | otherwise      = "<" <> BB.byteString (hexEncode bs) <> ">"
  where
    isAsciiSafe = BS.all (\w -> w >= 0x20 && w <= 0x7E
                              && w /= 0x28 && w /= 0x29 && w /= 0x5C)

-- | Escape special characters inside a PDF literal string.
escapeLiteral :: ByteString -> ByteString
escapeLiteral = BS.concatMap escape
  where
    escape 0x28 = "\\("
    escape 0x29 = "\\)"
    escape 0x5C = "\\\\"
    escape w    = BS.singleton w

-- | Encode a 'Text' value as a raw PDF string, using UTF-16BE for non-ASCII.
encodePDFStringValue :: Text -> ByteString
encodePDFStringValue t
  | Text.all isAsciiPrintable t = Text.encodeUtf8 t
  | otherwise                   = "\xFE\xFF" <> Text.encodeUtf16BE t
  where
    isAsciiPrintable c = c >= ' ' && c <= '~'

hexEncode :: ByteString -> ByteString
hexEncode = BS.concatMap
  (\w -> BSC.pack [intToDigit (fromIntegral (w `div` 16)),
                   intToDigit (fromIntegral (w `mod` 16))])

-- | Build the xref section for the incremental update.
-- Groups updated objects into consecutive subsections under a single
-- @xref@ keyword, as required by the PDF specification.
buildXRefSection :: [(ObjRef, Int64)] -> ByteString
buildXRefSection [] = ""
buildXRefSection offsets =
  let entries = map (\((n, _), off) -> (n, off)) offsets
      sorted  = Map.toAscList $
                  foldl' (\m (n, off) -> Map.insert n off m) Map.empty entries
  in BSC.pack $ "xref\n" <> concatMap renderSubsection (groupConsecutive sorted)
  where
    renderSubsection [] = ""
    renderSubsection grp@((n, _) : _) =
      show n <> " " <> show (length grp) <> "\n"
        <> concatMap (\(_, off) -> padDec10 off <> " 00000 n\r\n") grp

    groupConsecutive :: [(Int, Int64)] -> [[(Int, Int64)]]
    groupConsecutive [] = []
    groupConsecutive [x] = [[x]]
    groupConsecutive (x@(n1, _) : rest@((n2, _) : _))
      | n2 == n1 + 1 = case groupConsecutive rest of
                         []          -> [[x]]
                         (grp : grps) -> (x : grp) : grps
      | otherwise    = [x] : groupConsecutive rest

padDec10 :: Int64 -> String
padDec10 n = let s = show n in replicate (10 - length s) '0' <> s

-- | Build the trailer section (without @startxref@ line).
buildTrailerSection
  :: Int
  -> Int64
  -> Map ByteString PDFValue
  -> ByteString
buildTrailerSection size prevOff origTrailer =
  let td = Map.fromList $
              [ ("Size", PDFInt size)
              , ("Prev", PDFInt (fromIntegral prevOff))
              ] <>
              -- Carry over document-level entries from the original trailer.
              -- /Encrypt and /ID must both be present when the source PDF is
              -- encrypted: /Encrypt tells readers to decrypt the original body
              -- objects (pages, fonts, etc.), and /ID is required by the
              -- Standard Security Handler algorithm to derive the file key
              -- (PDF spec §7.6.3.3 Algorithm 2).  Omitting either causes
              -- readers to either prompt for a password or fail to read the
              -- original page tree.
              [ (k, v)
              | k <- ["Root", "Info", "Encrypt", "ID"]
              , Just v <- [Map.lookup k origTrailer]
              ]
  in LBS.toStrict $ BB.toLazyByteString $
       "trailer\n" <> serializeDict td <> "\n"

-- | Encrypt all 'PDFString' leaf values in a dictionary using the given
-- 'Encryptor' (for a specific object number and generation number).
-- 'PDFName', 'PDFRef', and other non-string values are left unchanged.
encryptPDFValues :: Encryptor -> Int -> Int -> Map ByteString PDFValue -> Map ByteString PDFValue
encryptPDFValues enc n g = Map.map go
  where
    go (PDFString bs) = PDFString (enc n g bs)
    go (PDFArray vs)  = PDFArray  (map go vs)
    go (PDFDict d)    = PDFDict   (Map.map go d)
    go v              = v
