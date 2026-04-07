{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | PDF stream decryption and encryption (PDF Standard Security Handler, R=4, AES-128-CBC)
--
-- References: PDF 32000-1:2008 (PDF 1.7 specification):
--
-- * §7.6 – Encryption (Standard Security Handler, AES-128-CBC, per-object keys)

module Text.FDF.PDF.Decrypt (Decryptor, Encryptor, buildDecryptor, noDecrypt) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Bits (shiftR)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Crypto.Hash as CHash
import qualified Crypto.Cipher.AES as CAES
import qualified Crypto.Cipher.Types as CCipher
import qualified Crypto.Error as CError
import qualified Data.ByteArray as BA

import Text.FDF.PDF.Parse (hexDigit, parseIndirectObject)
import Text.FDF.PDF.Types

-- | A stream decryptor: given @(objectNumber, generationNumber, rawCiphertext)@
-- returns the decrypted bytes.  The no-op 'noDecrypt' is used for unencrypted PDFs.
type Decryptor = Int -> Int -> ByteString -> Either String ByteString

-- | No-op decryptor for unencrypted PDFs.
noDecrypt :: Decryptor
noDecrypt _ _ bs = Right bs

-- | Build a 'Decryptor' and matching 'Encryptor' from the trailer dictionary.
-- Returns 'noDecrypt' / 'noEncrypt' for unencrypted PDFs.
-- Only the Standard Security Handler with @\/V 4@ \/ @\/R ≥ 3@ (AES-128-CBC)
-- and an empty user password is currently supported.
buildDecryptor
  :: ByteString
  -> XRef
  -> Map ByteString PDFValue
  -> Either String (Decryptor, Encryptor)
buildDecryptor bs xref trailer =
  case Map.lookup "Encrypt" trailer of
    Nothing -> Right (noDecrypt, noEncrypt)
    Just (PDFRef n g) -> do
      encDict <- loadEncryptDict bs xref n g
      buildDecryptorFromDict encDict trailer
    Just (PDFDict encDict) -> buildDecryptorFromDict encDict trailer
    _ -> Left "Invalid /Encrypt entry in trailer"

-- | Build a 'Decryptor' and 'Encryptor' from a parsed /Encrypt dictionary.
buildDecryptorFromDict
  :: Map ByteString PDFValue
  -> Map ByteString PDFValue
  -> Either String (Decryptor, Encryptor)
buildDecryptorFromDict encDict trailer = do
  case Map.lookup "Filter" encDict of
    Just (PDFName "Standard") -> return ()
    Just f -> Left ("Unsupported encryption filter: " <> show f)
    Nothing -> Left "Encrypt dict missing /Filter"
  -- Extract first element of /ID array as the file identifier.
  fileId <- case Map.lookup "ID" trailer of
    Just (PDFArray (PDFString i : _)) -> Right i
    _ -> Left "Trailer /ID missing or not an array of strings"
  key <- computeFileEncKey encDict fileId
  Right (makeAESDecryptor key, makeAESEncryptor key)

-- | Load the Encrypt dictionary by direct offset (it is never itself
-- encrypted, so no decryptor is needed).
loadEncryptDict :: ByteString -> XRef -> Int -> Int -> Either String (Map ByteString PDFValue)
loadEncryptDict bs xref n _genNum =
  case IntMap.lookup n xref of
    Nothing -> Left $ "Encrypt object " <> show n <> " not in xref"
    Just (XRefOffset off) -> do
      v <- parseIndirectObject bs off
      case v of
        PDFDict d -> Right d
        _ -> Left "Encrypt object is not a dictionary"
    Just (XRefObjStm _ _) -> Left "Encrypt dict inside an ObjStm (not supported)"

-- | Compute the 16-byte file encryption key for the empty user password using
-- the Standard Security Handler algorithm (PDF spec §7.6.3.3, Algorithm 2).
-- This handles R=3 and R=4 (the iterations step).
computeFileEncKey
  :: Map ByteString PDFValue   -- ^ the @\/Encrypt@ dictionary
  -> ByteString                -- ^ first element of the @\/ID@ array
  -> Either String ByteString
computeFileEncKey encDict fileId = do
  p <- case Map.lookup "P" encDict of
         Just (PDFInt n) -> Right n
         _ -> Left "Encrypt dict missing /P"
  oVal <- case Map.lookup "O" encDict of
            Just (PDFString s) -> Right s
            _ -> Left "Encrypt dict missing /O"
  let n        = 16   -- AES-128 key: 128 bits / 8
      -- Step a: pad empty password to 32 bytes using the standard padding string.
      padded   = BS.take 32 (pdfPasswordPadding <> BS.replicate 32 0)
      -- Step d: pack P as signed 32-bit little-endian.
      p32      = BS.pack [ fromIntegral  p
                         , fromIntegral (p `shiftR`  8)
                         , fromIntegral (p `shiftR` 16)
                         , fromIntegral (p `shiftR` 24) ]
      -- Steps b–e: MD5(padded_password ++ O ++ P_LE ++ FileID).
      h0       = md5Hash (padded <> BS.take 32 oVal <> p32 <> fileId)
      -- Step g: for R≥3, iterate MD5 50 more times (truncating to n bytes each round).
      key      = foldl' (\acc _ -> md5Hash (BS.take n acc)) h0 ([1..50] :: [Int])
  Right (BS.take n key)

-- | Standard 32-byte password padding string (PDF spec §7.6.3.3).
pdfPasswordPadding :: ByteString
pdfPasswordPadding = BS.pack
  [ 0x28, 0xBF, 0x4E, 0x5E, 0x4E, 0x75, 0x8A, 0x41
  , 0x64, 0x00, 0x4E, 0x56, 0xFF, 0xFA, 0x01, 0x08
  , 0x2E, 0x2E, 0x00, 0xB6, 0xD0, 0x68, 0x3E, 0x80
  , 0x2F, 0x0C, 0xA9, 0xFE, 0x64, 0x53, 0x69, 0x7A ]

-- | Build a stream 'Decryptor' that uses AES-128-CBC with per-object keys
-- derived from the given file encryption key (PDF spec §7.6.2, Algorithm 1
-- for AES streams).
makeAESDecryptor :: ByteString -> Decryptor
makeAESDecryptor fileKey objNum genNum rawStream
  | BS.length rawStream < 16 = Left "Encrypted stream too short for AES-IV"
  | otherwise =
      let -- Per-object key: MD5(fileKey ++ objNum[3LE] ++ genNum[2LE] ++ "sAlT")
          perObjKey = BS.take 16 $ md5Hash $
                        fileKey
                        <> BS.pack [ lo objNum, md objNum, hi objNum ]
                        <> BS.pack [ lo genNum, md genNum ]
                        <> "sAlT"
          iv  = BS.take 16 rawStream
          ct  = BS.drop 16 rawStream
      in aes128CbcDecrypt perObjKey iv ct
  where
    lo n = fromIntegral  n
    md n = fromIntegral (n `shiftR`  8)
    hi n = fromIntegral (n `shiftR` 16)

-- | Compute the MD5 hash of a 'ByteString', returning the result as a
-- 16-byte 'ByteString'.
--
-- We obtain the hash via 'show' (which produces lowercase hex) and convert
-- back to bytes, avoiding any 'ByteArray'/'ByteArrayAccess' typeclass issues
-- between 'crypton' and 'memory'.
md5Hash :: ByteString -> ByteString
md5Hash bs =
  let hexStr = BSC.pack (show (CHash.hashWith CHash.MD5 bs))
  in BS.pack [ fromIntegral (hexDigit (BSC.index hexStr (2 * i)) * 16
                           + hexDigit (BSC.index hexStr (2 * i + 1)))
             | i <- [0 .. 15] ]

-- | AES-128-CBC decrypt, then strip PKCS#7 padding.
-- Converts all ByteStrings to 'BA.Bytes' for the crypto operations to avoid
-- relying on the 'ByteArray ByteString' instance from the 'memory' package.
aes128CbcDecrypt :: ByteString -> ByteString -> ByteString -> Either String ByteString
aes128CbcDecrypt key iv ct =
  let keyB = BA.convert key :: BA.Bytes
  in case (CCipher.cipherInit keyB :: CError.CryptoFailable CAES.AES128) of
       CError.CryptoPassed cipher ->
         let ivB = BA.convert iv :: BA.Bytes
         in case (CCipher.makeIV ivB :: Maybe (CCipher.IV CAES.AES128)) of
              Nothing  -> Left "AES: invalid IV length"
              Just iv' ->
                let ctB  = BA.convert ct :: BA.Bytes
                    plain :: BA.Bytes
                    plain = CCipher.cbcDecrypt cipher iv' ctB
                in Right (pkcs7Unpad (BS.pack (BA.unpack plain)))
       CError.CryptoFailed err -> Left ("AES init error: " <> show err)

-- | Remove PKCS#7 padding from a decrypted block.
pkcs7Unpad :: ByteString -> ByteString
pkcs7Unpad bs
  | BS.null bs = bs
  | otherwise  =
      let pad = fromIntegral (BS.last bs)
      in if pad >= 1 && pad <= 16 && pad <= BS.length bs
         then BS.dropEnd pad bs
         else bs

-- | Add PKCS#7 padding to reach the next multiple of 16 bytes (1–16 bytes added).
pkcs7Pad :: ByteString -> ByteString
pkcs7Pad bs =
  let padLen  = 16 - (BS.length bs `mod` 16)
      padByte = fromIntegral padLen
  in bs <> BS.replicate padLen padByte

-- | AES-128-CBC encrypt (no padding; caller must pre-pad to a block multiple).
aes128CbcEncrypt :: ByteString -> ByteString -> ByteString -> ByteString
aes128CbcEncrypt key iv plaintext =
  let keyB = BA.convert key :: BA.Bytes
  in case (CCipher.cipherInit keyB :: CError.CryptoFailable CAES.AES128) of
       CError.CryptoPassed cipher ->
         let ivB = BA.convert iv :: BA.Bytes
         in case (CCipher.makeIV ivB :: Maybe (CCipher.IV CAES.AES128)) of
              Nothing  -> plaintext  -- cannot happen: IV is always 16 bytes
              Just iv' ->
                let ptB :: BA.Bytes
                    ptB = BA.convert plaintext
                    ct :: BA.Bytes
                    ct  = CCipher.cbcEncrypt cipher iv' ptB
                in BS.pack (BA.unpack ct)
       CError.CryptoFailed _ -> plaintext  -- cannot happen: key is always 16 bytes

-- | An object string encryptor: given (objectNumber, generationNumber, rawPlaintext)
-- returns the AES-128-CBC ciphertext with a 16-byte IV prepended.
-- The no-op 'noEncrypt' is used for unencrypted PDFs.
type Encryptor = Int -> Int -> ByteString -> ByteString

-- | No-op encryptor for unencrypted PDFs.
noEncrypt :: Encryptor
noEncrypt _ _ bs = bs

-- | Build a string 'Encryptor' that uses AES-128-CBC with a deterministic IV
-- derived from the per-object key (via MD5).  The resulting format is:
-- @16-byte IV || AES-CBC(plaintext with PKCS#7 padding)@.
makeAESEncryptor :: ByteString -> Encryptor
makeAESEncryptor fileKey objNum genNum plaintext =
  let perObjKey = BS.take 16 $ md5Hash $
                    fileKey
                    <> BS.pack [ lo objNum, md objNum, hi objNum ]
                    <> BS.pack [ lo genNum, md genNum ]
                    <> "sAlT"
      -- Derive a deterministic IV from the per-object key so that it is
      -- unique per object while remaining purely functional.
      iv  = BS.take 16 (md5Hash (perObjKey <> "AES-IV"))
      ct  = aes128CbcEncrypt perObjKey iv (pkcs7Pad plaintext)
  in iv <> ct
  where
    lo n = fromIntegral  n
    md n = fromIntegral (n `shiftR`  8)
    hi n = fromIntegral (n `shiftR` 16)
