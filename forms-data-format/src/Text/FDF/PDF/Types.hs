module Text.FDF.PDF.Types where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)

-- ---------------------------------------------------------------------------
-- PDF value types

data PDFValue
  = PDFNull
  | PDFBool Bool
  | PDFInt Int
  | PDFReal Double
  | PDFName ByteString          -- without leading '/'
  | PDFString ByteString        -- decoded raw bytes (may be UTF-16BE)
  | PDFArray [PDFValue]
  | PDFDict (Map ByteString PDFValue)
  | PDFRef Int Int              -- object number, generation number
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- XRef

-- | A single cross-reference entry.
data XRefEntry
  = XRefOffset Int64   -- ^ byte offset of the object in the file
  | XRefObjStm Int Int -- ^ compressed: (object stream obj number, index in stream)
  deriving (Eq, Show)

-- | Cross-reference table: maps object number to its location.
type XRef = IntMap XRefEntry

type ObjRef = (Int, Int)  -- object number, generation

-- | Convert a 'PDFValue' to an 'Int', failing with a message otherwise.
toInt :: PDFValue -> Either String Int
toInt (PDFInt n) = Right n
toInt v          = Left $ "Expected integer, got: " <> show v
