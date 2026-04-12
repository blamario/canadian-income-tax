{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Conversion between PDF AcroForm fields and FDF format.
--
-- References: PDF 32000-1:2008 (PDF 1.7 specification):
--
-- * §7.3 – Objects (booleans, numbers, strings, names, arrays, dictionaries)
-- * §7.5 – File structure (cross-reference tables and streams, incremental updates)
-- * §7.6 – Encryption (Standard Security Handler, AES-128-CBC, per-object keys)
-- * §12.7 – Interactive forms (AcroForm, field dictionaries, @\/Kids@, @\/T@, @\/V@)
--
-- The module is intentionally self-contained (no external PDF library dependency).

module Text.FDF.PDF
  ( PDF (..)
  , parsePDF
  , fillPDF
  , serializePDF
  , fieldLabels
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isDigit, isSpace)
import Data.Int (Int64)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Text.FDF (FDF (..), Field (..), FieldContent (..))
import Text.FDF.PDF.Decompress (decompressStream)
import Text.FDF.PDF.Decrypt (Decryptor, Encryptor, buildDecryptor)
import Text.FDF.PDF.Labels (buildFieldLabels)
import Text.FDF.PDF.Parse (parseIndirectObject, parseValue, dropWS, parseDict)
import Text.FDF.PDF.Serialize (applyUpdate, appendIncrementalUpdate)
import Text.FDF.PDF.Types
import Text.FDF.PDF.XRef (parseXRefChain)

-- | A parsed PDF file, containing the extracted form data and the raw PDF
-- bytes.  The 'source' field preserves the original file content so that
-- 'fillPDF' can apply incremental updates and 'serializePDF' can reproduce
-- the byte-level representation.
data PDF = PDF
  { form   :: FDF         -- ^ the extracted AcroForm field data
  , source :: ByteString  -- ^ the raw PDF bytes
  } deriving (Eq, Show)

-- | Extract form field data from a PDF file.
--
-- Reads the PDF's AcroForm structure and returns a 'PDF' value containing
-- the corresponding 'FDF' form data together with the original bytes.
-- Supports both traditional (table-based) cross-reference sections
-- and cross-reference streams (PDF 1.5+), including FlateDecode-compressed
-- object streams.
parsePDF :: ByteString -> Either String PDF
parsePDF bs = do
  (_, xref, _trailer, dec, _enc, fieldsArr, _) <- loadAcroFormFields bs
  fields <- catMaybes <$> mapM (loadFieldObj bs xref dec) fieldsArr
  fdf <- case fields of
    []  -> Right $ FDF
             "1 0 obj\n"
             Field { name = "", content = Children [] }
             "endobj\ntrailer\n\n<<\n/Root 1 0 R\n>>\n"
    [f] -> Right $ FDF
             "1 0 obj\n"
             f
             "endobj\ntrailer\n\n<<\n/Root 1 0 R\n>>\n"
    _   -> Right $ FDF
             "1 0 obj\n"
             Field { name = "", content = Children fields }
             "endobj\ntrailer\n\n<<\n/Root 1 0 R\n>>\n"
  Right PDF { form = fdf, source = bs }

-- | Fill the form fields of a PDF using an 'FDF' value.
--
-- Uses an incremental-update append so the original PDF bytes are left intact.
-- For encrypted PDFs (Standard Security Handler, empty user password), the
-- new field-value objects are AES-encrypted with the same key as the original
-- body; the incremental update trailer carries both @\/Encrypt@ and @\/ID@ so
-- readers can decrypt both the original body objects and the new field objects.
fillPDF :: FDF -> PDF -> Either String PDF
fillPDF fdf pdf = do
  let pdfBytes = source pdf
  (xrefOff, xref, trailer, dec, enc, fieldsArr, acroInfo) <- loadAcroFormFields pdfBytes
  -- Build mapping:  full path → (objNum, current dict)
  pathMap      <- buildPathMap pdfBytes xref dec [] fieldsArr
  -- Collect leaf-value updates from FDF
  let updates   = collectUpdates [] (body fdf)
  -- Apply updates: produce list of (objNum, new dict)
  let totalObjs = fromMaybe 0 $ do
        PDFInt n <- Map.lookup "Size" trailer
        return n
  (fieldObjs, _) <- foldl' (applyUpdate pathMap) (Right ([], totalObjs)) updates
  -- When the AcroForm carries /XFA, emit an updated AcroForm dictionary
  -- with /XFA removed.  PDF viewers that encounter both XFA and AcroForm
  -- data may ignore the AcroForm field values we write, displaying stale
  -- (empty) XFA data instead.  Stripping /XFA forces them to fall back to
  -- the AcroForm fields.
  let newObjs = case acroInfo of
        Just (ref, dict) -> (ref, Map.delete "XFA" dict) : fieldObjs
        Nothing          -> fieldObjs
  let newBytes = if null newObjs
        then pdfBytes
        else appendIncrementalUpdate enc pdfBytes xrefOff trailer newObjs
  -- Re-parse so the returned 'form' accurately reflects *all* fields in the
  -- filled PDF (including those not mentioned in the input FDF).
  parsePDF newBytes

-- | Serialize a 'PDF' value back to its byte-level representation.
serializePDF :: PDF -> ByteString
serializePDF = source

-- | Parse the PDF cross-reference tables and locate the AcroForm @\/Fields@
-- array, returning the xref offset, xref table, trailer dictionary,
-- decryptor, encryptor, field references, and optionally the AcroForm
-- object reference and dictionary (when @\/XFA@ is present and needs to be
-- stripped).
loadAcroFormFields
  :: ByteString
  -> Either String (Int64, XRef, Map ByteString PDFValue, Decryptor, Encryptor, [PDFValue],
                    Maybe (ObjRef, Map ByteString PDFValue))
loadAcroFormFields bs = do
  xrefOff         <- findXRefOffset bs
  (xref, trailer) <- parseXRefChain bs xrefOff
  (dec, enc)      <- buildDecryptor bs xref trailer
  rootRef         <- dictLookupRef "Root" trailer
  catalog         <- loadDict bs xref dec rootRef
  acroRef         <- dictLookupRef "AcroForm" catalog
  acroForm        <- loadDict bs xref dec acroRef
  fieldsArr       <- loadArray bs xref dec "Fields" acroForm
  -- When the AcroForm carries an /XFA entry, record the AcroForm object
  -- reference and dictionary so that fillPDF can strip the /XFA key.
  -- PDF viewers that encounter both XFA and AcroForm data may ignore the
  -- AcroForm field values that fillPDF writes, displaying stale (empty)
  -- XFA data instead.
  let acroInfo
        | Map.member "XFA" acroForm = Just (acroRef, acroForm)
        | otherwise                 = Nothing
  return (xrefOff, xref, trailer, dec, enc, fieldsArr, acroInfo)

-- ---------------------------------------------------------------------------
-- Finding startxref

-- | Locate the byte offset stored after the last @startxref@ keyword in the
-- file (searching the final 1 KiB).
findXRefOffset :: ByteString -> Either String Int64
findXRefOffset bs =
  let searchFrom = max 0 (BS.length bs - 1024)
      suffix     = BS.drop searchFrom bs
  in case findLast "startxref" suffix of
    Nothing  -> Left "Cannot find 'startxref' in PDF"
    Just rel ->
      let absOff  = searchFrom + rel + 9  -- skip past "startxref"
          rest    = BSC.dropWhile isSpace (BS.drop absOff bs)
      in case BSC.readInt rest of
           Just (n, _) -> Right (fromIntegral n)
           Nothing     -> Left "Cannot parse the xref offset after 'startxref'"

-- | Return the byte position of the last occurrence of @needle@ inside
-- @haystack@, or 'Nothing' if not found.
findLast :: ByteString -> ByteString -> Maybe Int
findLast needle haystack = go 0 Nothing
  where
    nLen = BS.length needle
    hLen = BS.length haystack
    go pos best
      | pos + nLen > hLen = best
      | BS.take nLen (BS.drop pos haystack) == needle = go (pos + 1) (Just pos)
      | otherwise                                     = go (pos + 1) best

-- ---------------------------------------------------------------------------
-- Object loading

-- | Load and dereference an object.  References are followed one level deep.
-- For objects at a byte offset (@XRefOffset@) in an encrypted PDF, all
-- 'PDFString' leaf values in the result are decrypted with @dec@ using the
-- per-object key (objNum, genNum).  Objects within an object stream
-- (@XRefObjStm@) do not need per-string decryption because the stream itself
-- is already decrypted before parsing (per PDF spec §7.6.5).
loadObject :: ByteString -> XRef -> Decryptor -> PDFValue -> Either String PDFValue
loadObject bs xref dec (PDFRef n g) =
  case IntMap.lookup n xref of
    Nothing                 -> Left $ "Object " <> show n <> " not in xref"
    Just (XRefOffset off)   -> parseIndirectObject bs off >>= decryptPDFValue dec n g
    Just (XRefObjStm sn ix) -> loadFromObjStream bs xref dec sn ix
loadObject _ _ _ v = Right v

-- | Recursively decrypt all 'PDFString' leaf values in a 'PDFValue' using the
-- supplied 'Decryptor' with the given per-object (@objNum@, @genNum@) key.
-- 'noDecrypt' makes this a no-op for unencrypted PDFs.
decryptPDFValue :: Decryptor -> Int -> Int -> PDFValue -> Either String PDFValue
decryptPDFValue dec n g (PDFString bs) = PDFString <$> dec n g bs
decryptPDFValue dec n g (PDFArray vs)  = PDFArray  <$> mapM (decryptPDFValue dec n g) vs
decryptPDFValue dec n g (PDFDict d)    = PDFDict   <$> mapM (decryptPDFValue dec n g) d
decryptPDFValue _   _ _ v              = Right v

-- | Load an object that must be a dictionary.
loadDict :: ByteString -> XRef -> Decryptor -> (Int, Int) -> Either String (Map ByteString PDFValue)
loadDict bs xref dec (n, g) = do
  v <- loadObject bs xref dec (PDFRef n g)
  case v of
    PDFDict d -> Right d
    _         -> Left $ "Object " <> show n <> " is not a dictionary"

-- | Load an object stored inside an object stream.
-- @stmObjNum@ is the object number of the ObjStm; @idx@ is the 0-based index
-- of the desired object within that stream.
loadFromObjStream :: ByteString -> XRef -> Decryptor -> Int -> Int -> Either String PDFValue
loadFromObjStream bs xref dec stmObjNum idx = do
  stmOff <- case IntMap.lookup stmObjNum xref of
    Just (XRefOffset off) -> Right off
    Just (XRefObjStm _ _) -> Left "Object stream is itself compressed (not supported)"
    Nothing               -> Left $ "Object stream " <> show stmObjNum <> " not in xref"
  -- Parse the stream, resolving /Length via the xref if it's indirect.
  (rawDict, rawBytes) <- parseStreamAtIndirectLen bs xref dec stmObjNum stmOff
  streamBytes         <- decompressStream rawDict rawBytes
  -- /N: number of objects; /First: byte offset of first object in body.
  n     <- toInt =<< maybe (Left "ObjStm missing /N")     Right (Map.lookup "N"     rawDict)
  first <- toInt =<< maybe (Left "ObjStm missing /First") Right (Map.lookup "First" rawDict)
  -- The header is a flat list of (objNum offset) pairs.
  offsets <- parseObjStmHeader (BS.take first streamBytes) n
  (_, relOff) <-
    case drop idx offsets of
      (entry : _) -> Right entry
      []          -> Left $ "Object stream index " <> show idx <> " out of range (n=" <> show n <> ")"
  let objBody = BS.drop (first + relOff) streamBytes
  fst <$> parseValue (dropWS objBody)

-- | Like 'parseStreamAt' but resolves an indirect @\/Length@ reference and
-- applies the decryptor before decompression.
parseStreamAtIndirectLen
  :: ByteString
  -> XRef
  -> Decryptor
  -> Int    -- ^ object number of the stream (for per-object decryption key)
  -> Int64
  -> Either String (Map ByteString PDFValue, ByteString)
parseStreamAtIndirectLen bs xref dec objNum off = do
  let chunk = dropWS (BS.drop (fromIntegral off) bs)
  let (_, r1) = BSC.span isDigit chunk
      (genStr, r2) = BSC.span isDigit (dropWS r1)
      r3      = dropWS r2
  genNum <- readDecimal genStr
  after <- case BSC.stripPrefix "obj" r3 of
    Just r  -> Right (dropWS r)
    Nothing -> Left ("Expected 'obj' at offset " <> show off)
  (dict0, rest) <- parseDict after
  -- Resolve indirect /Length if necessary.
  dict <- case Map.lookup "Length" dict0 of
    Just (PDFRef ln lg) -> do
      lenVal <- loadObject bs xref dec (PDFRef ln lg)
      case lenVal of
        PDFInt n -> Right (Map.insert "Length" (PDFInt n) dict0)
        _        -> Left "Resolved /Length is not an integer"
    _ -> Right dict0
  let rest' = dropWS rest
  case BS.stripPrefix "stream" rest' of
    Nothing      -> Left ("Expected 'stream' keyword at offset " <> show off)
    Just afterKW -> do
      let streamStart = case BSC.uncons afterKW of
            Just ('\r', r) -> case BSC.uncons r of
                                Just ('\n', r') -> r'
                                _               -> r
            Just ('\n', r) -> r
            _              -> afterKW
      len <- case Map.lookup "Length" dict of
               Just (PDFInt n) -> Right n
               _               -> Left "Stream /Length missing or not resolved"
      let rawBytes = BS.take len streamStart
      -- Decrypt before decompression (xref streams are not encrypted;
      -- ObjStm and other streams are encrypted when /Encrypt is present).
      decrypted <- dec objNum genNum rawBytes
      Right (dict, decrypted)

-- | Parse the object-number/offset header of an object stream.
-- Returns a list of @(objectNumber, relativeOffset)@ pairs.
parseObjStmHeader :: ByteString -> Int -> Either String [(Int, Int)]
parseObjStmHeader bs n = go (dropWS bs) n []
  where
    go _   0 acc = Right (reverse acc)
    go bs' k acc =
      let (numStr, r1) = BSC.span isDigit bs'
          (offStr, r2) = BSC.span isDigit (dropWS r1)
      in if BS.null numStr || BS.null offStr
         then Left "Truncated object stream header"
         else do
           num <- readDecimal numStr
           off <- readDecimal offStr
           go (dropWS r2) (k - 1) ((num, off) : acc)

-- ---------------------------------------------------------------------------
-- AcroForm field loading

-- | Load a single field object from a PDF reference.
loadFieldObj :: ByteString -> XRef -> Decryptor -> PDFValue -> Either String (Maybe Field)
loadFieldObj bs xref dec ref = do
  obj  <- loadObject bs xref dec ref
  dict <- case obj of
            PDFDict d -> Right d
            _         -> Left "Field is not a dictionary"
  buildField bs xref dec dict

-- | Build a 'Field' from a PDF field dictionary.
-- Returns 'Nothing' for widget annotations that have no @\/T@ entry and so
-- cannot participate in the FDF field hierarchy.
buildField :: ByteString -> XRef -> Decryptor -> Map ByteString PDFValue -> Either String (Maybe Field)
buildField bs xref dec dict =
  case Map.lookup "T" dict of
    Nothing -> Right Nothing  -- widget annotation without /T; skip it
    _ -> do
      t    <- decodeFieldText dict "T"
      cont <- case Map.lookup "Kids" dict of
        Just (PDFArray kids) -> kidsContent bs xref dec dict kids
        Just ref@PDFRef{} -> do
          kidsVal <- loadObject bs xref dec ref
          case kidsVal of
            PDFArray kids -> kidsContent bs xref dec dict kids
            _             -> Left "Kids is not an array"
        _ -> leafFieldValue dict
      return $ Just Field { name = t, content = cont }

-- | Build the 'FieldContent' for a field's @\/Kids@ array.
-- If all kids are anonymous widget annotations (no @\/T@), the field is
-- treated as a leaf and its own @\/V@ value is returned instead.
kidsContent
  :: ByteString -> XRef -> Decryptor
  -> Map ByteString PDFValue  -- ^ the parent field's dictionary (for fallback @\/V@)
  -> [PDFValue]               -- ^ the raw kid references
  -> Either String FieldContent
kidsContent bs xref dec parentDict kids = do
  namedKids <- catMaybes <$> mapM (loadFieldObj bs xref dec) kids
  if null namedKids
    then leafFieldValue parentDict   -- all kids are widget annotations → leaf
    else return (Children namedKids)

-- | Extract the @\/V@ field value from a PDF field dictionary as a 'FieldContent'.
-- PDF name objects (including empty ones, representing no-selection) are returned
-- as 'FieldNameValue'; string objects are returned as 'FieldValue'.
leafFieldValue :: Map ByteString PDFValue -> Either String FieldContent
leafFieldValue dict = case Map.lookup "V" dict of
  Nothing              -> Right (FieldValue "")
  Just (PDFString raw) -> Right (FieldValue (decodePDFString raw))
  Just (PDFName nm)    -> Right (FieldNameValue (Text.decodeLatin1 nm))
  Just _               -> Right (FieldValue "")

-- | Decode the value of a string-typed field entry as 'Text'.
decodeFieldText :: Map ByteString PDFValue -> ByteString -> Either String Text
decodeFieldText dict key =
  case Map.lookup key dict of
    Nothing             -> Left ("Field is missing /" <> BSC.unpack key)
    Just (PDFString bs) -> Right (decodePDFString bs)
    Just (PDFName nm)   -> Right (Text.decodeLatin1 nm)
    Just v              -> Left ("/" <> BSC.unpack key <> " has unexpected type: " <> show v)

-- | Decode a raw PDF string (literal or hex) to 'Text'.
-- Strings starting with a UTF-16BE BOM are decoded accordingly; otherwise
-- Latin-1 (PDFDocEncoding) is assumed.
decodePDFString :: ByteString -> Text
decodePDFString bs
  | "\xFE\xFF" `BS.isPrefixOf` bs = Text.decodeUtf16BE (BS.drop 2 bs)
  | otherwise                      = Text.decodeLatin1 bs

-- ---------------------------------------------------------------------------
-- Path → object mapping (for fillPDF)

-- | Build a map from field paths (encoded as slash-joined names) to the
-- corresponding (object number, generation, current dict) triple.
buildPathMap
  :: ByteString
  -> XRef
  -> Decryptor
  -> [Text]    -- ^ path prefix (ancestor names)
  -> [PDFValue]
  -> Either String (Map (NonEmpty Text) (ObjRef, Map ByteString PDFValue))
buildPathMap bs xref dec prefix refs = do
  entries <- mapM (buildPathEntry bs xref dec prefix) refs
  return (Map.unions entries)

buildPathEntry
  :: ByteString
  -> XRef
  -> Decryptor
  -> [Text]
  -> PDFValue
  -> Either String (Map (NonEmpty Text) (ObjRef, Map ByteString PDFValue))
buildPathEntry bs xref dec prefix ref = do
  (objNum, objGen) <- case ref of
    PDFRef n g -> Right (n, g)
    _          -> Left "Field entry in /Fields is not a reference"
  dict <- loadDict bs xref dec (objNum, objGen)
  case Map.lookup "T" dict of
    Nothing -> Right Map.empty  -- widget annotation without /T; skip it
    _ -> do
      t <- decodeFieldText dict "T"
      let path   = prefix ++ [t]
          pathNE = NonEmpty.fromList path  -- safe: appending [t] ensures non-empty
      case Map.lookup "Kids" dict of
        Just (PDFArray kids) -> do
          m <- buildPathMap bs xref dec path kids
          if Map.null m
            then Right $ Map.singleton pathNE ((objNum, objGen), dict)  -- all kids are widgets
            else Right m
        Just r@PDFRef{} -> do
          kidsVal <- loadObject bs xref dec r
          case kidsVal of
            PDFArray kids -> do
              m <- buildPathMap bs xref dec path kids
              if Map.null m
                then Right $ Map.singleton pathNE ((objNum, objGen), dict)
                else Right m
            _ -> Left "Kids is not an array"
        _ ->
          -- This is a leaf field.
          Right $ Map.singleton pathNE ((objNum, objGen), dict)

-- ---------------------------------------------------------------------------
-- Collecting FDF leaf values

-- | Return all leaf (path, value) pairs from an FDF body.
collectUpdates :: [Text] -> Field -> [(NonEmpty Text, Text)]
collectUpdates prefix Field { name = n, content = cont } =
  let path = if Text.null n then prefix else prefix ++ [n]
  in case cont of
    FieldValue v     -> case NonEmpty.nonEmpty path of
                          Nothing     -> []
                          Just pathNE -> [(pathNE, v)]
    FieldNameValue v -> case NonEmpty.nonEmpty path of
                          Nothing     -> []
                          Just pathNE -> [(pathNE, v)]
    Children kids    -> concatMap (collectUpdates path) kids

-- ---------------------------------------------------------------------------
-- helpers

dictLookupRef :: ByteString -> Map ByteString PDFValue -> Either String (Int, Int)
dictLookupRef key d =
  case Map.lookup key d of
    Just (PDFRef n g) -> Right (n, g)
    Just _            -> Left ("/" <> BSC.unpack key <> " is not a reference")
    Nothing           -> Left ("/" <> BSC.unpack key <> " not found in dict")

-- | Look up an array value in a dictionary, following an indirect reference
-- if needed.
loadArray
  :: ByteString
  -> XRef
  -> Decryptor
  -> ByteString                    -- ^ key name
  -> Map ByteString PDFValue
  -> Either String [PDFValue]
loadArray bs xref dec key d =
  case Map.lookup key d of
    Just (PDFArray a) -> Right a
    Just ref@PDFRef{} -> do
      v <- loadObject bs xref dec ref
      case v of
        PDFArray a -> Right a
        _          -> Left ("/" <> BSC.unpack key <> " reference is not an array")
    Just _ -> Left ("/" <> BSC.unpack key <> " is not an array")
    Nothing -> Left ("/" <> BSC.unpack key <> " not found in dict")

-- | Read a non-negative decimal integer from a 'ByteString', failing with
-- an error if the input does not start with at least one digit.
readDecimal :: ByteString -> Either String Int
readDecimal bs = case BSC.readInt bs of
  Just (n, _) -> Right n
  Nothing     -> Left ("Expected decimal integer, got: " <> BSC.unpack (BS.take 10 bs))

-- ---------------------------------------------------------------------------
-- Field labels: text fragments associated with form fields

-- | Extract a list of 'Field's that mirrors the AcroForm hierarchy, but
-- where each leaf value is the nearby page text (its label) rather than the
-- field's current value.
--
-- For each form field the function:
--
-- 1. reads the widget annotation's @\/Rect@ and @\/P@ (page reference);
-- 2. loads and decompresses the page's @\/Contents@ stream;
-- 3. extracts positioned text fragments from the content stream;
-- 4. selects text whose position is close to the field's rectangle.
--
-- The result preserves the field hierarchy: parent fields with named
-- children produce 'Children' nodes.  Fields without a locatable label
-- get an empty 'FieldValue'.
fieldLabels :: PDF -> Either String [Field]
fieldLabels pdf = do
  let bs = source pdf
  (_, xref, _trailer, dec, _enc, fieldsArr, _) <- loadAcroFormFields bs
  let objLoader  = loadObject bs xref dec
      pageLoader = loadPageStream bs xref dec
  buildFieldLabels objLoader pageLoader fieldsArr

-- | Load a page's decompressed content-stream bytes by page object number.
loadPageStream :: ByteString -> XRef -> Decryptor -> Int -> Either String ByteString
loadPageStream bs xref dec pageObjNum = do
  pageDict <- loadDict bs xref dec (pageObjNum, 0)
  loadContents bs xref dec pageDict

-- | Load and concatenate the page's @\/Contents@ stream(s).
-- @\/Contents@ may be a single stream reference or an array of references.
loadContents
  :: ByteString -> XRef -> Decryptor
  -> Map ByteString PDFValue
  -> Either String ByteString
loadContents bs xref dec dict =
  case Map.lookup "Contents" dict of
    Nothing -> Right BS.empty  -- page with no content stream
    Just (PDFRef n _) -> loadStreamBytes bs xref dec n
    Just (PDFArray refs) -> do
      chunks <- mapM (loadContentRef bs xref dec) refs
      Right (BS.concat chunks)
    _ -> Right BS.empty

loadContentRef :: ByteString -> XRef -> Decryptor -> PDFValue -> Either String ByteString
loadContentRef bs xref dec (PDFRef n _) = loadStreamBytes bs xref dec n
loadContentRef _ _ _ _                  = Right BS.empty

-- | Load a stream object by object number: parse, decrypt, and decompress.
loadStreamBytes :: ByteString -> XRef -> Decryptor -> Int -> Either String ByteString
loadStreamBytes bs xref dec objNum = do
  off <- case IntMap.lookup objNum xref of
    Just (XRefOffset o) -> Right o
    _                   -> Left $ "Content stream object " <> show objNum <> " not at a byte offset"
  (sdict, rawBytes) <- parseStreamAtIndirectLen bs xref dec objNum off
  decompressStream sdict rawBytes
