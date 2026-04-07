{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Parse and serialize between FDF files and `Map [Text] Text`.

module Text.FDF (FDF (FDF, body), Field (Field, name, content), FieldContent (FieldValue, FieldNameValue, Children),
                 insert, delete, update,
                 mapWithKey, mapFieldWithKey,
                 foldMapWithKey, foldMapFieldWithKey,
                 traverseWithKey, traverseFieldWithKey,
                 parse, serialize) where

import Control.Applicative ((<*), (<*>), (<|>), many, some, optional)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Char (chr, digitToInt, isAscii, isAlphaNum, isSpace, ord)
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import Data.Monoid.Instances.ByteString.UTF8 (ByteStringUTF8 (ByteStringUTF8))
import Data.Monoid.Textual (singleton, toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf16BE, encodeUtf8, encodeUtf16BE)
import Numeric (showOct)
import Rank2 qualified
import Text.Grampa
import Text.Grampa.Combinators
import Text.Parser.Char (octDigit)
import Text.Parser.Combinators (manyTill)
import Text.Grampa.PEG.Backtrack qualified as PEG

type Parser = PEG.Parser (Rank2.Only FDF)

-- | Parsed FDF data structure
data FDF = FDF {
  header :: ByteString,
  body :: Field,
  trailer :: ByteString}
  deriving (Eq, Show)

-- | The body of FDF is a tree of nestable 'Field's.
data Field = Field {
    name :: Text,
    content :: FieldContent}
  deriving (Eq, Ord, Show)

data FieldContent
  = FieldValue Text
  | FieldNameValue Text    -- ^ a PDF name value, serialized as @\/V \/name@
  | Children [Field]
  deriving (Show, Eq, Ord)

-- | Insert a value at a new path into the FDF
insert :: NonEmpty Text -> Text -> FDF -> FDF
insert key value x@FDF{body} = x{body = insertField key value body}

-- | Delete an existing field at the given path from the FDF
delete :: NonEmpty Text -> Text -> FDF -> FDF
delete key old x@FDF{body} = x{body = deleteField key old body}

-- | Update a value at a new path into the FDF
update :: NonEmpty Text -> Text -> Text -> FDF -> FDF
update key old new x@FDF{body} = x{body = updateField key old new body}

insertField :: NonEmpty Text -> Text -> Field -> Field
insertField (root :| path) new x@Field{name, content}
  | root /= name = error ("Insertion name mismatch: " <> show root <> "/=" <> show name)
  | otherwise = case nonEmpty path of
      Nothing
        | Just old <- leafValue content -> error ("Insertion would overwrite value: " <> show old <> "->" <> show new)
        | otherwise -> error ("Insertion would prune " <> show root)
      Just path' -> case content of
        FieldValue _     -> error ("Insertion ran out " <> show (root : path))
        FieldNameValue _ -> error ("Insertion ran out " <> show (root : path))
        Children kids -> x{content= Children $ insertAmong path' new kids}

deleteField :: NonEmpty Text -> Text -> Field -> Field
deleteField (root :| path) old x@Field{name, content}
  | root /= name = error ("Deletion name mismatch: " <> show root <> "/=" <> show name)
  | otherwise = case nonEmpty path of
      Nothing
        | Children{} <- content -> error ("Deletion would prune " <> show root)
        | Just v <- leafValue content, v == old -> x  -- will be removed by deleteAmong
        | otherwise -> error ("Expected to delete " <> show old <> ", instead found " <> show content)
      Just path'
        | Children kids <- content -> x{content= Children $ deleteAmong path' old kids}
        | otherwise -> error ("Deletion ran out " <> show root)

updateField :: NonEmpty Text -> Text -> Text -> Field -> Field
updateField (root :| path) old new x@Field{name, content}
  | root /= name = error ("Update name mismatch: " <> show root <> "/=" <> show name)
  | otherwise = case nonEmpty path of
      Nothing
        | Just v <- leafValue content, v /= old -> error ("Expected to update " <> show old <> ", instead found " <> show content)
        | Just setter <- leafValueSetter content -> x{content= setter new}
        | otherwise -> error ("Expected to update " <> show old <> ", instead found " <> show content)
      Just path'
        | Children kids <- content -> x{content= Children $ updateAmong path' old new kids}
        | otherwise -> error ("Update ran out " <> show root)

-- | Extract the text value from a leaf 'FieldContent', if any.
leafValue :: FieldContent -> Maybe Text
leafValue (FieldValue v)     = Just v
leafValue (FieldNameValue v) = Just v
leafValue (Children _)       = Nothing

-- | Return a setter for the text value of a leaf 'FieldContent' that tries to preserve
-- its construtor, or 'Nothing' for 'Children' nodes.
leafValueSetter :: FieldContent -> Maybe (Text -> FieldContent)
leafValueSetter (FieldValue _)     = Just FieldValue
leafValueSetter (FieldNameValue _) = Just toFieldContent
leafValueSetter (Children _)       = Nothing

insertAmong :: NonEmpty Text -> Text -> [Field] -> [Field]
insertAmong path@(root :| _) new (x@Field{name} : xs)
  | root == name = insertField path new x : xs
  | otherwise = x : insertAmong path new xs
insertAmong (root :| path) new [] = case nonEmpty path of
  Nothing -> [Field{name=root, content = FieldValue new}]
  Just path' ->[Field{name=root, content = Children $ insertAmong path' new []}]

deleteAmong :: NonEmpty Text -> Text -> [Field] -> [Field]
deleteAmong path@(root :| rest) old (x@Field{name, content} : xs)
  | root /= name = x : deleteAmong path old xs
  | Just path' <- nonEmpty rest = deleteField path' old x : xs
  | Just v <- leafValue content, v == old = xs
  | otherwise = error ("Expected to delete " <> show old <> ", instead found " <> show content)
deleteAmong path _ [] = error ("Can't find the path to delete, " <> show path)

updateAmong :: NonEmpty Text -> Text -> Text -> [Field] -> [Field]
updateAmong path@(root :| _) old new (x@Field{name} : xs)
  | root == name = updateField path old new x : xs
  | otherwise = x : updateAmong path old new xs
updateAmong path _ _ [] = error ("Can't find the path to update, " <> show path)

mapWithKey :: ([Text] -> Text -> Text) -> FDF -> FDF
mapWithKey f x@FDF{body} = x{body = mapFieldWithKey f body}

mapFieldWithKey :: ([Text] -> Text -> Text) -> Field -> Field
mapFieldWithKey f x@Field{name, content=FieldValue v} = x{content = FieldValue $ f [name] v}
mapFieldWithKey f x@Field{name, content=FieldNameValue v} = x{content = toFieldContent $ f [name] v}
mapFieldWithKey f x@Field{name, content=Children kids} = x{content = Children $ mapFieldWithKey (f . (name:)) <$> kids}

foldMapWithKey :: Monoid a => ([Text] -> Text -> a) -> FDF -> a
foldMapWithKey f FDF{body} = foldMapFieldWithKey f body

foldMapFieldWithKey :: Monoid a => ([Text] -> Text -> a) -> Field -> a
foldMapFieldWithKey f Field{name, content = FieldValue v} = f [name] v
foldMapFieldWithKey f Field{name, content = FieldNameValue v} = f [name] v
foldMapFieldWithKey f Field{name, content = Children kids} = foldMap (foldMapFieldWithKey $ f . (name:)) kids

traverseWithKey :: Applicative f => ([Text] -> Text -> f Text) -> FDF -> f FDF
traverseWithKey f x@FDF{body} = (\body'-> x{body = body'}) <$> traverseFieldWithKey f body

traverseFieldWithKey :: Applicative f => ([Text] -> Text -> f Text) -> Field -> f Field
traverseFieldWithKey f Field{name, content = FieldValue v} = Field name . FieldValue <$> f [name] v
traverseFieldWithKey f Field{name, content = FieldNameValue v} = Field name . toFieldContent <$> f [name] v
traverseFieldWithKey f Field{name, content = Children kids} =
  Field name . Children <$> traverse (traverseFieldWithKey $ f . (name:)) kids

serialize :: FDF -> ByteString
serialize FDF{header, body, trailer} =
  "%FDF-1.2\n"
  <> header
  <> "<<\n"
  <> "/FDF\n"
  <> "<<\n"
  <> "/Fields [\n"
  <> serializeField body <> "\n"
  <> "]\n"
  <> ">>\n"
  <> ">>\n"
  <> trailer
  <> "%%EOF\n"

serializeField :: Field -> ByteString
serializeField Field{name, content = FieldValue v} =
  "<<\n"
  <> "/T (" <> encodeUtf8 name <> ")\n"
  <> "/V (" <> serializeValue v <> ")\n"
  <> ">>"
serializeField Field{name, content = FieldNameValue v} =
  "<<\n"
  <> "/T (" <> encodeUtf8 name <> ")\n"
  <> "/V " <> (if isNameValid v then "/" <> encodeUtf8 v else "(" <> serializeValue v <> ")") <> "\n"
  <> ">>"
serializeField Field{name, content = Children kids} =
  "<<\n"
  <> "/T (" <> encodeUtf8 name <> ")\n"
  <> (if null kids then "" else "/Kids [\n" <> ByteString.intercalate "\n" (serializeField <$> kids) <> "]\n")
  <> ">>"

serializeValue :: Text -> ByteString
serializeValue t
  | Text.isAscii t = encodeUtf8 (plain <> escaped)
  | otherwise = escapeRawBytes (utf16beBOM <> encodeUtf16BE t)
  where (plain, special) = Text.span (\c -> c >= ' ' && c `notElem` ['(', ')', '\\']) t
        escaped = Text.concatMap escape special
        escape '(' = Text.pack "\\("
        escape ')' = "\\)"
        escape '\\' = "\\\\"
        escape '\n' = "\\n"
        escape '\r' = "\\r"
        escape '\t' = "\\t"
        escape '\b' = "\\b"
        escape c
          | c < ' ' = "\\" <> Text.justifyRight 3 '0' (Text.pack $ showOct (ord c) "")
          | otherwise = Text.singleton c

-- | Escape raw bytes that would break a PDF literal string @(...)@.
-- Used for the UTF-16BE branch of 'serializeValue' where the byte stream
-- may contain @(@, @)@, or @\\@ as part of multi-byte code units.
--
-- The escaping is at the byte level, matching the PDF spec (§7.3.4.2):
-- readers un-escape first, then interpret the resulting bytes as UTF-16BE.
-- Since all escaped bytes are in the ASCII range (< 0x80), they can never
-- appear as UTF-8 continuation bytes and are always single-byte
-- 'ByteStringUTF8' factors, so the parser correctly un-escapes them.
escapeRawBytes :: ByteString -> ByteString
escapeRawBytes = ByteString.concatMap $ \b -> case b of
  0x0A -> "\\n"
  0x0D -> "\\r"
  0x28 -> "\\("
  0x29 -> "\\)"
  0x5C -> "\\\\"
  _    -> ByteString.singleton b

toFieldContent :: Text -> FieldContent
toFieldContent v = if isNameValid v then FieldNameValue v else FieldValue v

isNameValid :: Text -> Bool
isNameValid v = Text.all (\c-> isAscii c && isAlphaNum c) v

parse :: ByteString -> Either String FDF
parse input =
  bimap (\failure-> toString (const "<?>") $ failureDescription s failure 4) id $ simply parseComplete parser s
  where s = ByteStringUTF8 input

parser :: Parser ByteStringUTF8 FDF
parser = FDF
  <$ (string "%FDF-1.2" <* lineEnd <?> "first line")
  <*> extract ((takeWhile1 (`notElem` ["\r", "\n"]) <?> "bytes")
               <> lineEnd <> (mconcat <$> manyTill line begin) <?> "header")
  <* (string "/FDF" <* takeCharsWhile (== ' ') <* lineEnd <?> "end header")
  <* begin
  <* (string "/Fields [" <* takeCharsWhile (== ' ') <* lineEnd <?> "fields")
  <*> field
  <* (string "]" <* takeCharsWhile (== ' ') <* lineEnd <?> "end the fields")
  <* (end <?> "end the body")
  <*> extract ((end <?> "end the object")
               <> string "endobj" <> lineEnd
               <> takeCharsWhile isSpace
               <> string "trailer" <> lineEnd
               <> (mconcat <$> manyTill line (string "%%EOF" <?> "last line"))
               <?> "trailer")
  <* optional lineEnd

field :: Parser ByteStringUTF8 Field
field = Field <$ begin
  <*> strictText (string "/T (" *> takeCharsWhile (`notElem` [')', '\r', '\n']) <* string ")" <* lineEnd <?> "name")
  <*> (FieldValue <$> fieldStringValue <|> FieldNameValue <$> fieldNameValue <|> Children <$> children)
  <* end
  where
    fieldStringValue = strictText $
                 admit (string "/V ("
                        *> commit ((string (ByteStringUTF8 utf16beBOM)
                                     *> (utf8from16 <$> concatMany (takeWhile1 (\b -> b `notElem` [")", "\\", "\n", "\r"]) <|> escape))
                                    <|> concatMany (takeCharsWhile1 (`notElem` [')', '\r', '\n', '\\']) <|> escape))
                                   <* string ")" <* lineEnd)
                        <?> "string value")
    fieldNameValue = strictText $
                 admit (string "/V /" *> commit (takeCharsWhile (`notElem` ['\r', '\n']) <* lineEnd)
                        <?> "name value")
    children = admit (string "/Kids [" *> commit (lineEnd *> takeSome field <* string "]" <* lineEnd <?> "kids")
                      <|> commit mempty)
    escape = char '\\'
             *> (singleton <$> (char 'n' *> pure '\n'
                                <|> char 'r' *> pure '\r'
                                <|> char 't' *> pure '\t'
                                <|> char 'b' *> pure '\b'
                                <|> char 'f' *> pure '\f'
                                <|> char '(' *> pure '('
                                <|> char ')' *> pure ')'
                                <|> char '\\' *> pure '\\'
                                <|> chr . sum <$> sequenceA [(64 *) <$> octalDigit, (8 *) <$> octalDigit, octalDigit]))
    octalDigit = digitToInt <$> octDigit
    utf8from16 (ByteStringUTF8 bs) = ByteStringUTF8 (encodeUtf8 $ decodeUtf16BE bs)

begin :: Parser ByteStringUTF8 ByteStringUTF8
begin = string "<<" *> lineEnd <?> "<<"

end :: Parser ByteStringUTF8 ByteStringUTF8
end = string ">>" *> takeCharsWhile (== ' ') *> moptional lineEnd *> pure mempty <?> ">>"

line :: Parser ByteStringUTF8 ByteStringUTF8
line = takeCharsWhile (`notElem` ['\r', '\n']) <> lineEnd <?> "line"

lineEnd :: Parser ByteStringUTF8 ByteStringUTF8
lineEnd = string "\r\n" <|> string "\r" <|> string "\n"

strictText :: Parser ByteStringUTF8 ByteStringUTF8 -> Parser ByteStringUTF8 Text
strictText = fmap $ toText (error . ("Invalid UTF-8 sequence: " ++) . show)

extract :: Parser ByteStringUTF8 ByteStringUTF8 -> Parser ByteStringUTF8 ByteString
extract = fmap $ \(ByteStringUTF8 bs) -> bs

utf16beBOM :: ByteString
utf16beBOM = "\xFE\xFF"
