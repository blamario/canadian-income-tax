{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Utility functions for dealing with 'FDF' forms

module Tax.FDF (FDFs, FieldConst(..), Entry(..),
                mapForm, mapForm2, mapForms, load, loadAll, store, storeAll, update, updateAll, formKeys, within) where

import Control.Monad (join)
import Data.Biapplicative (biliftA2, biliftA3)
import Data.Bifunctor (bimap)
import Data.Bitraversable (bisequence, bitraverse)
import Data.CAProvinceCodes qualified as Province
import Data.Char (isDigit, isSpace)
import Data.Fixed (Centi)
import Data.Foldable (find)
import Data.Functor.Const (Const (Const, getConst))
import Data.List (elemIndex)
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import Data.Semigroup (Endo (Endo, appEndo))
import Data.Semigroup.Cancellative (stripSuffix)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, defaultTimeLocale, formatTime, parseTimeM)
import Data.Void (Void)
import Rank2 qualified
import Text.FDF (FDF (FDF, body), Field, foldMapWithKey, mapWithKey, parse, serialize, traverseWithKey)
import Text.Read (readEither)

-- | A form field path serves to uniquely identify and locate the field inside a form
data FieldConst a = Field {path :: [Text], entry :: Entry a}
                  | NoField

-- | The type of a single form field value
data Entry a where
  Constant :: (Eq a, Show a) => a -> Entry a -> Entry a
  Count :: Entry Word
  Date :: Entry Day
  Year :: Entry Int
  Province :: Entry Province.Code
  Textual :: Entry Text
  Amount :: Entry Centi
  Percent :: Entry Rational
  Checkbox :: Entry Bool
  RadioButton :: (Eq a, Show a) => [a] -> Entry a
  RadioButtons :: (Bounded a, Enum a, Eq a, Show a) => Int -> Int -> Text -> [a] -> Entry a
  Switch :: Text -> Text -> Text -> Entry Bool
  Switch' :: Text -> Entry Bool

deriving instance Show a => Show (Entry a)

-- | A collection of 'FDF' forms keyed by a 'Text' identifier
type FDFs = Map Text FDF

-- | Add a head component to a field path
within :: Text -> FieldConst x -> FieldConst x
within root field@Field{path} = field{path = root:path}
within _ NoField = NoField

-- | The list of all field paths inside a form
formKeys :: Rank2.Foldable form => form FieldConst -> [[Text]]
formKeys = flip appEndo [] . Rank2.foldMap addEntry
  where addEntry :: FieldConst a -> Endo [[Text]]
        addEntry NoField = mempty
        addEntry Field{path, entry = Switch yes no leaf} = Endo ([path ++ [yes, leaf], path ++ [no, leaf]] ++)
        addEntry Field{path, entry = Switch' leaf} = Endo ([path ++ [leaf], path ++ [leaf <> "[1]"]] ++)
        addEntry Field{path} = Endo (path :)

-- | Given a form's field paths and a function that modifies a form with optional field values, try to update an
-- 'FDF' with the form. Fail if any of the field paths can't be found in the form.
mapForm :: (Rank2.Apply form, Rank2.Traversable form)
        => form FieldConst -> (form Maybe -> form Maybe) -> FDF -> Either String FDF
mapForm fields f fdf = load fields fdf >>= store fields fdf . f

-- | Given the field paths of multiple forms with path heads distinguishing among the forms, and a function that
-- modifies a collection of forms with optional field values, try to update 'FDFs' of the forms. Fail if any of the
-- field paths can't be found in the forms.
mapForms :: (Rank2.Apply form, Rank2.Traversable form)
         => form FieldConst -> (form Maybe -> form Maybe) -> FDFs -> Either String FDFs
mapForms fields f fdfs = loadAll fields fdfs >>= storeAll fields fdfs . f

-- | Given two forms' field paths and a function that modifies both forms with optional field values, try to update
-- two 'FDF's with the forms. Fail if any of the field paths can't be found in the forms.
mapForm2 :: (Rank2.Apply form1, Rank2.Apply form2, Rank2.Traversable form1, Rank2.Traversable form2)
         => (form1 FieldConst, form2 FieldConst)
         -> ((form1 Maybe, form2 Maybe) -> (form1 Maybe, form2 Maybe))
         -> (FDF, FDF)
         -> Either String (FDF, FDF)
mapForm2 fields f fdfs = bisequence (biliftA2 load load fields fdfs) >>= bisequence . biliftA3 store store fields fdfs . f

-- | Try to load all 'FDFs' into a form with optional values using the given field paths.
loadAll :: forall form. (Rank2.Apply form, Rank2.Traversable form) => form FieldConst -> FDFs -> Either String (form Maybe)
loadAll fields fdfs = fromPresentFieldMap $ Map.foldMapWithKey (\k-> foldMapWithKey (Map.singleton . (k <> "[0]" :))) fdfs
  where fromPresentFieldMap :: Map [Text] Text -> Either String (form Maybe)
        fillPresent :: Map [Text] Text -> FieldConst a -> Either String (Maybe a)
        fromPresentFieldMap m = Rank2.traverse (fillPresent m) fields
        fillPresent m f@Field {path = root : _}
          | Map.member root fdfs = fill m f
        fillPresent _ _ = Right Nothing

-- | Try to load an 'FDF' into a form with optional values using the given field paths.
load :: (Rank2.Apply form, Rank2.Traversable form) => form FieldConst -> FDF -> Either String (form Maybe)
load fields = fromFieldMap fields . foldMapWithKey Map.singleton

-- | Try to store a form with optional values into the given map of 'FDFs' according to given field paths. The heads
-- of the paths correspond to the map keys. Fail if any of the 'FDF's doesn't contain a field path, but ignore the
-- path heads not present among the keys of 'FDFs'.
storeAll :: (Rank2.Apply form, Rank2.Foldable form) => form FieldConst -> FDFs -> form Maybe -> Either String FDFs
storeAll fields = flip (updateAll fields)

-- | Try to store a form with optional values into the given 'FDF' according to given field paths. Fail if the 'FDF'
-- doesn't contain a field path.
store :: (Rank2.Apply form, Rank2.Foldable form) => form FieldConst -> FDF -> form Maybe -> Either String FDF
store fields = flip (update fields)

-- | Try to update the given map of 'FDFs' from the form with optional values according to given field paths. The
-- heads of the paths correspond to the map keys. Fail if any of the 'FDF's doesn't contain a field path, but ignore
-- the path heads not present among the keys of 'FDFs'.
updateAll :: (Rank2.Apply form, Rank2.Foldable form) => form FieldConst -> form Maybe -> FDFs -> Either String FDFs
updateAll formFields formValues = case toFieldMap formFields formValues of
  Left err -> const (Left err)
  Right m -> Right . Map.mapWithKey (\k-> mapWithKey (updateKeyFrom m . (k <> "[0]" :)))

-- | Try to update the given 'FDF' from the form with optional values according to given form field paths. Fail if
-- the 'FDF' doesn't contain a field path.
update :: (Rank2.Apply form, Rank2.Foldable form) => form FieldConst -> form Maybe -> FDF -> Either String FDF
update formFields formValues = case toFieldMap formFields formValues of
  Left err -> const (Left err)
  Right m -> Right . mapWithKey (updateKeyFrom m)

updateKeyFrom :: Map [Text] Text -> [Text] -> Text -> Text
updateKeyFrom m k v = Map.findWithDefault v k m

toFieldMap :: (Rank2.Apply form, Rank2.Foldable form) => form FieldConst -> form Maybe -> Either String (Map [Text] Text)
toFieldMap fields = sequenceA
                    . Rank2.foldMap (foldMap (uncurry Map.singleton) . getConst)
                    . textualFields fields

textualFields :: (Rank2.Apply form, Rank2.Foldable form)
              => form FieldConst -> form Maybe -> form (Const (Maybe ([Text], Either String Text)))
textualFields = Rank2.liftA2 pairKey
  where pairKey :: FieldConst a -> Maybe a -> Const (Maybe ([Text], Either String Text)) a
        pairKey Field {path, entry = RadioButtons start step leaf values} (Just v)
          | Just i <- elemIndex v values
          = Const $ Just (map addIndex path ++ [leaf <> "[" <> Text.pack (show $ start + i*step) <> "]"],
                          Right $ Text.pack $ show $ succ i)
          | otherwise = Const $ Just (path, Left ("Missing enum value " <> show v))
        pairKey Field {path, entry = Switch yes no leaf} (Just v) =
          Const $ Just (addIndex <$> (path ++ [if v then yes else no, leaf]), Right $ if v then "1" else "2")
        pairKey Field {path, entry = Switch' leaf} (Just True) = Const $ Just (addIndex <$> (path ++ [leaf]), Right "1")
        pairKey Field {path, entry = Switch' leaf} (Just False) =
          Const $ Just (map addIndex path ++ [leaf <> "[1]"], Right "1")
        pairKey Field {path, entry = Constant c e} (Just v)
          | c == v = Const Nothing
          | otherwise = Const $ Just (path, Left ("Trying to replace constant field " ++ show (path, c) ++ " with " ++ show v))
        pairKey Field {path, entry} v = Const $ Just (addIndex <$> path, maybe (Right "") (fromEntry entry) v)
        pairKey NoField _ = Const Nothing
        fromEntry :: Entry a -> a -> Either String Text
        fromEntry (Constant c e) _ = fromEntry e c
        fromEntry Textual v = Right v
        fromEntry Date v = Right $ Text.pack $ formatTime defaultTimeLocale "%Y%m%d" v
        fromEntry Checkbox True = Right "Yes"
        fromEntry Checkbox False = Right "No"
        fromEntry e@(RadioButton values) v = case elemIndex v values
                                             of Just i -> Right $ Text.pack $ show $ i+1
                                                Nothing -> Left (show e <> " doesn't allow value " <> show v)
        fromEntry Amount v = Right $ Text.pack (show v)
        fromEntry Percent v = Right $ dropInsignificantZeros (Text.pack $ show (fromRational $ v * 100 :: Centi)) <> "%"
          where dropInsignificantZeros = Text.dropWhileEnd (== '.') . Text.dropWhileEnd (== '0')
        fromEntry Count v = Right $ Text.pack (show v)
        fromEntry Year v = Right $ Text.pack (show v)
        fromEntry Province v = Right $ Text.pack (show v)

fromFieldMap :: Rank2.Traversable form => form FieldConst -> Map [Text] Text -> Either String (form Maybe)
fromFieldMap fieldForm fieldMap = Rank2.traverse (fill fieldMap) fieldForm

fill :: forall a. Map [Text] Text -> FieldConst a -> Either String (Maybe a)
fill fieldValues NoField = Right Nothing
fill fieldValues Field {path, entry}
  | Just v <- Map.lookup path fieldValues = toEntry entry (Text.unpack v)
  | Just v <- Map.lookup (addIndex <$> path) fieldValues = toEntry entry (Text.unpack v)
  | RadioButtons start step leaf values <- entry,
    alts <- [ (Map.lookup (map addIndex path <> [leaf <> "[" <> Text.pack (show $ start + i*step) <> "]"]) fieldValues,
               v)
            | (i, v) <- zip [0 ..] values ]
  = if null alts then Left ("No radio buttons on path " <> show path)
    else Right $ snd <$> find (any (`notElem` ["", "Off"]) . fst) (alts :: [(Maybe Text, a)])
  | Switch yes no leaf <- entry,
    Just yesValue <- Map.lookup (addIndex <$> (path <> [yes, leaf])) fieldValues,
    Just noValue <- Map.lookup (addIndex <$> (path <> [no, leaf])) fieldValues
  = if yesValue `elem` ["", "Off"] && noValue `elem` ["", "Off"] then Right Nothing
    else if yesValue == "1" && noValue `elem` ["", "Off"] then Right (Just True)
    else if yesValue `elem` ["", "Off"] && noValue `elem` ["1", "2"] then Right (Just False)
    else Left ("Can't figure out the checkbox at " <> show (path, entry, yesValue, noValue))
  | Switch' leaf <- entry,
    Just yesValue <- Map.lookup (map addIndex path <> [leaf <> "[0]"]) fieldValues,
    Just noValue <- Map.lookup (map addIndex path <> [leaf <> "[1]"]) fieldValues
  = if yesValue `elem` ["", "Off"] && noValue `elem` ["", "Off"] then Right Nothing
    else if yesValue == "1" && noValue `elem` ["", "Off"] then Right (Just True)
    else if yesValue `elem` ["", "Off"] && noValue `elem` ["1", "2"] then Right (Just False)
    else Left ("Can't figure out the checkbox at " <> show (path, entry, yesValue, noValue))
  | otherwise = Left ("Unknown field path " ++ show path ++ " between "
                       ++ show (Map.lookupLT (addIndex <$> path) fieldValues,
                                Map.lookupGT (addIndex <$> path) fieldValues))

toEntry :: Entry a -> String -> Either String (Maybe a)
toEntry _ "" = Right Nothing
toEntry (Constant expected entry) v = do
  e <- toEntry entry v
  if e == Just expected
    then pure e
    else Left ("Expected " <> show expected <> ", got " <> show (v, e))
toEntry Count v = bimap (<> " for the count of " <> show v) Just $ readEither v
toEntry Year v = bimap (<> " for year " <> show v) Just $ readEither v
toEntry Date v =  bimap (<> " for date " <> show v) Just $ parseTimeM False defaultTimeLocale "%Y%m%d" v
toEntry Province v = bimap (<> " for province code " <> show v) Just $ readEither v
toEntry Textual v = Right $ Just $ Text.pack v
toEntry Amount v = bimap (<> " for the amount of " <> show v) Just $ readEither (dropCommas v)
  where dropCommas num
          | (wholePart, pointyPart@('.' : decimals)) <- span (/= '.') num,
            length decimals == 2,
            all isDigit decimals
          = filter (/= ',') wholePart <> pointyPart
          | (suffix, ',':_) <- span (/= ',') (reverse num), length suffix > 2 = filter (/= ',') num
          | otherwise = num
toEntry Percent v
  | Just v' <- stripSuffix "%" v,
    (wholePart, pointyPart) <- span (/= '.') v',
    Right whole <- fromInteger <$> readEither wholePart,
    Right decimal <- case takeWhile (not . isSpace) pointyPart
                     of '.' : decimals -> (/ 10 ^ length decimals) . fromInteger <$> readEither decimals
                        "" -> Right 0
                        _ -> Left ("Bad decimals: " <> show v')
  = Right $ Just ((whole + decimal) / 100)
  | otherwise = Left ("Bad percentage value: " <> show v)
toEntry Checkbox "Yes" = Right $ Just True
toEntry Checkbox "No" = Right $ Just False
toEntry Checkbox "Off" = Right $ Just False
toEntry Checkbox "1" = Right $ Just True
toEntry Checkbox v = Left ("Bad checkbox value: " <> show v)
toEntry e@(RadioButton values) v
  | Right n <- readEither v, n > 0, x:_ <- drop (n - 1) values = Right $ Just x
  | otherwise = Left ("Bad radio button value: " <> show (e, v))
toEntry e@RadioButtons{} v = Left (show (e, v))
toEntry e@(Switch a b leaf) v = Left (show (e, v))
toEntry e@(Switch' leaf) v = Left (show (e, v))

addIndex :: Text -> Text
addIndex key
  | "]" `Text.isSuffixOf` key = key
  | otherwise = key <> "[0]"

instance MonadFail (Either String) where
  fail = Left
