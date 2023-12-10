{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Tax.FDF where

import Control.Monad (join)
import Data.Biapplicative (biliftA2, biliftA3)
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
import Text.FDF (FDF (FDF, body), Field, foldMapWithKey, foldMapFieldWithKey, mapFieldWithKey, parse, serialize)
import Text.Read (readEither)

data FieldConst a = Field {path :: [Text], entry :: Entry a}
                  | NoField

data Entry a where
  Constant :: (Eq a, Show a) => a -> Entry a -> Entry a
  Count :: Entry Word
  Date :: Entry Day
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

within :: Text -> FieldConst x -> FieldConst x
within root field@Field{path} = field{path = root:path}
within _ NoField = NoField

formKeys :: Rank2.Foldable form => form FieldConst -> [[Text]]
formKeys = flip appEndo [] . Rank2.foldMap addEntry
  where addEntry :: FieldConst a -> Endo [[Text]]
        addEntry NoField = mempty
        addEntry Field{path, entry = Switch yes no leaf} = Endo ([path ++ [yes, leaf], path ++ [no, leaf]] ++)
        addEntry Field{path, entry = Switch' leaf} = Endo ([path ++ [leaf], path ++ [leaf <> "[1]"]] ++)
        addEntry Field{path} = Endo (path :)

mapForm :: (Rank2.Apply form, Rank2.Traversable form)
        => form FieldConst -> (form Maybe -> form Maybe) -> FDF -> Either String FDF
mapForm fields f fdf = store fields fdf . f <$> load fields fdf

mapForm2 :: (Rank2.Apply form1, Rank2.Apply form2, Rank2.Traversable form1, Rank2.Traversable form2)
         => (form1 FieldConst, form2 FieldConst)
         -> ((form1 Maybe, form2 Maybe) -> (form1 Maybe, form2 Maybe))
         -> (FDF, FDF)
         -> Either String (FDF, FDF)
mapForm2 fields f fdfs = biliftA3 store store fields fdfs . f <$> bisequence (biliftA2 load load fields fdfs)

load :: (Rank2.Apply form, Rank2.Traversable form) => form FieldConst -> FDF -> Either String (form Maybe)
load fields = fromFieldMap fields . foldMapWithKey Map.singleton

loadFields :: (Rank2.Apply form, Rank2.Traversable form) => form FieldConst -> Field -> Either String (form Maybe)
loadFields fields = fromFieldMap fields . foldMapFieldWithKey Map.singleton

store :: (Rank2.Apply form, Rank2.Foldable form) => form FieldConst -> FDF -> form Maybe -> FDF
store fields = flip (update fields)

update :: (Rank2.Apply form, Rank2.Foldable form) => form FieldConst -> form Maybe -> FDF -> FDF
update formFields values fdf@FDF{body = fields} = fdf{body = updateFields formFields values fields}

updateFields :: (Rank2.Apply form, Rank2.Foldable form) => form FieldConst -> form Maybe -> Field -> Field
updateFields fields = mapFieldWithKey
                      . updateKey
                      . Rank2.foldMap (foldMap (uncurry Map.singleton) . getConst)
                      . Rank2.liftA2 pairKey fields
  where pairKey :: FieldConst a -> Maybe a -> Const (Maybe ([Text], Text)) a
        pairKey Field {path, entry = RadioButtons start step leaf values} (Just v)
          | Just i <- elemIndex v values
          = Const $ Just (map addIndex path ++ [leaf <> "[" <> Text.pack (show $ start + i*step) <> "]"],
                          Text.pack $ show $ succ i)
          | otherwise = error ("Missing enum value " <> show v)
        pairKey Field {path, entry = Switch yes no leaf} (Just v) =
          Const $ Just (addIndex <$> (path ++ [if v then yes else no, leaf]), if v then "1" else "2")
        pairKey Field {path, entry = Switch' leaf} (Just True) = Const $ Just (addIndex <$> (path ++ [leaf]), "1")
        pairKey Field {path, entry = Switch' leaf} (Just False) =
          Const $ Just (map addIndex path ++ [leaf <> "[1]"], "1")
        pairKey Field {path, entry = Constant c e} (Just v)
          | c == v = Const Nothing
          | otherwise = error ("Trying to replace constant field " ++ show (path, c) ++ " with " ++ show v)
        pairKey Field {path, entry} v = Const $ Just (addIndex <$> path, foldMap (fromEntry entry) v)
        pairKey NoField _ = Const Nothing
        updateKey :: Map [Text] Text -> [Text] -> Text -> Text
        updateKey m k v = Map.findWithDefault v k m
        fromEntry :: Entry a -> a -> Text
        fromEntry (Constant c e) _ = fromEntry e c
        fromEntry Textual v = v
        fromEntry Date v = Text.pack $ formatTime defaultTimeLocale "%Y%m%d" v
        fromEntry Checkbox True = "Yes"
        fromEntry Checkbox False = "No"
        fromEntry e@(RadioButton values) v = case elemIndex v values
                                             of Just i -> Text.pack $ show $ i+1
                                                Nothing -> error (show e <> " doesn't allow value " <> show v)
        fromEntry Amount v = Text.pack (show v)
        fromEntry Percent v = dropInsignificantZeros (Text.pack $ show (fromRational $ v * 100 :: Centi)) <> "%"
          where dropInsignificantZeros = Text.dropWhileEnd (== '.') . Text.dropWhileEnd (== '0')
        fromEntry Count v = Text.pack (show v)
        fromEntry Province v = Text.pack (show v)

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
toEntry Count v = Just <$> readEither v
toEntry Date v = Just <$> parseTimeM False defaultTimeLocale "%Y%m%d" v
toEntry Province v = Just <$> readEither v
toEntry Textual v = Right $ Just $ Text.pack v
toEntry Amount v = Just <$> readEither (dropCommas v)
  where dropCommas num
          | (wholePart, pointyPart@('.' : decimals)) <- span (/= '.') num,
            length decimals == 2,
            all isDigit decimals
          = filter (/= ',') wholePart <> pointyPart
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
