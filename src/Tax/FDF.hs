{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Tax.FDF where

import Control.Monad (join)
import Data.Char (isDigit)
import Data.CAProvinceCodes qualified as Province
import Data.Fixed (Centi)
import Data.Foldable (find)
import Data.Functor.Const (Const (Const, getConst))
import Data.List (elemIndex)
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import Data.Semigroup.Cancellative (stripSuffix)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, defaultTimeLocale, formatTime, parseTimeM)
import Rank2 qualified
import Text.FDF (FDF, foldMapWithKey, mapWithKey, parse, serialize)
import Text.Read (readEither)

data FieldConst a = Field {path :: [Text], entry :: Entry a}
                  | NoField

data Entry a where
  Count :: Entry Word
  Date :: Entry Day
  Province :: Entry Province.Code
  Textual :: Entry Text
  Amount :: Entry Centi
  Percent :: Entry Rational
  Checkbox :: Entry Bool
  RadioButton :: (Bounded a, Enum a, Eq a, Show a) => [a] -> Entry a
  RadioButtons :: (Bounded a, Enum a, Eq a, Show a) => Text -> [a] -> Entry a
  Switch :: Text -> Text -> Text -> Entry Bool
  Switch' :: Text -> Entry Bool

deriving instance Show a => Show (Entry a)

within :: Text -> FieldConst x -> FieldConst x
within root field@Field{path} = field{path = root:path}
within _ NoField = NoField

load :: (Rank2.Apply form, Rank2.Traversable form) => form FieldConst -> FDF -> Either String (form Maybe)
load fields = fromFieldMap fields . foldMapWithKey Map.singleton

update :: (Rank2.Apply form, Rank2.Foldable form) => form FieldConst -> form Maybe -> FDF -> FDF
update fields = mapWithKey
                . updateKey
                . Rank2.foldMap (foldMap (uncurry Map.singleton) . getConst)
                . Rank2.liftA2 pairKey fields
  where pairKey :: FieldConst a -> Maybe a -> Const (Maybe ([Text], Text)) a
        pairKey Field {path, entry = RadioButtons leaf values} (Just v)
          | Just i <- elemIndex v values
          = Const $ Just (map (<> "[0]") path ++ [leaf <> "[" <> Text.pack (show i) <> "]"], "1")
          | otherwise = error ("Missing enum value " <> show v)
        pairKey Field {path, entry = Switch yes no leaf} (Just v) =
          Const $ Just ((<> "[0]") <$> (path ++ [if v then yes else no, leaf]), "1")
        pairKey Field {path, entry = Switch' leaf} (Just True) = Const $ Just ((<> "[0]") <$> (path ++ [leaf]), "1")
        pairKey Field {path, entry = Switch' leaf} (Just False) =
          Const $ Just (map (<> "[0]") path ++ [leaf <> "[1]"], "1")
        pairKey Field {path, entry} v = Const $ Just ((<> "[0]") <$> path, foldMap (fromEntry entry) v)
        pairKey NoField _ = Const Nothing
        updateKey :: Map [Text] Text -> [Text] -> Text -> Text
        updateKey m k v = Map.findWithDefault v k m
        fromEntry :: Entry a -> a -> Text
        fromEntry Textual v = v
        fromEntry Date v = Text.pack $ formatTime defaultTimeLocale "%Y%m%d" v
        fromEntry Checkbox True = "Yes"
        fromEntry Checkbox False = "No"
        fromEntry (RadioButton values) v = Text.pack $ show $ fromEnum v + 1
        fromEntry Amount v = Text.pack (show v)
        fromEntry Percent v = Text.pack (show (fromRational $ v * 100 :: Centi)) <> "%"
        fromEntry Count v = Text.pack (show v)
        fromEntry Province v = Text.pack (show v)

fromFieldMap :: Rank2.Traversable form => form FieldConst -> Map [Text] Text -> Either String (form Maybe)
fromFieldMap fieldForm fieldMap = Rank2.traverse (fill fieldMap) fieldForm

fill :: forall a. Map [Text] Text -> FieldConst a -> Either String (Maybe a)
fill fieldValues NoField = Right Nothing
fill fieldValues Field {path, entry}
  | Just v <- Map.lookup path fieldValues = toEntry entry (Text.unpack v)
  | Just v <- Map.lookup ((<> "[0]") <$> path) fieldValues = toEntry entry (Text.unpack v)
  | RadioButtons leaf values <- entry,
    alts <- [ (Map.lookup (((<> "[0]") <$> path) <> [leaf <> "[" <> Text.pack (show i) <> "]"]) fieldValues, v)
            | (i, v) <- zip [0 ..] values ]
  = if null alts then error ("No radio buttons on path " <> show path)
    else Right $ snd <$> find (any (`notElem` ["", "Off"]) . fst) (alts :: [(Maybe Text, a)])
  | Switch yes no leaf <- entry,
    Just yesValue <- Map.lookup ((<> "[0]") <$> (path <> [yes, leaf])) fieldValues,
    Just noValue <- Map.lookup ((<> "[0]") <$> (path <> [no, leaf])) fieldValues
  = if yesValue `elem` ["", "Off"] && noValue `elem` ["", "Off"] then Right Nothing
    else if yesValue == "1" && noValue `elem` ["", "Off"] then Right (Just True)
    else if yesValue `elem` ["", "Off"] && noValue `elem` ["1", "2"] then Right (Just False)
    else error ("Can't figure out the checkbox at " <> show (path, entry, yesValue, noValue))
  | Switch' leaf <- entry,
    Just yesValue <- Map.lookup (map (<> "[0]") path <> [leaf <> "[0]"]) fieldValues,
    Just noValue <- Map.lookup (map (<> "[0]") path <> [leaf <> "[1]"]) fieldValues
  = if yesValue `elem` ["", "Off"] && noValue `elem` ["", "Off"] then Right Nothing
    else if yesValue == "1" && noValue `elem` ["", "Off"] then Right (Just True)
    else if yesValue `elem` ["", "Off"] && noValue `elem` ["1", "2"] then Right (Just False)
    else error ("Can't figure out the checkbox at " <> show (path, entry, yesValue, noValue))
  | otherwise = error ("Unknown field path " ++ show path ++ " between "
                       ++ show (Map.lookupLT ((<> "[0]") <$> path) fieldValues,
                                Map.lookupGT ((<> "[0]") <$> path) fieldValues))

toEntry :: Entry a -> String -> Either String (Maybe a)
toEntry _ "" = Right Nothing
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
    Right decimal <- case pointyPart
                     of '.' : decimals -> (/ 10 ^ length decimals) . fromInteger <$> readEither decimals
                        "" -> Right 0
                        _ -> Left "bad decimals"
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
toEntry e@RadioButtons{} v = error (show (e, v))
toEntry e@(Switch a b leaf) v = error (show (e, v))
toEntry e@(Switch' leaf) v = error (show (e, v))

instance MonadFail (Either String) where
  fail = Left
