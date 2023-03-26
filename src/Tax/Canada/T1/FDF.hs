{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.T1.FDF where

import Data.Fixed (Centi)
import Data.Functor.Const (Const (Const, getConst))
import Data.Functor.Identity (Identity (Identity))
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, defaultTimeLocale, formatTime, parseTimeM)
import Rank2 qualified
import Text.FDF (FDF, foldMapWithKey, mapWithKey, parse, serialize)
import Text.Read (readEither)
import Tax.Canada.T1.Types
import Tax.Canada.T1.FieldNames (FieldConst (Field, path, entry), Entry (..), t1Fields)

load :: FDF -> Either String (T1 Identity)
load = fromFieldMap . foldMapWithKey Map.singleton

update :: T1 Identity -> FDF -> FDF
update = mapWithKey . updateKey . Rank2.foldMap (uncurry Map.singleton . getConst) . Rank2.liftA2 pairKey t1Fields
  where pairKey :: FieldConst a -> Identity a -> Const ([Text], Text) a
        pairKey Field {path, entry} (Identity v) = Const (path, fromEntry entry v)
        updateKey :: Map [Text] Text -> [Text] -> Text -> Text
        updateKey m k v = Map.findWithDefault v k m
        fromEntry :: Entry a -> a -> Text
        fromEntry Textual v = v
        fromEntry Date v = Text.pack $ formatTime defaultTimeLocale "%Y%m%d" v
        fromEntry Checkbox True = "Yes"
        fromEntry Checkbox False = "No"
        fromEntry RadioButton True = "1"
        fromEntry RadioButton False = "0"
        fromEntry Amount v = Text.pack (show v)
        fromEntry Count v = Text.pack (show v)
        fromEntry Province v = Text.pack (show v)

fromFieldMap :: Map [Text] Text -> Either String (T1 Identity)
fromFieldMap fieldValues = Rank2.traverse fill t1Fields
  where fill :: FieldConst a -> Either String (Identity a)
        fill Field {path, entry}
          | Just v <- Map.lookup path fieldValues = toEntry entry (Text.unpack v)
          | Just v <- Map.lookup ((<> "[0]") <$> path) fieldValues = toEntry entry (Text.unpack v)
          | otherwise = error ("Unknown field path " ++ show path ++ " between "
                               ++ show (Map.lookupLT path fieldValues, Map.lookupGT path fieldValues))
        toEntry :: Entry a -> String -> Either String (Identity a)
        toEntry Count v = Identity <$> readEither v
        toEntry Date v = Identity <$> parseTimeM False defaultTimeLocale "%Y%m%d" v
        toEntry Province v = Identity <$> readEither v
        toEntry Textual v = Right $ Identity $ Text.pack v
        toEntry Amount v = Identity <$> readEither v
        toEntry Checkbox "Yes" = Right $ Identity True
        toEntry Checkbox "No" = Right $ Identity False
        toEntry Checkbox x = Left ("Bad checkbox value: " <> x)
        toEntry RadioButton "1" = Right $ Identity True
        toEntry RadioButton "0" = Right $ Identity False
        toEntry RadioButton x = Left ("Bad radio button value: " <> x)
        toEntry e@(RadioButtons leaf vals) v = error (show (e, v))
        toEntry e@(Switch a b leaf) v = error (show (e, v))
        toEntry e@(Switch' leaf) v = error (show (e, v))

instance MonadFail (Either String) where
  fail = Left
