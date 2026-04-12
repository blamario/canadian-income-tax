-- | Outputs the difference of two input FDF files in the following format:
--
-- output = (line "\n")*
-- line = "< " path "=" value
--      | "> " path "=" value
--      | "! " path ": " value "->" value
-- path = name ("/" name)*
-- name = <any printable character except "/" and "=">*
-- value = <any printable character except ">">*

{-# LANGUAGE Haskell2010, ImportQualifiedPost, OverloadedRecordDot, OverloadedStrings, NoFieldSelectors  #-}

module Main (main) where

import Control.Applicative ((<|>), some)
import Control.Monad (when)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Options.Applicative qualified as OptsAp

import Text.FDF (FDF)
import Text.FDF qualified as FDF
import Common (Difference(Deletion, Addition, Change), readFDF)


data Options = Options {
  ignore :: [Text],
  oldPaths :: Bool,
  old :: FilePath,
  new :: FilePath,
  verbose :: Bool}

optionsParser :: OptsAp.Parser Options
optionsParser =
  (
    Options
    <$> some (OptsAp.strOption (OptsAp.short 'i' <> OptsAp.long "ignore" <> OptsAp.metavar "<name to ignore>"
                                <> OptsAp.help "ignore the named difference in the field path"))
    <*> (OptsAp.flag' True (OptsAp.long "old" <> OptsAp.help "emit field paths from the old FDF")
         <|> OptsAp.flag' False (OptsAp.long "new" <> OptsAp.help "emit field paths from the new FDF"))
    <|>
    pure (Options [] True)
  )
  <*> OptsAp.strArgument (OptsAp.metavar "<old form file input>")
  <*> OptsAp.strArgument (OptsAp.metavar "<new form file input>")
  <*> OptsAp.switch (OptsAp.short 'v' <> OptsAp.long "verbose" <> OptsAp.help "also diff fields with empty values")

diffLine :: ([Text], Difference) -> Text
diffLine (path, Deletion value) = "< " <> Text.intercalate "/" path <> "=" <> value
diffLine (path, Addition value) = "> " <> Text.intercalate "/" path <> "=" <> value
diffLine (path, Change old new) = "! " <> Text.intercalate "/" path <> ": " <> old <> "->" <> new

diff :: Bool -> (Text -> Bool) -> [Text] -> FDF.Field -> FDF.Field -> [([Text], Difference)]
diff oldPaths ignorable ancestry old new
  | old.name /= new.name =
    map (Deletion <$>) (list (ancestry ++) old) ++
    map (Addition <$>) (list (ancestry ++) new)
  | otherwise = diffContents oldPaths ignorable (ancestry ++ [old.name]) old.content new.content

diffContents :: Bool -> (Text -> Bool) -> [Text] -> FDF.FieldContent -> FDF.FieldContent -> [([Text], Difference)]
diffContents oldPaths ignorable ancestry (FDF.FieldNameValue old) new =
  diffContents oldPaths ignorable ancestry (FDF.FieldValue old) new
diffContents oldPaths ignorable ancestry old (FDF.FieldNameValue new) =
  diffContents oldPaths ignorable ancestry old (FDF.FieldValue new)
diffContents oldPaths ignorable ancestry (FDF.FieldValue old) (FDF.FieldValue new)
  | old == new = []
  | otherwise = [(ancestry, Change old new)]
diffContents oldPaths ignorable ancestry (FDF.Children old) (FDF.Children new) =
  diffAll oldPaths ignorable ancestry old new
diffContents oldPaths ignorable ancestry old new =
  map (Deletion <$>) (listContent (ancestry ++) old) ++
  map (Addition <$>) (listContent (ancestry ++) new)

diffAll, diffSorted :: Bool -> (Text -> Bool) -> [Text] -> [FDF.Field] -> [FDF.Field] -> [([Text], Difference)]

diffAll oldPaths ignorable ancestry old new = diffSorted oldPaths ignorable ancestry (List.sort old) (List.sort new)

diffSorted oldPaths ignorable ancestry (old : olds) (new : news)
  | ignorable old.name, FDF.Children kids <- old.content
  = diffSorted oldPaths ignorable
      (if oldPaths then ancestry ++ [old.name] else ancestry)
      (List.sort $ kids ++ olds)
      (new : news)
  | ignorable new.name, FDF.Children kids <- new.content
  = diffSorted oldPaths ignorable
      (if oldPaths then ancestry else ancestry ++ [new.name])
      (old : olds)
      (List.sort $ kids ++ news)
  | old.name < new.name = map (Deletion <$>) (list (ancestry ++) old) ++ diffSorted oldPaths ignorable ancestry olds (new : news)
  | old.name > new.name = map (Addition <$>) (list (ancestry ++) new) ++ diffSorted oldPaths ignorable ancestry (old : olds) news
  | otherwise = diff oldPaths ignorable ancestry old new <> diffSorted oldPaths ignorable ancestry olds news
diffSorted _ _ ancestry olds [] = foldMap (map (Deletion <$>) . list (ancestry ++)) olds
diffSorted _ _ ancestry [] news = foldMap (map (Addition <$>) . list (ancestry ++)) news

list :: ([Text] -> [Text]) -> FDF.Field -> [([Text], Text)]
list addAncestry x = listContent (addAncestry . (x.name :)) x.content

listContent :: ([Text] -> [Text]) -> FDF.FieldContent -> [([Text], Text)]
listContent addAncestry (FDF.FieldValue v) = [(addAncestry [], v)]
listContent addAncestry (FDF.FieldNameValue v) = [(addAncestry [], v)]
listContent addAncestry (FDF.Children kids) = foldMap (list addAncestry) (List.sort kids)

hasNonemptyValue :: Difference -> Bool
hasNonemptyValue (Deletion v) = not (Text.null v)
hasNonemptyValue (Addition v) = not (Text.null v)
hasNonemptyValue (Change v1 v2) = v1 /= v2

process :: Options -> IO ()
process options = do
  when (options.old == "-" && options.new == "-") $ error "Only one input can be '-' stdin"
  old <- readFDF options.old
  new <- readFDF options.new
  let filterEmpty = if options.verbose then id else filter (hasNonemptyValue . snd)
      ignorable name = maybe (name `elem` options.ignore) (`elem` options.ignore) (Text.stripSuffix "[0]" name)
  traverse_ (Text.IO.putStrLn . diffLine) (filterEmpty $ diff options.oldPaths ignorable [] old.body new.body)

main :: IO ()
main =
  OptsAp.execParser (OptsAp.info optionsParser
                     $ OptsAp.progDesc "Output the difference between two input FDF files")
  >>= process
