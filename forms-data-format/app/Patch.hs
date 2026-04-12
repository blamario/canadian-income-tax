{-# LANGUAGE Haskell2010, ImportQualifiedPost, OverloadedRecordDot, OverloadedStrings, NoFieldSelectors  #-}

-- | Patches an input FDF file guided by the specified difference file in the following format:
--
-- output = (line "\n")*
-- line = "< " path ("=" value)?
--      | "> " path ("=" value)?
--      | "! " path ": " value "->" value
-- path = name ("/" name)*
-- name = <any printable character except "/" and "=">*
-- value = <any printable character except ">">*

module Main (main) where

import Data.Bifunctor (first)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Semigroup (Endo(..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Options.Applicative qualified as OptsAp

import Text.FDF (FDF)
import Text.FDF qualified as FDF
import Common (Difference(Deletion, Addition, Change), readFDF)


data Options = Options {
  diff :: FilePath,
  input :: FilePath}

optionsParser :: OptsAp.Parser Options
optionsParser =
  Options
  <$> OptsAp.strArgument (OptsAp.metavar "<diff file input>")
  <*> OptsAp.strArgument (OptsAp.metavar "<form file input>")

parseLine :: Text -> ([Text], Difference)
parseLine line = case Text.splitAt 2 line of
  ("< ", rest)
    | let (path, eqValue) = Text.break (== '=') rest,
      Just value <- Text.stripPrefix "=" eqValue
    -> (Text.split (== '/') path, Deletion value)
  ("> ", rest)
    | let (path, eqValue) = Text.break (== '=') rest,
      Just value <- Text.stripPrefix "=" eqValue
    -> (Text.split (== '/') path, Addition value)
  ("! ", rest)
    | let (path, colonValues) = Text.breakOn ": " rest,
      Just values <- Text.stripPrefix ": " colonValues,
      let (old, arrowNew) = Text.breakOn "->" values,
      Just new <- Text.stripPrefix "->" arrowNew
      -> (Text.split (== '/') path, Change old new)
  _ -> error ("Invalid diff syntax:\n" <> Text.unpack line)

patchFrom :: (NonEmpty Text, Difference) -> FDF -> FDF
patchFrom (path, Deletion v) = FDF.delete path v
patchFrom (path, Addition v) = FDF.insert path v
patchFrom (path, Change old new) = FDF.update path old new

process :: Options -> IO ()
process options = do
  diffs <- map parseLine . filter (not . Text.null) . Text.lines <$> Text.IO.readFile options.diff
  old <- readFDF options.input
  ByteString.putStr $ FDF.serialize $ appEndo (foldMap (Endo . patchFrom . first NonEmpty.fromList) diffs) $ old

main :: IO ()
main =
  OptsAp.execParser (OptsAp.info optionsParser
                     $ OptsAp.progDesc "Patch the input FDF file with the given diff")
  >>= process
