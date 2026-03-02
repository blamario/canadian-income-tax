{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.FormKey where

import Data.Text (Text)
import Data.Text qualified as Text

-- | The type of form keys to use as the parameter of 'Tax.FDF.FDFs'
data FormKey = T1 | T4 | Schedule6 | Schedule7 | Schedule8 | Schedule9 | Schedule11 | Provincial428 | Provincial479
             deriving (Eq, Ord, Read, Show)

-- | Message for the user about a potential issue discovered in the return by the function 'examine'
data Message = Message{
  severity :: Severity,
  form :: FormKey,
  line :: Text,
  explanation :: Text}
  deriving (Eq, Show)

-- | 'Message' severity
data Severity = Notice | Summary | Warning | Error deriving (Eq, Ord, Show)

messageText :: Message -> Text
messageText Message{form, line, explanation} =
  "At line " <> line <> " of " <> Text.pack (show form) <> ": " <> explanation
