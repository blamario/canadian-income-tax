{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.T1.FieldNames.PE (t1Fields) where

import Tax.FDF (FieldConst)
import Tax.Canada.T1.Types
import Tax.Canada.T1.FieldNames.AB qualified as AB

t1Fields :: T1 FieldConst
t1Fields = AB.t1Fields
