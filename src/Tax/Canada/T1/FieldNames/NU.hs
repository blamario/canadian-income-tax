{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.T1.FieldNames.NU (t1Fields) where

import Rank2 qualified

import Tax.FDF (FieldConst (NoField), within)
import Tax.Canada.T1.Types
import Tax.Canada.T1.FieldNames.NB qualified as NB
import Tax.Canada.T1.FieldNames qualified as ON

t1Fields :: T1 FieldConst
t1Fields = NB.t1Fields{
   page2 = within "form1" . within "Page2" Rank2.<$> page2Fields}

page2Fields = ON.page2Fields {
   cai = NoField}
