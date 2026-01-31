{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.T1.FieldNames.PE (t1Fields, NT.page2Fields, BC.page8Fields) where

import Rank2 qualified

import Tax.FDF (FieldConst, within)
import Tax.Canada.T1.Types
import Tax.Canada.T1.FieldNames.AB qualified as AB
import Tax.Canada.T1.FieldNames.BC qualified as BC
import Tax.Canada.T1.FieldNames.NT qualified as NT

t1Fields :: T1 FieldConst
t1Fields = AB.t1Fields{
   page2 = within "form1" . within "Page2" Rank2.<$> NT.page2Fields,
   page3 = BC.t1Fields.page3,
   page8 = BC.t1Fields.page8}
