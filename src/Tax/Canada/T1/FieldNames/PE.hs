{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.T1.FieldNames.PE (t1Fields, BC.page2Fields, BC.page8Fields) where

import Rank2 qualified

import Tax.FDF (FieldConst (Field), Entry (..), within)
import Tax.Canada.T1.Types
import Tax.Canada.T1.FieldNames.AB qualified as AB
import Tax.Canada.T1.FieldNames.BC qualified as BC

t1Fields :: T1 FieldConst
t1Fields = AB.t1Fields{
   page2 = within "form1" . within "Page2" . within "Return-pg2" Rank2.<$> BC.page2Fields,
   page3 = BC.t1Fields.page3,
   page8 = BC.t1Fields.page8}
