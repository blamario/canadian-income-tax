{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.T1.FieldNames.NU (t1Fields) where

import Rank2 qualified

import Tax.FDF (FieldConst (NoField), within)
import Tax.Canada.T1.Types
import Tax.Canada.T1.FieldNames.NB qualified as NB
import Tax.Canada.T1.FieldNames.ON qualified as ON

t1Fields :: T1 FieldConst
t1Fields = NB.t1Fields{
   page2 = within "form1" . within "Page2" . within "Step1" Rank2.<$> page2Fields,
   page3 = within "form1" . within "Page3" . within "Return-pg3" Rank2.<$> ON.page3Fields}

page2Fields = ON.page2Fields {
   cai = NoField}
