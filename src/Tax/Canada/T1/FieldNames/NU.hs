{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.T1.FieldNames.NU (t1Fields) where

import Rank2 qualified

import Tax.FDF (FieldConst, within)
import Tax.Canada.T1.Types
import Tax.Canada.T1.FieldNames.PE qualified as PE
import Tax.Canada.T1.FieldNames.ON qualified as ON

t1Fields :: T1 FieldConst
t1Fields = PE.t1Fields{
   page2 = within "form1" . within "Page2" . within "Step1" Rank2.<$> PE.page2Fields,
   page3 = within "form1" . within "Page3" Rank2.<$> ON.page3Fields}
