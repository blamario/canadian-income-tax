{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.T1.FieldNames.NB where

import Rank2 qualified

import Tax.FDF (FieldConst (Field, NoField), Entry (..), within)
import Tax.Canada.Shared (subCalculationFields)
import Tax.Canada.T1.Types
import Tax.Canada.T1.Types qualified as Page8 (Page8(..))
import Tax.Canada.T1.FieldNames.AB qualified as AB
import Tax.Canada.T1.FieldNames.BC qualified as BC

t1Fields :: T1 FieldConst
t1Fields = AB.t1Fields{
   page2 = within "form1" . within "Page2" . within "Return-pg2" Rank2.<$> BC.page2Fields,
   page3 = within "form1" . within "Page3" . within "Return-pg3" Rank2.<$> BC.page3Fields,
   page8 = within "form1" . within "Page8" . within "Return-pg8" Rank2.<$> BC.page8Fields}
