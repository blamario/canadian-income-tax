{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.T1.FieldNames.AB (module Tax.Canada.T1.FieldNames.AB, ON.page1Fields) where

import Rank2 qualified

import Tax.FDF (FieldConst (Field, NoField), Entry (..), within)
import Tax.Canada.T1.Types
import Tax.Canada.T1.FieldNames.BC qualified as BC
import Tax.Canada.T1.FieldNames.ON qualified as ON

t1Fields :: T1 FieldConst
t1Fields = within "form1" Rank2.<$> T1 {
   page1 = within "Page1" Rank2.<$> ON.page1Fields,
   page2 = within "Page2" . within "Step1-Cont" Rank2.<$> page2Fields,
   page3 = within "Page3" Rank2.<$> ON.page3Fields,
   page4 = within "Page4" Rank2.<$> ON.page4Fields,
   page5 = within "Page5" Rank2.<$> ON.page5Fields,
   page6 = within "Page6" . within "PartB" Rank2.<$> ON.page6Fields,
   page7 = within "Page7" Rank2.<$> ON.page7Fields,
   page8 = within "Page8" Rank2.<$> BC.page8Fields}

page2Fields :: Page2 FieldConst
page2Fields = ON.page2Fields {
   organ_donor = NoField}
