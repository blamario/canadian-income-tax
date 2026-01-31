{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.T1.FieldNames.NL (t1Fields) where

import Rank2 qualified

import Tax.FDF (FieldConst (Field), Entry (..), within)
import Tax.Canada.T1.Types
import Tax.Canada.T1.Types qualified as Page8 (Page8(..))
import Tax.Canada.T1.FieldNames.ON qualified as ON
import Tax.Canada.T1.FieldNames.AB qualified as AB
import Tax.Canada.T1.FieldNames.BC qualified as BC

t1Fields :: T1 FieldConst
t1Fields = within "form1" Rank2.<$> T1 {
   page1 = within "Page1" Rank2.<$> ON.page1Fields,
   page2 = within "Page2" Rank2.<$> page2Fields,
   page3 = within "Page3" Rank2.<$> BC.page3Fields,
   page4 = within "Page4" Rank2.<$> ON.page4Fields,
   page5 = within "Page5" Rank2.<$> ON.page5Fields,
   page6 = within "Page6" . within "PartB" Rank2.<$> ON.page6Fields,
   page7 = within "Page7" Rank2.<$> ON.page7Fields,
   page8 = within "Page8" Rank2.<$> page8Fields}

page2Fields :: Page2 FieldConst
page2Fields = AB.page2Fields {
   foreign_property = Field ["Foreign_property", "Line26600"] $ Switch' "ForeignProperty_CheckBox",
   tax_exempt = Field ["Indian_Act", "IndianAct_Question", "Checkbox"] Checkbox}

page8Fields :: Page8 FieldConst
page8Fields = BC.page8Fields {
   Page8.step6_RefundOrBalanceOwing = within "Step6-Cont" Rank2.<$> BC.page8step6Fields,
   telephone = Field ["Certification", "Telephone"] Amount,
   date = Field ["Certification", "Date"] Amount}
