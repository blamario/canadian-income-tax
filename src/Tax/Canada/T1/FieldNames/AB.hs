{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.T1.FieldNames.AB (module Tax.Canada.T1.FieldNames.AB, ON.page1Fields) where

import Rank2 qualified

import Tax.FDF (FieldConst (Field, NoField), Entry (..), within)
import Tax.Canada.Shared (subCalculationFields)
import Tax.Canada.T1.Types
import Tax.Canada.T1.Types qualified as Page8 (Page8(..))
import Tax.Canada.T1.FieldNames.BC qualified as BC
import Tax.Canada.T1.FieldNames.ON qualified as ON

t1Fields :: T1 FieldConst
t1Fields = within "form1" Rank2.<$> T1 {
   page1 = within "Page1" . within "Return-pg1" Rank2.<$> ON.page1Fields,
   page2 = within "Page2" . within "Step1-Cont" Rank2.<$> page2Fields,
   page3 = within "Page3" . within "Return-pg3" Rank2.<$> ON.page3Fields,
   page4 = within "Page4" . within "Return-pg4" Rank2.<$> ON.page4Fields,
   page5 = within "Page5" . within "Return-pg5" Rank2.<$> ON.page5Fields,
   page6 = within "Page6" . within "Return-pg6" . within "PartB" Rank2.<$> ON.page6Fields,
   page7 = within "Page7" . within "Return-pg7" Rank2.<$> ON.page7Fields,
   page8 = within "Page8" . within "Return-pg8" Rank2.<$> page8Fields}

page2Fields = ON.page2Fields {
   cai = Field ["CAI", "AB_CAI", "Tick_box"] Checkbox,
   organ_donor = NoField}

page8Fields = BC.page8Fields {
   Page8.step6_RefundOrBalanceOwing = within "Step6-Cont" Rank2.<$> BC.page8step6Fields,
   line48400_Refund = Field ["Refund_or_Balance-Owing", "Line48400", "Line_48400_Amount"] Amount,
   line48500_BalanceOwing = Field ["Refund_or_Balance-Owing", "Line48500", "Line_48500_Amount"] Amount}
