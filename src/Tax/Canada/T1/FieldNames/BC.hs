{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.T1.FieldNames.BC where

import Rank2 qualified

import Tax.FDF (FieldConst (Field, NoField), Entry (..), within)
import Tax.Canada.T1.Types
import Tax.Canada.T1.Types qualified as Page8 (Page8(..))
import Tax.Canada.T1.FieldNames.ON qualified as ON

t1Fields :: T1 FieldConst
t1Fields = ON.t1Fields {
   page2 = within "form1" . within "Page2" Rank2.<$> page2Fields,
   page8 = within "form1" . within "Page8" . within "Return-pg8" Rank2.<$> page8Fields}

page2Fields :: Page2 FieldConst
page2Fields = ON.page2Fields {
   cai = NoField,
   organ_donor = Field ["Organ_donor", "Authori"] $ Switch "Option1" "Option2" "OrganDonor_CheckBox"}

page3Fields :: Page3 FieldConst
page3Fields = ON.page3Fields

page8Fields :: Page8 FieldConst
page8Fields = ON.page8Fields {
   Page8.step6_RefundOrBalanceOwing = within "Step6-Cont" Rank2.<$> page8step6Fields,
   taxPreparer = within "Efile" Rank2.<$> taxPreparerFields,
   line1_ONOpportunitiesFund = NoField,
   line_46500 = NoField,
   line_46600 = NoField}

page8step6Fields :: Page8Step6 FieldConst
page8step6Fields = ON.page8step6Fields {
   line_45350_CTC = Field ["Line45350", "Line_45300_Amount"] Amount,
   line_45355_MHRTC = Field ["Line45355", "Line_45300_Amount"] Amount,
   line_47555_TaxPaid = Field ["Line47555", "Line_47555_Amount"] Amount}

taxPreparerFields :: TaxPreparer FieldConst
taxPreparerFields = ON.taxPreparerFields {
   eFileNumber = Field ["Line48900", "EFile"] Textual,
   line_49000_WasAFeeCharged = Field ["Line49000", "Line49000_CheckBoxGroup"] $ Switch' "Line49000_CheckBox_EN"}
