{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.T1.FieldNames.NB (t1Fields) where

import Rank2 qualified

import Tax.FDF (FieldConst (Field, NoField), Entry (..), within)
import Tax.Canada.T1.Types
import Tax.Canada.T1.Types qualified as Page8 (Page8(..))
import Tax.Canada.T1.FieldNames.AB qualified as AB

t1Fields :: T1 FieldConst
t1Fields = AB.t1Fields{
   page2 = within "form1" . within "Page2" Rank2.<$> page2Fields,
   page8 = within "form1" . within "Page8" Rank2.<$> page8Fields}

page2Fields = AB.page2Fields {
   cai = NoField,
   organ_donor = NoField}

page8Fields = AB.page8Fields {
   Page8.step6_RefundOrBalanceOwing = page8step6Fields}

page8step6Fields = AB.page8step6Fields {
   line_47600_TaxPaid = Field ["Line161", "Line_47600_Amount"] Amount,
   line_47900_ProvTerrCredits = Field ["Line162", "Line_47900_Amount"] Amount,
   line_48200_sum = Field ["Line163", "Line_48200_Amount1"] Amount,
   line_48200_cont = Field ["Line163", "Line_48200_Amount2"] Amount,
   line164_Refund_or_BalanceOwing = Field ["Line164", "Amount"] Amount}
