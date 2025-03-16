{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.T1.FieldNames.YT (t1Fields) where

import Rank2 qualified

import Tax.FDF (FieldConst (Field), Entry (..), within)
import Tax.Canada.T1.Types
import Tax.Canada.T1.Types qualified as Page8 (Page8(..))
import Tax.Canada.T1.FieldNames.BC qualified as BC
import Tax.Canada.T1.FieldNames.NT qualified as NT
import Tax.Canada.T1.FieldNames.ON qualified as ON

t1Fields :: T1 FieldConst
t1Fields = NT.t1Fields{
   page2 = within "form1" . within "Page2" . within "Step1-Continued" Rank2.<$> NT.page2Fields,
   page7 = within "form1" . within "Page7" Rank2.<$> page7Fields,
   page8 = within "form1" . within "Page8" Rank2.<$> page8Fields}

page7Fields :: Page7 FieldConst
page7Fields = Page7 {
   partC_NetFederalTax = within "PartC" Rank2.<$> partCFields,
   step6_RefundOrBalanceOwing = within "Step6" Rank2.<$> page7step6Fields}

partCFields :: Page7PartC FieldConst
partCFields = ON.partCFields {
   line_40900 = Field ["Line4100", "Line40900", "Line_40900_Amount"] Amount,
   line_41000 = Field ["Line4100", "Line_41000_Amount"] Amount}

page7step6Fields :: Page7Step6 FieldConst
page7step6Fields = ON.page7step6Fields {
   line_43200_FirstNationsTax = Field ["Line43200", "Line_43200_Amount"] Amount}

page8Fields :: Page8 FieldConst
page8Fields = BC.page8Fields {
   Page8.step6_RefundOrBalanceOwing = within "Step6-Cont" Rank2.<$> page8step6Fields,
   taxPreparer = within "Line_49000_IfFeeWasCharged" Rank2.<$> BC.taxPreparerFields}

page8step6Fields :: Page8Step6 FieldConst
page8step6Fields = ON.page8step6Fields {
   line_43500_totalpayable = Field ["Line155", "Amount"] Amount,
   line_44100 = Field ["Line44100", "Line_44000_Amount"] Amount,
   line_46800 = Field ["Line46900", "sfline46800end", "Line_46800_Amount"] Amount,
   line_47555_TaxPaid = Field ["Line47555", "Line_47555_Amount"] Amount,
   line164_Refund_or_BalanceOwing = Field ["Line174", "Amount"] Amount}
