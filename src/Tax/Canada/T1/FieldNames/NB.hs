{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.T1.FieldNames.NB where

import Rank2 qualified

import Tax.FDF (FieldConst (Field), Entry (..), within)
import Tax.Canada.T1.Types
import Tax.Canada.T1.FieldNames.PE qualified as PE

t1Fields :: T1 FieldConst
t1Fields = PE.t1Fields{
   page2 = within "form1" . within "Page2" Rank2.<$> page2Fields,
   page8 = within "form1" . within "Page8" . within "Return-pg8" Rank2.<$> page8Fields}

page2Fields :: Page2 FieldConst
page2Fields = PE.page2Fields{
   cai = Field ["CAI", "CAI_2024", "Tick_box"] Checkbox}

page8Fields :: Page8 FieldConst
page8Fields = PE.page8Fields {
   line_48400_Refund = Field ["Refund_or_Balance-Owing", "Line48400", "Line_48400_Amount"] Amount,
   line_48500_BalanceOwing = Field ["Refund_or_Balance-Owing", "Line48500", "Line_48500_Amount"] Amount}
