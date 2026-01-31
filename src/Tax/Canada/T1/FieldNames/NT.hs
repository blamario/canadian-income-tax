{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada.T1.FieldNames.NT (module Tax.Canada.T1.FieldNames.NT, page1Fields) where

import Rank2 qualified

import Tax.FDF (FieldConst (NoField), within)
import Tax.Canada.T1.Types
import Tax.Canada.T1.Types qualified as Page8 (Page8(..))
import Tax.Canada.T1.FieldNames.ON
  hiding (t1Fields,
          page2Fields, page3Fields, page4Fields, page5Fields, page6Fields, page7Fields, page8Fields,
          step4Fields, partBFields, page6MedicalExpensesFields, partCFields, page7step6Fields, page8step6Fields,
          taxPreparerFields)
import Tax.Canada.T1.FieldNames.BC qualified as BC
import Tax.Canada.T1.FieldNames.ON qualified as ON

t1Fields :: T1 FieldConst
t1Fields = ON.t1Fields {
   page2 = within "form1" . within "Page2" Rank2.<$> page2Fields,
   page8 = within "form1" . within "Page8" Rank2.<$> page8Fields}

page2Fields :: Page2 FieldConst
page2Fields = BC.page2Fields{
   organ_donor = NoField}

page8Fields :: Page8 FieldConst
page8Fields = BC.page8Fields {
   Page8.step6_RefundOrBalanceOwing = within "Step6-Cont" Rank2.<$> BC.page8step6Fields}
