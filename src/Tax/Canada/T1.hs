{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

-- | The T1 forms look similar, but there are subtle differences between different provinces and
-- territories. Therefore they share the same 'T1' form type and the same 'fixT1' completion function, but field
-- paths are separately provided by 't1FieldsForProvince'.
module Tax.Canada.T1 (fixT1, formPrefixForProvince, t1FieldsForProvince, module Tax.Canada.T1.Types) where

import Data.CAProvinceCodes qualified as Province
import Data.Enum.Memo (memoize)

import Tax.FDF (FieldConst)
import Tax.Canada.T1.Types
import Tax.Canada.T1.Fix (fixT1)
import Tax.Canada.T1.FieldNames.AB qualified as AB
import Tax.Canada.T1.FieldNames.BC qualified as BC
import Tax.Canada.T1.FieldNames.NB qualified as NB
import Tax.Canada.T1.FieldNames.NL qualified as NL
import Tax.Canada.T1.FieldNames.NT qualified as NT
import Tax.Canada.T1.FieldNames.NU qualified as NU
import Tax.Canada.T1.FieldNames.ON qualified as ON
import Tax.Canada.T1.FieldNames.QC qualified as QC
import Tax.Canada.T1.FieldNames.YT qualified as YT

-- | The distinct provincial prefix of the T1 form PDF file, such as @5006@ in @5006-r-fill-23e.pdf@ for Ontario
formPrefixForProvince :: Province.Code -> String
formPrefixForProvince = memoize $ \case
   Province.AB -> "5015"
   Province.BC -> "5010"
   Province.MB -> "5015"
   Province.NB -> "5000"
   Province.NL -> "5001"
   Province.NS -> "5015"
   Province.NT -> "5012"
   Province.NU -> "5014"
   Province.ON -> "5006"
   Province.PE -> "5000"
   Province.QC -> "5005"
   Province.SK -> "5015"
   Province.YT -> "5011"

-- | T1 field paths for the given province
t1FieldsForProvince :: Province.Code -> T1 FieldConst
t1FieldsForProvince = memoize $ \case
   Province.AB -> AB.t1Fields
   Province.BC -> BC.t1Fields
   Province.MB -> AB.t1Fields
   Province.NB -> NB.t1Fields
   Province.NL -> NL.t1Fields
   Province.NS -> AB.t1Fields
   Province.NT -> NT.t1Fields
   Province.NU -> NU.t1Fields
   Province.ON -> ON.t1Fields
   Province.PE -> NB.t1Fields
   Province.QC -> QC.t1Fields
   Province.SK -> AB.t1Fields
   Province.YT -> YT.t1Fields
