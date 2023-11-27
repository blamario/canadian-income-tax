{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Tax.Canada.T1.FieldNames where

import Data.CAProvinceCodes qualified as Province
import Data.Enum.Memo (memoize)

import Tax.FDF (FieldConst)
import Tax.Canada.T1.Types (T1)
import Tax.Canada.T1.FieldNames.AB qualified as AB
import Tax.Canada.T1.FieldNames.BC qualified as BC
import Tax.Canada.T1.FieldNames.NB qualified as NB
import Tax.Canada.T1.FieldNames.NL qualified as NL
import Tax.Canada.T1.FieldNames.NT qualified as NT
import Tax.Canada.T1.FieldNames.NU qualified as NU
import Tax.Canada.T1.FieldNames.ON qualified as ON
import Tax.Canada.T1.FieldNames.QC qualified as QC
import Tax.Canada.T1.FieldNames.YT qualified as YT

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
