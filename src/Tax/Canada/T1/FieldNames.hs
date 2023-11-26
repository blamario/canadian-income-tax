{-# LANGUAGE ImportQualifiedPost #-}

module Tax.Canada.T1.FieldNames where

import GHC.Arr (Array, array, (!))
import Data.CAProvinceCodes qualified as Province

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
t1FieldsForProvince p = fieldNames ! fromEnum p

fieldNames :: Array Int (T1 FieldConst)
fieldNames = array (minBound, maxBound) $
             [(fromEnum Province.AB, AB.t1Fields),
              (fromEnum Province.BC, BC.t1Fields),
              (fromEnum Province.MB, AB.t1Fields),
              (fromEnum Province.NB, NB.t1Fields),
              (fromEnum Province.NL, NL.t1Fields),
              (fromEnum Province.NS, AB.t1Fields),
              (fromEnum Province.NT, NT.t1Fields),
              (fromEnum Province.NU, NU.t1Fields),
              (fromEnum Province.ON, ON.t1Fields),
              (fromEnum Province.PE, NB.t1Fields),
              (fromEnum Province.QC, QC.t1Fields),
              (fromEnum Province.SK, AB.t1Fields),
              (fromEnum Province.YT, YT.t1Fields)]
