{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Tax.Canada (Forms, completeForms, fixT1) where

import Data.CAProvinceCodes qualified as Province
import Data.Functor.Const (Const)
import Data.Functor.Product (Product (Pair))
import Data.Kind (Type)
import Rank2 qualified
import Text.FDF (FDF, foldMapWithKey, mapWithKey, parse, serialize)

import Tax.Canada.T1.Types (T1)
import Tax.Canada.T1.Fix (fixT1)
import Tax.Canada.Province.AB qualified as AB
import Tax.Canada.Province.MB qualified as MB
import Tax.Canada.Province.ON qualified as ON
import Tax.Canada.Province.QC qualified as QC
import Tax.FDF (FieldConst, within)

type family Forms (p :: Province.Code) = (f :: (Type -> Type) -> Type) | f -> p where
   Forms 'Province.AB = Product T1 AB.AB428
   Forms 'Province.ON = ON.Returns
   Forms 'Province.QC = T1

class TaxCompletable (p :: Province.Code) where
   completeForms :: Forms p Maybe -> Forms p Maybe
   formFields :: Forms p FieldConst

instance TaxCompletable 'Province.AB where
   completeForms (Pair t1 ab428) = uncurry Pair $ AB.fixReturns (t1, ab428)
   formFields = Pair (within "T1" Rank2.<$> AB.t1Fields) (within "AB428" Rank2.<$> AB.ab428Fields)

instance TaxCompletable 'Province.ON where
   completeForms = ON.fixReturns
   formFields = ON.returnFields

instance TaxCompletable 'Province.QC where
   completeForms = fixT1
   formFields = QC.t1Fields
