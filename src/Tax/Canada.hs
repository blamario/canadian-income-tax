{-# LANGUAGE ImportQualifiedPost #-}

module Tax.Canada (completeForms) where

import Data.CAProvinceCodes qualified as Province
import Data.Functor.Const (Const)
import Data.Functor.Product (Product (Pair))
import Data.Kind (Type)

import Tax.Canada.Province.AB qualified as AB
import Tax.Canada.Province.BC qualified as BC
import Tax.Canada.Province.MB qualified as MB
import Tax.Canada.Province.ON qualified as ON
import Tax.Canada.T1 (T1, fixT1, t1FieldsForProvince)
import Tax.FDF (FDFs)
import Tax.FDF qualified as FDF

completeForms :: Province.Code -> FDFs -> Either String FDFs
completeForms Province.AB = FDF.mapForms AB.returnFields AB.fixReturns
completeForms Province.BC = FDF.mapForms BC.returnFields BC.fixReturns
completeForms Province.MB = FDF.mapForms MB.returnFields MB.fixReturns
completeForms Province.ON = FDF.mapForms ON.returnFields ON.fixReturns
completeForms p = traverse (FDF.mapForm (t1FieldsForProvince p) fixT1)
