{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada (completeForms) where

import Data.CAProvinceCodes qualified as Province
import Data.Functor.Const (Const)
import Data.Functor.Product (Product (Pair))
import Data.Kind (Type)
import Data.Map (Map, fromList)
import Data.Text (Text)

import Tax.Canada.Federal qualified as Federal
import Tax.Canada.Federal (fixFederalForms)
import Tax.Canada.Province.AB qualified as AB
import Tax.Canada.Province.BC qualified as BC
import Tax.Canada.Province.MB qualified as MB
import Tax.Canada.Province.ON qualified as ON
import Tax.Canada.T1 as T1 (T1, fixT1, t1FieldsForProvince, fileNameForProvince)
import Tax.FDF (FDFs)
import Tax.FDF qualified as FDF

-- | Complete all FDF forms in the given map, keyed by identifiers (@T1@, @428@, @Schedule9@, etc). The inter-form
-- field references are resolved as well.
completeForms :: Province.Code -> Federal.InputForms Maybe -> FDFs -> Either String FDFs
completeForms Province.AB = FDF.mapForms AB.returnFields . AB.fixReturns
completeForms Province.BC = FDF.mapForms BC.returnFields . BC.fixReturns
completeForms Province.MB = FDF.mapForms MB.returnFields . MB.fixReturns
completeForms Province.ON = FDF.mapForms ON.returnFields . ON.fixReturns
completeForms p = FDF.mapForms (Federal.formFieldsForProvince p) . fixFederalForms p

formFileNames :: Province.Code -> Map Text Text
formFileNames Province.AB = AB.formFileNames <> Federal.formFileNames
formFileNames Province.BC = BC.formFileNames <> Federal.formFileNames
formFileNames Province.MB = MB.formFileNames <> Federal.formFileNames
formFileNames Province.ON = ON.formFileNames <> Federal.formFileNames
formFileNames p = fromList [("T1", T1.fileNameForProvince p)] <> Federal.formFileNames
