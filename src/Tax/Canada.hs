{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada (completeForms, completeRelevantForms, formFileNames) where

import Data.CAProvinceCodes qualified as Province
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Rank2 qualified

import Tax.Canada.Federal qualified as Federal
import Tax.Canada.Federal (fixFederalForms, relevantFormKeys)
import Tax.Canada.FormKey qualified as FormKey
import Tax.Canada.FormKey (FormKey)
import Tax.Canada.Province.AB qualified as AB
import Tax.Canada.Province.BC qualified as BC
import Tax.Canada.Province.MB qualified as MB
import Tax.Canada.Province.ON qualified as ON
import Tax.Canada.T1 as T1 (T1, fileNameForProvince)
import Tax.FDF (FDFs)
import Tax.FDF qualified as FDF

-- | Complete all FDF forms in the given map, keyed by 'FormKey'. The inter-form field references are resolved as
-- well.
completeForms :: Province.Code -> Federal.InputForms Maybe -> FDFs FormKey -> Either String (FDFs FormKey)
completeForms p = completeAndFilterForms (const $ Map.keysSet) p

-- | Complete the FDF forms in the given map, keyed by 'FormKey'. The inter-form field references are resolved as
-- well. Only the relevant forms that affect T1 are kept.
completeRelevantForms :: Province.Code -> Federal.InputForms Maybe -> FDFs FormKey -> Either String (FDFs FormKey)
completeRelevantForms p = completeAndFilterForms (\t1 _-> relevantFormKeys t1 <> alwaysRelevant) p
  where alwaysRelevant = Set.fromList [FormKey.Provincial428, FormKey.Provincial479, FormKey.T1]

-- | Complete the FDF forms in the given map, keyed by 'FormKey', then filter them to the 'FormKey' subset calculated
-- by the given function.
completeAndFilterForms
  :: (T1 Maybe -> FDFs FormKey -> Set FormKey) -- ^ keys of completed forms to keep
  -> Province.Code
  -> Federal.InputForms Maybe                  -- ^ input-only forms like 'Tax.Canada.T4.T4'
  -> FDFs FormKey                              -- ^ forms to complete
  -> Either String (FDFs FormKey)              -- ^ completed and filtered forms
completeAndFilterForms keysToKeep Province.AB =
  fmap (filterForms keysToKeep <$>)
  . mapFormsWithT1 AB.returnFields ((.t1) . Rank2.fst :: Rank2.Product Federal.Forms AB.AB428 Maybe -> T1 Maybe)
  . AB.fixReturns
completeAndFilterForms keysToKeep Province.BC =
  fmap (filterForms keysToKeep <$>)
  . mapFormsWithT1 BC.returnFields ((.federal.t1) :: BC.Returns Maybe -> T1 Maybe)
  . BC.fixReturns
completeAndFilterForms keysToKeep Province.MB =
  fmap (filterForms keysToKeep <$>)
  . mapFormsWithT1 MB.returnFields ((.t1) . Rank2.fst :: Rank2.Product Federal.Forms MB.MB428 Maybe -> T1 Maybe)
  . MB.fixReturns
completeAndFilterForms keysToKeep Province.ON =
  fmap (filterForms keysToKeep <$>)
  . mapFormsWithT1 ON.returnFields ((.federal.t1) :: ON.Returns Maybe -> T1 Maybe)
  . ON.fixReturns
completeAndFilterForms keysToKeep p =
  fmap (filterForms keysToKeep <$>)
  . mapFormsWithT1 (Federal.formFieldsForProvince p) (.t1)
  . fixFederalForms p

-- | Filter the 'FDFs' according to the supplied function
filterForms :: (T1 Maybe -> FDFs FormKey -> Set FormKey) -> (T1 Maybe, FDFs FormKey) -> FDFs FormKey
filterForms keysToKeep (t1, forms) = Map.restrictKeys forms (keysToKeep t1 forms)

-- | Like 'mapForms', but also returns the T1 form by itself.
mapFormsWithT1 :: (Rank2.Apply form, Rank2.Traversable form)
               => form FDF.FieldConst -> (form Maybe -> T1 Maybe) -> (form Maybe -> form Maybe) -> FDFs FormKey
               -> Either String (T1 Maybe, FDFs FormKey)
mapFormsWithT1 fields getT1 f fdfs = do
  forms <- FDF.loadAll fields fdfs
  let forms' = f forms
      t1' = getT1 forms'
  (,) t1' <$> FDF.storeAll fields fdfs forms'

-- | A map of standard file paths of all supported forms for the given province, without the common file suffix and
-- extension.
formFileNames :: Province.Code -> Map FormKey Text
formFileNames Province.AB = AB.formFileNames <> Federal.formFileNames
formFileNames Province.BC = BC.formFileNames <> Federal.formFileNames
formFileNames Province.MB = MB.formFileNames <> Federal.formFileNames
formFileNames Province.ON = ON.formFileNames <> Federal.formFileNames
formFileNames p = Map.fromList [(FormKey.T1, T1.fileNameForProvince p)] <> Federal.formFileNames
