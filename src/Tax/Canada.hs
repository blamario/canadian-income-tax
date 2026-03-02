{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Tax.Canada (completeAndFilterForms, completeForms, completeRelevantForms, examine,
                   allFormKeys, relevantFormKeys, formFileNames) where

import Data.CAProvinceCodes qualified as Province
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Rank2 qualified

import Tax.Canada.Federal qualified as Federal
import Tax.Canada.FormKey qualified as FormKey
import Tax.Canada.FormKey (FormKey, Message)
import Tax.Canada.Province.AB qualified as AB
import Tax.Canada.Province.BC qualified as BC
import Tax.Canada.Province.MB qualified as MB
import Tax.Canada.Province.ON qualified as ON
import Tax.Canada.T1 as T1 (T1, fileNameForProvince)
import Tax.FDF (FDFs)
import Tax.FDF qualified as FDF

-- | Complete all FDF forms in the given map, keyed by 'FormKey'. The inter-form field references are resolved as
-- well.
completeForms :: Province.Code -> Federal.InputForms Maybe -> FDFs FormKey
              -> Either String ([Message], FDFs FormKey)
completeForms p = completeAndFilterForms allFormKeys p

allFormKeys, relevantFormKeys :: T1 Maybe -> FDFs FormKey -> Set FormKey
allFormKeys = const $ Map.keysSet
relevantFormKeys t1 _ = Federal.relevantFormKeys t1 <> alwaysRelevant
  where alwaysRelevant = Set.fromList [FormKey.Provincial428, FormKey.Provincial479, FormKey.T1]

-- | Complete the FDF forms in the given map, keyed by 'FormKey'. The inter-form field references are resolved as
-- well. Only the relevant forms that affect T1 are kept.
completeRelevantForms :: Province.Code -> Federal.InputForms Maybe -> FDFs FormKey
                      -> Either String ([Message], FDFs FormKey)
completeRelevantForms p = completeAndFilterForms relevantFormKeys p

-- | Complete the FDF forms in the given map, keyed by 'FormKey', then filter them to the 'FormKey' subset calculated
-- by the given first argument function: typically 'allFormKeys' or 'relevantFormKeys'.
completeAndFilterForms
  :: (T1 Maybe -> FDFs FormKey -> Set FormKey) -- ^ keys of completed forms to keep
  -> Province.Code
  -> Federal.InputForms Maybe                  -- ^ input-only forms like 'Tax.Canada.T4.T4'
  -> FDFs FormKey                              -- ^ forms to complete
  -> Either String ([Message], FDFs FormKey)
completeAndFilterForms keysToKeep Province.AB = mapFormsWithT1 keysToKeep AB.returnFields Rank2.fst . AB.fixReturns
completeAndFilterForms keysToKeep Province.BC = mapFormsWithT1 keysToKeep BC.returnFields (.federal) . BC.fixReturns
completeAndFilterForms keysToKeep Province.MB = mapFormsWithT1 keysToKeep MB.returnFields Rank2.fst . MB.fixReturns
completeAndFilterForms keysToKeep Province.ON = mapFormsWithT1 keysToKeep ON.returnFields (.federal) . ON.fixReturns
completeAndFilterForms keysToKeep p =
  mapFormsWithT1 keysToKeep (Federal.formFieldsForProvince p) id . Federal.fixFederalForms p

-- | Given the original and filled-in federal forms, return a list of observations for the user
examine :: Federal.Forms Maybe -> Federal.Forms Maybe -> [Message]
examine inputs outputs = []

-- | Like 'mapForms', but filters the result using the first argument function
mapFormsWithT1 :: (Rank2.Apply form, Rank2.Traversable form)
               => (T1 Maybe -> FDFs FormKey -> Set FormKey)
               -> form FDF.FieldConst
               -> (form Maybe -> Federal.Forms Maybe)
               -> (form Maybe -> form Maybe)
               -> FDFs FormKey
               -> Either String ([Message], FDFs FormKey)
mapFormsWithT1 keysToKeep fields getFederal f fdfs = do
  forms <- FDF.loadAll fields fdfs
  let forms' = f forms
      fed' = getFederal forms'
      msgs = examine (getFederal forms) fed'
      fdfs' = FDF.storeAll fields fdfs forms'
  (\x-> (msgs, Map.restrictKeys x $ keysToKeep fed'.t1 x)) <$> fdfs'

-- | A map of standard file paths of all supported forms for the given province, without the common file suffix and
-- extension.
formFileNames :: Province.Code -> Map FormKey Text
formFileNames Province.AB = AB.formFileNames <> Federal.formFileNames
formFileNames Province.BC = BC.formFileNames <> Federal.formFileNames
formFileNames Province.MB = MB.formFileNames <> Federal.formFileNames
formFileNames Province.ON = ON.formFileNames <> Federal.formFileNames
formFileNames p = Map.fromList [(FormKey.T1, T1.fileNameForProvince p)] <> Federal.formFileNames
