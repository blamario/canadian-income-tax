{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The federal income tax forms

module Tax.Canada.Federal (Forms(..), fixFederalForms, formFieldsForProvince) where

import Control.Applicative ((<|>))
import Data.CAProvinceCodes qualified as Province
import Data.Fixed (Centi)
import Data.Text (Text)
import Data.Time (Day)
import Rank2 qualified
import Rank2.TH qualified
import Transformation.Shallow.TH qualified

import Tax.Canada.Federal.Schedule6 qualified as Schedule6
import Tax.Canada.Federal.Schedule6 (Schedule6, fixSchedule6, schedule6Fields)
import Tax.Canada.Federal.Schedule7 qualified
import Tax.Canada.Federal.Schedule7 (Schedule7, fixSchedule7, schedule7Fields)
import Tax.Canada.Federal.Schedule9 (Schedule9(line23_sum), fixSchedule9, schedule9Fields)
import Tax.Canada.Federal.Schedule11 (Schedule11(page1), Page1(line5_trainingClaim, line17_sum), fixSchedule11, schedule11Fields)
import Tax.Canada.T1 (fixT1, t1FieldsForProvince)
import Tax.Canada.T1.Types (T1(page4, page6, page8),
                            Page4(line_20800_RRSPDeduction), Page6(line32300, line34900), Page8(step6_RefundOrBalanceOwing),
                            Page8Step6(line_45300_CWB, line_45350_CTC), LanguageOfCorrespondence, MaritalStatus)
import Tax.FDF (Entry (Amount), FieldConst (Field), within)
import Tax.Util (fixEq)

-- | All supported federal forms
data Forms line = Forms{
   t1 :: T1 line,
   schedule6 :: Schedule6 line,
   schedule7 :: Schedule7 line,
   schedule9 :: Schedule9 line,
   schedule11 :: Schedule11 line}

deriving instance (Show (line Bool), Show (line Centi), Show (line Word), Show (line Text),
                   Show (line Rational), Show (line Province.Code), Show (line Day),
                   Show (line LanguageOfCorrespondence), Show (line MaritalStatus))
               => Show (Forms line)
deriving instance (Eq (line Bool), Eq (line Centi), Eq (line Word), Eq (line Text),
                   Eq (line Rational), Eq (line Province.Code), Eq (line Day),
                   Eq (line LanguageOfCorrespondence), Eq (line MaritalStatus))
               => Eq (Forms line)

Rank2.TH.deriveAll ''Forms
Transformation.Shallow.TH.deriveAll ''Forms

-- | Complete all the federal forms, also handling the inter-form field references.
fixFederalForms :: Forms Maybe -> Forms Maybe
fixFederalForms = fixEq $ \Forms{t1, schedule6, schedule7, schedule9, schedule11}-> Forms{
   t1 = fixT1 t1{page4 = t1.page4{line_20800_RRSPDeduction = schedule7.page3.partC.line20_deduction},
                 page6 = t1.page6{line32300 = schedule11.page1.line17_sum, line34900 = schedule9.line23_sum},
                 page8 = t1.page8{step6_RefundOrBalanceOwing =
                                  t1.page8.step6_RefundOrBalanceOwing{line_45300_CWB = schedule6.page4.step3.line42_sum <|>
                                                                                       schedule6.page4.step2.line28_difference,
                                                                      line_45350_CTC = schedule11.page1.line5_trainingClaim}}},
   schedule6 = fixSchedule6 Nothing t1 schedule6,
   schedule7 = fixSchedule7 t1 schedule7,
   schedule9 = fixSchedule9 t1 schedule9,
   schedule11 = fixSchedule11 t1 schedule11}

-- | The paths of all the fields in all federal forms, with the form key added as the head of every field path.
formFieldsForProvince :: Province.Code -> Forms FieldConst
formFieldsForProvince p = Forms{
  t1 = within "T1" Rank2.<$> t1FieldsForProvince p,
  schedule6 = within "Schedule6" Rank2.<$> schedule6Fields,
  schedule7 = within "Schedule7" Rank2.<$> schedule7Fields,
  schedule9 = within "Schedule9" Rank2.<$> schedule9Fields,
  schedule11 = within "Schedule11" Rank2.<$> schedule11Fields}
