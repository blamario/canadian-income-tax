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

module Tax.Canada.Federal (Forms(..), fixFederalForms, formFieldsForProvince) where

import Data.CAProvinceCodes qualified as Province
import Data.Fixed (Centi)
import Data.Text (Text)
import Data.Time (Day)
import Rank2 qualified
import Rank2.TH qualified
import Transformation.Shallow.TH qualified

import Tax.Canada.Federal.Schedule9 (Schedule9(line23_sum), fixSchedule9, schedule9Fields)
import Tax.Canada.Federal.Schedule11 (Schedule11(page1), Page1(line6_trainingClaim, line17_sum), fixSchedule11, schedule11Fields)
import Tax.Canada.T1 (fixT1, t1FieldsForProvince)
import Tax.Canada.T1.Types (T1(page6, page8), Page6(line32300, line34900), Page8(step6_RefundOrBalanceOwing),
                            Page8Step6(line_45350_CTC), LanguageOfCorrespondence, MaritalStatus)
import Tax.FDF (Entry (Amount), FieldConst (Field), within)
import Tax.Util (fixEq)

data Forms line = Forms{
   t1 :: T1 line,
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

fixFederalForms :: Forms Maybe -> Forms Maybe
fixFederalForms = fixEq  $ \Forms{t1, schedule9, schedule11}-> Forms{
   t1 = fixT1 t1{page6 = t1.page6{line32300 = schedule11.page1.line17_sum, line34900 = schedule9.line23_sum},
                 page8 = t1.page8{step6_RefundOrBalanceOwing =
                                  t1.page8.step6_RefundOrBalanceOwing{line_45350_CTC = schedule11.page1.line6_trainingClaim}}},
   schedule9 = fixSchedule9 t1 schedule9,
   schedule11 = fixSchedule11 t1 schedule11}

formFieldsForProvince :: Province.Code -> Forms FieldConst
formFieldsForProvince p = Forms{
  t1 = within "T1" Rank2.<$> t1FieldsForProvince p,
  schedule9 = within "Schedule9" Rank2.<$> schedule9Fields,
  schedule11 = within "Schedule11" Rank2.<$> schedule11Fields}
