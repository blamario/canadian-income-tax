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

import Tax.Canada.Federal.Schedule9 (Schedule9(line23_sum), fixSchedule9, schedule9FieldNames)
import Tax.Canada.T1 (fixT1, t1FieldsForProvince)
import Tax.Canada.T1.Types (T1(page6), Page6(line34900), LanguageOfCorrespondence, MaritalStatus)
import Tax.FDF (Entry (Amount), FieldConst (Field), within)
import Tax.Util (fixEq)

data Forms line = Forms{
   t1 :: T1 line,
   schedule9 :: Schedule9 line}

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
fixFederalForms = fixEq  $ \Forms{t1, schedule9}-> Forms{
   t1 = fixT1 t1{page6 = t1.page6{line34900 = schedule9.line23_sum}},
   schedule9 = fixSchedule9 t1 schedule9}

formFieldsForProvince :: Province.Code -> Forms FieldConst
formFieldsForProvince p = Forms{
  t1 = within "T1" Rank2.<$> t1FieldsForProvince p,
  schedule9 = within "Schedule9" Rank2.<$> schedule9FieldNames}
