{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Tax.Canada.Province.BC.BC428.Types where

import Data.Fixed (Centi)
import Data.Text (Text)
import Language.Haskell.TH qualified as TH
import Rank2.TH qualified
import Transformation.Shallow.TH qualified

import Tax.Canada.Shared (BaseCredit, MedicalExpenses, SubCalculation, TaxIncomeBracket)

data BC428 line = BC428 {
   page1 :: Page1 line,
   page2 :: Page2 line,
   page3 :: Page3 line}

data Page1 line = Page1 {
   partA :: Page1PartA line,
   partB :: Page1PartB line}

data Page1PartA line = Page1PartA {
   income :: line Centi,
   column1 :: TaxIncomeBracket line,
   column2 :: TaxIncomeBracket line,
   column3 :: TaxIncomeBracket line,
   column4 :: TaxIncomeBracket line,
   column5 :: TaxIncomeBracket line,
   column6 :: TaxIncomeBracket line,
   column7 :: TaxIncomeBracket line}

data Page1PartB line = Page1PartB {
   line16_basic :: line Centi,
   line17_age :: line Centi,
   spouseAmount :: BaseCredit line,
   dependantAmount :: BaseCredit line,
   line24_caregiver :: line Centi,
   line25 :: line Centi}

data Page2 line = Page2 {
  partB :: Page2PartB line}

data Page2PartB line = Page2PartB {
   line26 :: line Centi,
   line27_cppQpp :: line Centi,
   line28_cppQpp :: line Centi,
   line29_employmentInsurance :: line Centi,
   line30_employmentInsurance :: line Centi,
   line31_firefighters :: line Centi,
   line32_rescue :: line Centi,
   line33_sum :: SubCalculation line,
   line34_adoption :: line Centi,
   line35 :: line Centi,
   line36_pension :: line Centi,
   line37 :: line Centi,
   line38_disability :: line Centi,
   line39 :: line Centi,
   line40 :: line Centi,
   line41_interest :: line Centi,
   line42_education :: line Centi,
   line43_transferredChild :: line Centi,
   line44_transferredSpouse :: line Centi,
   line45 :: line Centi,
   medicalExpenses :: MedicalExpenses line,
   line52 :: line Centi,
   line53_sum :: SubCalculation line,
   line54 :: line Centi,
   line55_rate :: line Rational,
   line56_fraction :: line Centi,
   line57_donations :: line Centi,
   line58 :: line Centi,
   line59_food :: line Centi,
   line59_fraction :: line Centi,
   line60 :: line Centi}

data PartC line = PartC {
   line61_tax :: line Centi,
   line62_splitIncomeTax :: line Centi,
   line63 :: line Centi,
   line64_copy :: line Centi,
   line65_dividendCredits :: line Centi,
   line66_copy :: line Centi,
   line66_fraction :: line Centi,
   line67_sum :: SubCalculation line,
   line68 :: line Centi,
   line69_copy :: line Centi,
   line69_fraction :: line Centi,
   line70 :: line Centi,
   line71_foreignCredit :: line Centi,
   line72 :: line Centi}

data Page3 line = Page3 {
   partC :: PartC line,
   line73_basicReduction :: line Centi,
   line74_copy :: line Centi,
   line75_base :: line Centi,
   line76_difference :: line Centi,
   line77_rate :: line Rational,
   line78_fraction :: SubCalculation line,
   line79_difference :: SubCalculation line,
   line80_difference :: line Centi,
   line81_logging :: line Centi,
   line82_difference :: line Centi,
   line83_political :: line Centi,
   line84_political :: line Centi,
   line85_difference :: line Centi,
   line86_esop20 :: line Text,
   line_60450_esop20 :: line Centi,
   line87_evcc30 :: line Text,
   line_60470_evcc30 :: line Centi,
   line88_sum :: SubCalculation line,
   line89_difference :: line Centi,
   line90_mining :: line Centi,
   line91_tax :: line Centi}

$(foldMap
   (\t-> concat <$> sequenceA [
       [d|
           deriving instance (Show (line Centi), Show (line Rational), Show (line Text), Show (line Word))
                          => Show ($(TH.conT t) line)
           deriving instance (Eq (line Centi), Eq (line Rational), Eq (line Text), Eq (line Word))
                          => Eq ($(TH.conT t) line)
       |],
       Rank2.TH.deriveAll t,
       Transformation.Shallow.TH.deriveAll t])
   [''BC428, ''Page1, ''Page2, ''Page3,
    ''Page1PartA, ''Page1PartB, ''Page2PartB, ''PartC])
