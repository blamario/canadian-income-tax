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

module Tax.Canada.Province.AB.AB428.Types where

import Data.Fixed (Centi)
import Language.Haskell.TH qualified as TH
import Rank2.TH qualified
import Transformation.Shallow.TH qualified

import Tax.Canada.Shared (MedicalExpenses, TaxIncomeBracket)

data AB428 line = AB428 {
   page1 :: Page1 line,
   page2 :: Page2 line,
   page3 :: Page3 line}

data Page1 line = Page1 {
   income :: line Centi,
   partA :: Page1PartA line,
   partB :: Page1PartB line}

data Page1PartA line = Page1PartA {
   column1 :: TaxIncomeBracket line,
   column2 :: TaxIncomeBracket line,
   column3 :: TaxIncomeBracket line,
   column4 :: TaxIncomeBracket line,
   column5 :: TaxIncomeBracket line}

data Page1PartB line = Page1PartB {
   line9_basic :: line Centi,
   line10_age :: line Centi,
   line11_base :: line Centi,
   line12_spouseIncome :: line Centi,
   line13_difference :: line Centi,
   line13_cont :: line Centi,
   line14_base :: line Centi,
   line15_dependentIncome :: line Centi,
   line16_difference :: line Centi,
   line16_cont :: line Centi,
   line17_infirm :: line Centi,
   line18 :: line Centi,
   line19_cppQpp :: line Centi,
   line20_cppQpp :: line Centi,
   line21_employmentInsurance :: line Centi,
   line22_employmentInsurance :: line Centi,
   line23_adoption :: line Centi,
   line24_sum :: line Centi,
   line24_cont :: line Centi,
   line25 :: line Centi}

data Page2 line = Page2 {
  partB :: Page2PartB line}

data Page2PartB line = Page2PartB {
   line26 :: line Centi,
   line27_pension :: line Centi,
   line28_caregiver :: line Centi,
   line29 :: line Centi,
   line30_disability :: line Centi,
   line31 :: line Centi,
   line32 :: line Centi,
   line33_interest :: line Centi,
   line34_education :: line Centi,
   line35_transferredSpouse :: line Centi,
   line36 :: line Centi,
   medicalExpenses :: MedicalExpenses line,
   line43 :: line Centi,
   line44_sum :: line Centi,
   line44_cont :: line Centi,
   line45 :: line Centi,
   line46_rate :: line Rational,
   line47_fraction :: line Centi,
   donations :: Donations line,
   line50_sum :: line Centi,
   line50_cont :: line Centi,
   line51 :: line Centi}

data Donations line = Donations {
   line48_base :: line Centi,
   line48_fraction :: line Centi,
   line49_base :: line Centi,
   line49_fraction :: line Centi}

data Page3 line = Page3 {
   partC :: PartC line,
   partD :: PartD line}

data PartC line = PartC {
   line52_tax :: line Centi,
   line53_splitIncomeTax :: line Centi,
   line54 :: line Centi,
   line55_copy :: line Centi,
   line56_dividendCredits :: line Centi,
   line57_copy :: line Centi,
   line57_fraction :: line Centi,
   line58_sum :: line Centi,
   line58_cont :: line Centi,
   line59_difference :: line Centi,
   line60_fromT691 :: line Centi,
   line60_fraction :: line Centi,
   line61 :: line Centi,
   line62_foreignCredit :: line Centi,
   line63_difference :: line Centi,
   line64_political :: line Centi,
   line65_political :: line Centi,
   line66_tax :: line Centi}

data PartD line = PartD {
   line67_investorCredit :: line Centi,
   line68_stockCredit :: line Centi,
   line69_credits :: line Centi}

$(foldMap
   (\t-> concat <$> sequenceA [
       [d|
           deriving instance (Show (line Centi), Show (line Rational), Show (line Word))
                          => Show ($(TH.conT t) line)
           deriving instance (Eq (line Centi), Eq (line Rational), Eq (line Word))
                          => Eq ($(TH.conT t) line)
       |],
       Rank2.TH.deriveAll t,
       Transformation.Shallow.TH.deriveAll t])
   [''AB428, ''Page1, ''Page2, ''Page3,
    ''Page1PartA, ''Page1PartB, ''Page2PartB, ''PartC, ''PartD, ''Donations])
