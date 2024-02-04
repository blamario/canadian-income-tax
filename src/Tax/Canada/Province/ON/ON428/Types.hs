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

module Tax.Canada.Province.ON.ON428.Types where

import Data.Fixed (Centi)
import Language.Haskell.TH qualified as TH
import Rank2.TH qualified
import Transformation.Shallow.TH qualified

import Tax.Canada.Shared (BaseCredit, MedicalExpenses, SubCalculation, TaxIncomeBracket)

data ON428 line = ON428 {
   page1 :: Page1 line,
   page2 :: Page2 line,
   page3 :: Page3 line,
   page4 :: Page4 line}

data Page1 line = Page1 {
   line1 :: line Centi,
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
   spouseAmount :: BaseCredit line,
   dependantAmount :: BaseCredit line,
   line17_caregiver :: line Centi,
   line18 :: line Centi,
   line19_cppQpp :: line Centi,
   line20_cppQpp :: line Centi,
   line21_employmentInsurance :: line Centi,
   line22_employmentInsurance :: line Centi,
   line23_adoption :: line Centi,
   line24_sum :: SubCalculation line,
   line25 :: line Centi}

data Page2 line = Page2 {
  partB :: Page2PartB line,
  partC :: Page2PartC line}

data Page2PartB line = Page2PartB {
   line26 :: line Centi,
   line27_pension :: line Centi,
   line28 :: line Centi,
   line29_disability :: line Centi,
   line30 :: line Centi,
   line31 :: line Centi,
   line32_interest :: line Centi,
   line33_education :: line Centi,
   line34_transferred :: line Centi,
   line35 :: line Centi,
   medicalExpenses :: MedicalExpenses line,
   line42 :: line Centi,
   line43_sum :: SubCalculation line,
   line44 :: line Centi,
   line45_rate :: line Rational,
   line46_fraction :: line Centi,
   donations :: Donations line,
   line50 :: line Centi}

data Donations line = Donations {
   line47_base :: line Centi,
   line47_fraction :: line Centi,
   line48_base :: line Centi,
   line48_fraction :: line Centi,
   line49_sum :: SubCalculation line}

data Page2PartC line = Page2PartC {
   line51_tax :: line Centi,
   line52_credits :: line Centi,
   line53 :: line Centi,
   line54 :: line Centi,
   line55 :: line Centi,
   line56 :: line Centi,
   line57 :: line Centi,
   line58 :: line Centi,
   line59_copy :: line Centi,
   line59_product :: line Centi,
   line60_lesser :: line Centi,
   line61 :: line Centi}

data Page3 line = Page3 {
   line62 :: line Centi,
   line63 :: line Centi,
   line64 :: line Centi,
   line65 :: line Centi,
   line66_copy :: line Centi,
   line66_surtax :: line Centi,
   line67_copy :: line Centi,
   line67_surtax :: line Centi,
   line68_sum :: SubCalculation line,
   line69 :: line Centi,
   line70 :: line Centi,
   line71 :: line Centi,
   line72 :: line Centi,
   line73 :: line Centi,
   line74_basicReduction :: line Centi,
   line75_amount :: line Centi,
   line75_childrenNum :: line Word,
   line76_amount :: line Centi,
   line76_childrenNum :: line Word,
   line77 :: line Centi,
   line78_copy :: line Centi,
   line78_product :: line Centi,
   line79 :: line Centi,
   line80_difference :: SubCalculation line,
   line81 :: line Centi,
   line82 :: line Centi,
   line83 :: line Centi}

data Page4 line = Page4 {
   line84 :: line Centi,
   line85_lift :: line Centi,
   line86 :: line Centi,
   line87_foodDonations :: line Centi,
   line87_fraction :: line Centi,
   line88 :: line Centi,
   line89_health :: line Centi,
   line90 :: line Centi,
   healthPremium :: HealthPremium line}

data HealthPremium line = HealthPremium {
   row1 :: HealthPremiumBracket line,
   row2 :: HealthPremiumBracket line,
   row3 :: HealthPremiumBracket line,
   row4 :: HealthPremiumBracket line,
   row5 :: HealthPremiumBracket line}

data HealthPremiumBracket line = HealthPremiumBracket {
   taxableIncome :: line Centi,
   overThreshold :: line Centi,
   timesRate :: line Centi,
   equalsTax :: line Centi}

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
   [''ON428, ''Page1, ''Page2, ''Page3, ''Page4,
    ''Page1PartA, ''Page1PartB, ''Page2PartB, ''Page2PartC,
    ''Donations, ''HealthPremium, ''HealthPremiumBracket])
