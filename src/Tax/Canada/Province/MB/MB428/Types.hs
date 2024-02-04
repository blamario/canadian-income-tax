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

module Tax.Canada.Province.MB.MB428.Types where

import Data.Fixed (Centi)
import Language.Haskell.TH qualified as TH
import Rank2.TH qualified
import Transformation.Shallow.TH qualified

import Tax.Canada.Shared (BaseCredit, MedicalExpenses, SubCalculation, TaxIncomeBracket)

data MB428 line = MB428 {
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
   column3 :: TaxIncomeBracket line}

data Page1PartB line = Page1PartB {
   line9_basic :: line Centi,
   line10_age :: line Centi,
   spouseAmount :: BaseCredit line,
   dependantAmount :: BaseCredit line,
   line17_infirm :: line Centi,
   line18 :: line Centi,
   line19_cppQpp :: line Centi,
   line20_cppQpp :: line Centi,
   line21_employmentInsurance :: line Centi,
   line22_employmentInsurance :: line Centi,
   line23_firefighters :: line Centi,
   line24_rescue :: line Centi,
   line25_fitness :: line Centi,
   line26_arts :: line Centi,
   line27_adoption :: line Centi,
   line28_sum :: SubCalculation line,
   line29 :: line Centi}

data Page2 line = Page2 {
  partB :: Page2PartB line}

data Page2PartB line = Page2PartB {
   line30 :: line Centi,
   line31_pension :: line Centi,
   line32_caregiver :: line Centi,
   line33 :: line Centi,
   line34_disability :: line Centi,
   line35 :: line Centi,
   line36 :: line Centi,
   line37_interest :: line Centi,
   line38_education :: line Centi,
   line39_transferredChild :: line Centi,
   line40_transferredSpouse :: line Centi,
   line41_family :: line Centi,
   line42_sum :: line Centi,
   medicalExpenses :: MedicalExpenses line,
   line49 :: line Centi,
   line50_sum :: SubCalculation line,
   line51 :: line Centi,
   line52_rate :: line Rational,
   line53_fraction :: line Centi,
   donations :: Donations line,
   line56_sum :: SubCalculation line,
   line57 :: line Centi}

data Donations line = Donations {
   line54_base :: line Centi,
   line54_fraction :: line Centi,
   line55_base :: line Centi,
   line55_fraction :: line Centi}

data Page3 line = Page3 {
   partC :: PartC line}

data PartC line = PartC {
   line58_tax :: line Centi,
   line59_splitIncomeTax :: line Centi,
   line60 :: line Centi,
   line61_copy :: line Centi,
   line62_dividendCredits :: line Centi,
   line63_copy :: line Centi,
   line63_fraction :: line Centi,
   line64_sum :: SubCalculation line,
   line65_difference :: line Centi,
   line66_fromT691 :: line Centi,
   line66_fraction :: line Centi,
   line67 :: line Centi,
   line68_political :: line Centi,
   line69_political :: line Centi,
   line70_difference :: line Centi,
   line71_labour :: line Centi,
   line72_difference :: line Centi,
   line73_foreignCredit :: line Centi,
   line74_difference :: line Centi,
   line75_community :: line Centi,
   line76_difference :: line Centi,
   line77_venture :: line Centi,
   line78_difference :: line Centi,
   line79_sharePurchase :: line Centi,
   line80_difference :: line Centi,
   line81_mineral :: line Centi,
   line82_tax :: line Centi}

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
   [''MB428, ''Page1, ''Page2, ''Page3,
    ''Page1PartA, ''Page1PartB, ''Page2PartB, ''PartC, ''Donations])
