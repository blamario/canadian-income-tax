{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tax.Canada.Province.BC.BC428.Fix (BC428, fixBC428) where

import Control.Applicative (liftA2)
import Control.Monad (guard, mfilter)
import Data.Fixed (Centi)
import Rank2 qualified

import Tax.Canada.Province.BC.BC428.Types
import Tax.Canada.Shared (fixBaseCredit, fixMedicalExpenses, fixSubCalculation, fixTaxIncomeBracket,
                          BaseCredit(cont), MedicalExpenses (difference),
                          SubCalculation (result), TaxIncomeBracket (equalsTax))
import Tax.Util (fixEq, fractionOf, nonNegativeDifference, totalOf)

fixBC428 :: BC428 Maybe -> BC428 Maybe
fixBC428 = fixEq $ \bc428@BC428{..}-> BC428{page1 = fixPage1 page1,
                                            page2 = fixPage2 bc428 page2,
                                            page3 = fixPage3 bc428 page3}

fixPage1 :: Page1 Maybe -> Page1 Maybe
fixPage1 = fixEq $ \Page1{..}-> Page1{
   partA = fixPage1PartA partA,
   partB = fixPage1PartB partB}

fixPage1PartA :: Page1PartA Maybe -> Page1PartA Maybe
fixPage1PartA = fixEq $ \Page1PartA{..}-> Page1PartA{
   income = income,
   column1 = fixTaxIncomeBracket income (Just column2) column1,
   column2 = fixTaxIncomeBracket income (Just column3) column2,
   column3 = fixTaxIncomeBracket income (Just column4) column3,
   column4 = fixTaxIncomeBracket income (Just column5) column4,
   column5 = fixTaxIncomeBracket income (Just column6) column5,
   column6 = fixTaxIncomeBracket income (Just column7) column6,
   column7 = fixTaxIncomeBracket income Nothing column7}

fixPage1PartB :: Page1PartB Maybe -> Page1PartB Maybe
fixPage1PartB = fixEq $ \part@Page1PartB{..}-> part{
   line16_basic = Just 11981,
   spouseAmount = fixBaseCredit spouseAmount,
   dependantAmount = fixBaseCredit dependantAmount,
   line25 = totalOf [line16_basic, line17_age, spouseAmount.cont, dependantAmount.cont, line24_caregiver]}

fixPage2 :: BC428 Maybe -> Page2 Maybe -> Page2 Maybe
fixPage2 bc428 = fixEq $ \Page2{..}-> Page2{
  partB = fixPage2PartB bc428 partB}

fixPage2PartB :: BC428 Maybe -> Page2PartB Maybe -> Page2PartB Maybe
fixPage2PartB bc428 = fixEq $ \part@Page2PartB{..}-> part{
   line26 = bc428.page1.partB.line25,
   line33_sum = fixSubCalculation id $
                totalOf [line27_cppQpp,
                         line28_cppQpp,
                         line29_employmentInsurance,
                         line30_employmentInsurance,
                         line31_firefighters,
                         line32_rescue],
   line35 = totalOf [line26, line33_sum.result, line34_adoption],
   line37 = totalOf [line35, line36_pension],
   line40 = totalOf [line37, line38_disability, line39],
   line45 = totalOf [line40, line41_interest, line42_education, line43_transferredChild, line44_transferredSpouse],
   medicalExpenses = fixMedicalExpenses 2491 medicalExpenses,
   line53_sum = fixSubCalculation id $ totalOf [medicalExpenses.difference, line52],
   line54 = totalOf [line45, line53_sum.result],
   line56_fraction = line55_rate `fractionOf` line54,
   line58 = totalOf [line56_fraction, line57_donations],
   line59_fraction = Just 0.25 `fractionOf` line59_food,
   line60 = totalOf [line58, line59_fraction]}

fixPartC :: BC428 Maybe -> PartC Maybe -> PartC Maybe
fixPartC bc428 = fixEq $ \part@PartC{..}-> part{
   line61_tax = totalOf [bc428.page1.partA.column1.equalsTax,
                         bc428.page1.partA.column2.equalsTax,
                         bc428.page1.partA.column3.equalsTax,
                         bc428.page1.partA.column4.equalsTax,
                         bc428.page1.partA.column5.equalsTax,
                         bc428.page1.partA.column6.equalsTax,
                         bc428.page1.partA.column7.equalsTax],
   line63 = totalOf [line61_tax, line62_splitIncomeTax],
   line64_copy = bc428.page2.partB.line60,
   line66_fraction = Just 0.337 `fractionOf` line66_copy,
   line67_sum = fixSubCalculation id $ totalOf [line64_copy, line65_dividendCredits, line66_fraction],
   line68 = nonNegativeDifference line63 line67_sum.result,
   line69_fraction = Just 0.337 `fractionOf` line69_copy,
   line70 = totalOf [line68, line69_fraction],
   line72 = nonNegativeDifference line70 line71_foreignCredit}

fixPage3 :: BC428 Maybe -> Page3 Maybe -> Page3 Maybe
fixPage3 bc428 = fixEq $ \page@Page3{..}-> page{
   partC = fixPartC bc428 partC,
   line73_basicReduction = Just 521,
   line76_difference = nonNegativeDifference line74_copy line75_base,
   line78_fraction = fixSubCalculation id $ line77_rate `fractionOf` line76_difference,
   line79_difference = fixSubCalculation id $ nonNegativeDifference line73_basicReduction line78_fraction.result,
   line80_difference = nonNegativeDifference partC.line72 line79_difference.result,
   line82_difference = nonNegativeDifference line80_difference line81_logging,
   line85_difference = nonNegativeDifference line82_difference line84_political,
   line88_sum = fixSubCalculation id $ min 2000 <$> totalOf [line86_esop20, line87_evcc30],
   line89_difference = nonNegativeDifference line85_difference line88_sum.result,
   line91_tax = nonNegativeDifference line89_difference line90_mining}
