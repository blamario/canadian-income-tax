{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tax.Canada.Province.MB.MB428.Fix (MB428, fixMB428) where

import Control.Applicative (liftA2)
import Control.Monad (guard)
import Data.Fixed (Centi)
import Rank2 qualified

import Tax.Canada.Province.MB.MB428.Types
import Tax.Canada.Shared (fixBaseCredit, fixMedicalExpenses, fixSubCalculation, fixTaxIncomeBracket,
                          BaseCredit(cont), MedicalExpenses (difference),
                          SubCalculation (result), TaxIncomeBracket (equalsTax))
import Tax.Util (fixEq, fractionOf, nonNegativeDifference, totalOf)

fixMB428 :: MB428 Maybe -> MB428 Maybe
fixMB428 = fixEq $ \mb428@MB428{..}-> MB428{page1 = fixPage1 page1,
                                            page2 = fixPage2 mb428 page2,
                                            page3 = fixPage3 mb428 page3}

fixPage1 :: Page1 Maybe -> Page1 Maybe
fixPage1 = fixEq $ \Page1{..}-> Page1{
   income = income,
   partA = fixPage1PartA income partA,
   partB = fixPage1PartB partB}

fixPage1PartA :: Maybe Centi -> Page1PartA Maybe -> Page1PartA Maybe
fixPage1PartA income = fixEq $ \Page1PartA{..}-> Page1PartA{
   column1 = fixTaxIncomeBracket income (Just column2) column1,
   column2 = fixTaxIncomeBracket income (Just column3) column2,
   column3 = fixTaxIncomeBracket income Nothing column3}

fixPage1PartB :: Page1PartB Maybe -> Page1PartB Maybe
fixPage1PartB = fixEq $ \part@Page1PartB{..}-> part{
   line9_basic = Just 19814,
   spouseAmount = fixBaseCredit spouseAmount,
   dependantAmount = fixBaseCredit dependantAmount,
   line18 = totalOf [line9_basic, line10_age, spouseAmount.cont, dependantAmount.cont, line17_infirm],
   line28_sum = fixSubCalculation $
                totalOf [line19_cppQpp,
                         line20_cppQpp,
                         line21_employmentInsurance,
                         line22_employmentInsurance,
                         line23_firefighters,
                         line24_rescue,
                         line25_fitness,
                         line26_arts,
                         line27_adoption],
   line29 = totalOf [line18, line28_sum.result]}

fixPage2 :: MB428 Maybe -> Page2 Maybe -> Page2 Maybe
fixPage2 mb428 = fixEq $ \Page2{..}-> Page2{
  partB = fixPage2PartB mb428 partB}

fixPage2PartB :: MB428 Maybe -> Page2PartB Maybe -> Page2PartB Maybe
fixPage2PartB mb428 = fixEq $ \part@Page2PartB{..}-> part{
   line30 = mb428.page1.partB.line29,
   line33 = totalOf [line30, line31_pension, line32_caregiver],
   line36 = totalOf [line33, line34_disability, line35],
   line42_sum = totalOf [line36,
                         line37_interest,
                         line38_education,
                         line39_transferredChild,
                         line40_transferredSpouse,
                         line41_family],
   medicalExpenses = fixMedicalExpenses 1728 medicalExpenses,
   line50_sum = fixSubCalculation $ totalOf [medicalExpenses.difference, line49],
   line51 = totalOf [line42_sum, line50_sum.result],
   line53_fraction = line52_rate `fractionOf` line51,
   donations = fixDonations donations,
   line56_sum = fixSubCalculation $ totalOf [donations.line54_fraction, donations.line55_fraction],
   line57 = totalOf [line53_fraction, line56_sum.result]}

fixDonations :: Donations Maybe -> Donations Maybe
fixDonations = fixEq $ \part@Donations{..} -> part{
   line54_fraction = Just 0.108 `fractionOf` line54_base,
   line55_fraction = Just 0.174 `fractionOf` line55_base}

fixPage3 :: MB428 Maybe -> Page3 Maybe -> Page3 Maybe
fixPage3 mb428 = fixEq $ \page@Page3{..}-> page{
   partC = fixPartC mb428 partC}

fixPartC :: MB428 Maybe -> PartC Maybe -> PartC Maybe
fixPartC mb428 = fixEq $ \part@PartC{..}-> part{
   line58_tax = totalOf [mb428.page1.partA.column1.equalsTax,
                         mb428.page1.partA.column2.equalsTax,
                         mb428.page1.partA.column3.equalsTax],
   line60 = totalOf [line58_tax, line59_splitIncomeTax],
   line61_copy = mb428.page2.partB.line57,
   line63_fraction = Just 0.5 `fractionOf` line63_copy,
   line64_sum = fixSubCalculation $ totalOf [line61_copy, line62_dividendCredits, line63_fraction],
   line65_difference = nonNegativeDifference line60 line64_sum.result,
   line66_fraction = Just 0.5 `fractionOf` line66_fromT691,
   line67 = totalOf [line65_difference, line66_fraction],
   line70_difference = nonNegativeDifference line67 line69_political,
   line72_difference = nonNegativeDifference line70_difference line71_labour,
   line74_difference = nonNegativeDifference line72_difference line73_foreignCredit,
   line76_difference = nonNegativeDifference line74_difference line75_community,
   line78_difference = nonNegativeDifference line76_difference line77_venture,
   line80_difference = nonNegativeDifference line78_difference line79_sharePurchase,
   line82_tax = nonNegativeDifference line80_difference line81_mineral}
