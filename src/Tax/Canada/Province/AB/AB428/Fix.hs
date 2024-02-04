{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tax.Canada.Province.AB.AB428.Fix (AB428, fixAB428) where

import Control.Applicative (liftA2)
import Control.Monad (guard)
import Data.Fixed (Centi)
import Rank2 qualified

import Tax.Canada.Province.AB.AB428.Types
import Tax.Canada.Shared (fixBaseCredit, fixMedicalExpenses, fixSubCalculation, fixTaxIncomeBracket,
                          BaseCredit(cont), MedicalExpenses (difference),
                          SubCalculation (result), TaxIncomeBracket (equalsTax))
import Tax.Util (fixEq, fractionOf, nonNegativeDifference, totalOf)

fixAB428 :: AB428 Maybe -> AB428 Maybe
fixAB428 = fixEq $ \ab428@AB428{..}-> AB428{page1 = fixPage1 page1,
                                            page2 = fixPage2 ab428 page2,
                                            page3 = fixPage3 ab428 page3}

fixPage1 :: Page1 Maybe -> Page1 Maybe
fixPage1 = fixEq $ \Page1{..}-> Page1{
   income = income,
   partA = fixPage1PartA income partA,
   partB = fixPage1PartB partB}

fixPage1PartA :: Maybe Centi -> Page1PartA Maybe -> Page1PartA Maybe
fixPage1PartA income = fixEq $ \Page1PartA{..}-> Page1PartA{
   column1 = fixTaxIncomeBracket income (Just column2) column1,
   column2 = fixTaxIncomeBracket income (Just column3) column2,
   column3 = fixTaxIncomeBracket income (Just column4) column3,
   column4 = fixTaxIncomeBracket income (Just column5) column4,
   column5 = fixTaxIncomeBracket income Nothing column5}

fixPage1PartB :: Page1PartB Maybe -> Page1PartB Maybe
fixPage1PartB = fixEq $ \part@Page1PartB{..}-> part{
   line9_basic = Just 19814,
   spouseAmount = fixBaseCredit spouseAmount,
   dependantAmount = fixBaseCredit dependantAmount,
   line18 = totalOf [line9_basic, line10_age, spouseAmount.cont, dependantAmount.cont, line17_infirm],
   line24_sum = fixSubCalculation $
                totalOf [line19_cppQpp,
                         line20_cppQpp,
                         line21_employmentInsurance,
                         line22_employmentInsurance,
                         line23_adoption],
   line25 = totalOf [line18, line24_sum.result]}

fixPage2 :: AB428 Maybe -> Page2 Maybe -> Page2 Maybe
fixPage2 ab428 = fixEq $ \Page2{..}-> Page2{
  partB = fixPage2PartB ab428 partB}

fixPage2PartB :: AB428 Maybe -> Page2PartB Maybe -> Page2PartB Maybe
fixPage2PartB ab428 = fixEq $ \part@Page2PartB{..}-> part{
   line26 = ab428.page1.partB.line25,
   line29 = totalOf [line26, line27_pension, line28_caregiver],
   line32 = totalOf [line29, line30_disability, line31],
   line36 = totalOf [line32, line33_interest, line34_education, line35_transferredSpouse],
   medicalExpenses = fixMedicalExpenses 2350 medicalExpenses,
   line44_sum = fixSubCalculation $ totalOf [medicalExpenses.difference, line43],
   line45 = totalOf [line36, line44_sum.result],
   line47_fraction = line46_rate `fractionOf` line45,
   donations = fixDonations donations,
   line50_sum = fixSubCalculation $ totalOf [donations.line48_fraction, donations.line49_fraction],
   line51 = totalOf [line47_fraction, line50_sum.result]}

fixDonations :: Donations Maybe -> Donations Maybe
fixDonations = fixEq $ \part@Donations{..} -> part{
   line48_fraction = Just 0.1 `fractionOf` line48_base,
   line49_fraction = Just 0.21 `fractionOf` line49_base}

fixPage3 :: AB428 Maybe -> Page3 Maybe -> Page3 Maybe
fixPage3 ab428 = fixEq $ \page@Page3{..}-> page{
   partC = fixPartC ab428 partC,
   partD = fixPartD ab428 partD}

fixPartC :: AB428 Maybe -> PartC Maybe -> PartC Maybe
fixPartC ab428 = fixEq $ \part@PartC{..}-> part{
   line52_tax = totalOf [ab428.page1.partA.column1.equalsTax,
                         ab428.page1.partA.column2.equalsTax,
                         ab428.page1.partA.column3.equalsTax,
                         ab428.page1.partA.column4.equalsTax,
                         ab428.page1.partA.column5.equalsTax],
   line54 = totalOf [line52_tax, line53_splitIncomeTax],
   line55_copy = ab428.page2.partB.line51,
   line57_fraction = Just 0.35 `fractionOf` line57_copy,
   line58_sum = fixSubCalculation $ totalOf [line55_copy, line56_dividendCredits, line57_fraction],
   line59_difference = nonNegativeDifference line54 line58_sum.result,
   line60_fraction = Just 0.35 `fractionOf` line60_fromT691,
   line61 = totalOf [line59_difference, line60_fraction],
   line63_difference = nonNegativeDifference line61 line62_foreignCredit,
   line66_tax = nonNegativeDifference line63_difference line65_political}

fixPartD :: AB428 Maybe -> PartD Maybe -> PartD Maybe
fixPartD _ab428 = fixEq $ \part@PartD{..}-> part{
   line69_credits = totalOf [line67_investorCredit, line68_stockCredit]}
