{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tax.Canada.Province.ON.ON428.Fix (ON428, fixON428) where

import Control.Applicative (liftA2)
import Control.Monad (guard, mfilter)
import Data.Fixed (Centi)
import Rank2 qualified

import Tax.Canada.Province.ON.ON428.Types
import Tax.Canada.Shared (fixBaseCredit, fixMedicalExpenses, fixSubCalculation, fixTaxIncomeBracket,
                          BaseCredit(cont), MedicalExpenses (difference),
                          SubCalculation (result), TaxIncomeBracket (equalsTax))
import Tax.Util (fixEq, fractionOf, nonNegativeDifference, totalOf)

fixON428 :: ON428 Maybe -> ON428 Maybe
fixON428 = fixEq $ \on428@ON428{..}-> ON428{page1 = fixPage1 page1,
                                            page2 = fixPage2 on428 page2,
                                            page3 = fixPage3 on428 page3,
                                            page4 = fixPage4 on428 page4}

fixPage1 :: Page1 Maybe -> Page1 Maybe
fixPage1 = fixEq $ \Page1{..}-> Page1{
   line1 = line1,
   partA = fixPage1PartA line1 partA,
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
   line9_basic = Just 11141,
   spouseAmount = fixBaseCredit spouseAmount,
   dependantAmount = fixBaseCredit dependantAmount,
   line18 = totalOf [line9_basic, line10_age, spouseAmount.cont, dependantAmount.cont, line17_caregiver],
   line24_sum = fixSubCalculation $
                totalOf [line19_cppQpp,
                         line20_cppQpp,
                         line21_employmentInsurance,
                         line22_employmentInsurance,
                         line23_adoption],
   line25 = totalOf [line18, line24_sum.result]}

fixPage2 :: ON428 Maybe -> Page2 Maybe -> Page2 Maybe
fixPage2 on428 = fixEq $ \Page2{..}-> Page2{
  partB = fixPage2PartB on428 partB,
  partC = fixPage2PartC on428 partC}

fixPage2PartB :: ON428 Maybe -> Page2PartB Maybe -> Page2PartB Maybe
fixPage2PartB on428 = fixEq $ \part@Page2PartB{..}-> part{
   line26 = on428.page1.partB.line25,
   line28 = totalOf [line26, line27_pension],
   line31 = totalOf [line28, line29_disability, line30],
   line35 = totalOf [line31, line32_interest, line33_education, line34_transferred],
   medicalExpenses = fixMedicalExpenses 2522 medicalExpenses,
   line43_sum = fixSubCalculation $ totalOf [medicalExpenses.difference, line42],
   line44 = totalOf [line35, line43_sum.result],
   line46_fraction = line45_rate `fractionOf` line44,
   donations = fixDonations donations,
   line50 = totalOf [line46_fraction, donations.line49_sum.result]}

fixPage2PartC :: ON428 Maybe -> Page2PartC Maybe -> Page2PartC Maybe
fixPage2PartC on428 = fixEq $ \part@Page2PartC{..}-> part{
   line51_tax = totalOf [on428.page1.partA.column1.equalsTax,
                         on428.page1.partA.column2.equalsTax,
                         on428.page1.partA.column3.equalsTax,
                         on428.page1.partA.column4.equalsTax,
                         on428.page1.partA.column5.equalsTax],
   line52_credits = on428.page2.partB.line50,
   line53 = nonNegativeDifference line51_tax line52_credits,
   line55 = totalOf [line53, line54],
   line56 = line53,
   line58 = nonNegativeDifference line56 line57,
   line59_product = Just 0.3367 `fractionOf` line59_copy,
   line60_lesser = min <$> line58 <*> line59_product,
   line61 = nonNegativeDifference line55 line60_lesser}

fixDonations :: Donations Maybe -> Donations Maybe
fixDonations = fixEq $ \part@Donations{..} -> part{
   line47_fraction = Just 0.0505 `fractionOf` line47_base,
   line48_fraction = Just 0.1116 `fractionOf` line48_base,
   line49_sum = fixSubCalculation $ totalOf [line47_fraction, line48_fraction]}

fixPage3 :: ON428 Maybe -> Page3 Maybe -> Page3 Maybe
fixPage3 on428 = fixEq $ \page@Page3{..}-> page{
   line62 = on428.page2.partC.line61,
   line63 = line62,
   line64 = on428.page2.partC.line54,
   line65 = nonNegativeDifference line63 line64,
   line66_copy = line65,
   line66_surtax = Just 0.2 `fractionOf` nonNegativeDifference line66_copy (Just 4991),
   line67_copy = line65,
   line67_surtax = Just 0.36 `fractionOf` nonNegativeDifference line67_copy (Just 6387),
   line68_sum = fixSubCalculation $ totalOf [line66_surtax, line67_surtax],
   line69 = totalOf [line62, line68_sum.result],
   line70 = on428.page2.partC.line57,
   line71 = nonNegativeDifference line69 line70,
   line73 = totalOf [line71, line72],
   line75_amount = ((475 *) . fromIntegral) <$> line75_childrenNum,
   line76_amount = ((475 *) . fromIntegral) <$> line76_childrenNum,
   line77 = totalOf [line74_basicReduction, line75_amount, line76_amount],
   line78_copy = line77,
   line78_product = (2 *) <$> line78_copy,
   line79 = line73,
   line80_difference = fixSubCalculation $ nonNegativeDifference line78_product line79,
   line81 = nonNegativeDifference line73 line80_difference.result,
   line83 = nonNegativeDifference line81 line82}

fixPage4 :: ON428 Maybe -> Page4 Maybe -> Page4 Maybe
fixPage4 on428 = fixEq $ \page@Page4{..}-> page{
   line84 = on428.page3.line83,
   line86 = nonNegativeDifference line84 line85_lift,
   line87_fraction = Just 0.25 `fractionOf` line87_foodDonations,
   line88 = nonNegativeDifference line86 line87_fraction,
   line89_health = totalOf [between 0 20000 0,
                            healthPremium.row1.equalsTax,
                            between 25000 36000 300,
                            healthPremium.row2.equalsTax,
                            between 38500 48000 450,
                            healthPremium.row3.equalsTax,
                            between 48600 72000 600,
                            healthPremium.row4.equalsTax,
                            between 72600 200000 750,
                            healthPremium.row5.equalsTax,
                            if income > 200600 then Just 900 else Nothing],
   line90 = totalOf [line88, line89_health],
   healthPremium = fixHealthPremium income healthPremium}
   where income = sum on428.page1.line1
         between floor ceiling tax
            | income > floor && income <= ceiling = Just tax
            | otherwise = Nothing

fixHealthPremium :: Centi -> HealthPremium Maybe -> HealthPremium Maybe
fixHealthPremium income = fixEq $ \HealthPremium{..}-> HealthPremium{
   row1 = fixHealthPremiumBracket income 20000 25000 0.06 0 row1,
   row2 = fixHealthPremiumBracket income 36000 38500 0.06 300 row2,
   row3 = fixHealthPremiumBracket income 48000 48600 0.25 450 row3,
   row4 = fixHealthPremiumBracket income 72000 72600 0.25 600 row4,
   row5 = fixHealthPremiumBracket income 200000 200600 0.25 750 row5}

fixHealthPremiumBracket :: Centi -> Centi -> Centi -> Rational -> Centi
                        -> HealthPremiumBracket Maybe -> HealthPremiumBracket Maybe
fixHealthPremiumBracket income floor ceiling rate base HealthPremiumBracket{..}
   | income > floor && income < ceiling = HealthPremiumBracket{
       taxableIncome = Just income,
       overThreshold = Just $ income - floor,
       timesRate = Just rate `fractionOf` overThreshold,
       equalsTax = totalOf [timesRate, Just base]}
   | otherwise = Rank2.pure Nothing
