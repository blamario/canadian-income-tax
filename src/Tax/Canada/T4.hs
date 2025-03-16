{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Tax.Canada.T4 (T4(..), T4Slip(..), t4Fields) where

import Control.Applicative (ZipList(ZipList))
import Data.Fixed (Centi)
import Data.Functor.Product (Product(Pair))
import Data.CAProvinceCodes qualified as Province
import Data.Text (Text)
import Rank2 qualified
import Rank2.TH qualified
import Transformation.Shallow.TH qualified

import Tax.FDF (FieldConst (Field), Entry (Amount, Province, Textual, Year), within)

data T4 line = T4{
   slip1 :: T4Slip line,
   slip2 :: T4Slip line}

data T4Slip line = T4Slip{
   year :: line Int,
   box10_provinceOfEmployment :: line Province.Code,
   box12_SIN :: line Text,
   employerName :: line Text,
   employeeFirstName :: line Text,
   employeeLastName :: line Text,
   employeeInitial :: line Text,
   employeeAddress :: line Text,
   box14_employmentIncome :: line Centi,
   box16_employeeCPP :: line Centi,
   box16a_employeeCPP :: line Centi,
   box17_employeeQPP :: line Centi,
   box17a_employeeQPP :: line Centi,
   box18_employeeEI :: line Centi,
   box20_employeeRPP :: line Centi,
   box22_incomeTaxDeducted :: line Centi,
   box24_insurableEarnings :: line Centi,
   box26_pensionableEarnings :: line Centi,
   box29_employmentCode :: line Text,
   box44_unionDues :: line Centi,
   box45_dental :: line Text,
   box46_charityDonations :: line Centi,
   box50_rppNumber :: line Text,
   box52_pensionAdjustment :: line Centi,
   box54_employersAccount :: line Text,
   box55_premiumPPIP :: line Centi,
   box56_insurableEarningsPPIP :: line Centi,
   otherInformation :: ZipList (Product (Rank2.Only Text) (Rank2.Only Centi) line)}

deriving instance (Show (line Centi), Show (line Province.Code), Show (line Text), Show (line Int)) => Show (T4 line)
deriving instance (Eq (line Centi), Eq (line Province.Code), Eq (line Text), Eq (line Int)) => Eq (T4 line)
deriving instance (Show (line Centi), Show (line Province.Code),
                   Show (line Text), Show (line Int)) => Show (T4Slip line)
deriving instance (Eq (line Centi), Eq (line Province.Code), Eq (line Text), Eq (line Int)) => Eq (T4Slip line)

Rank2.TH.deriveFunctor ''T4Slip
Rank2.TH.deriveApply ''T4Slip
Rank2.TH.deriveApplicative ''T4Slip
Rank2.TH.deriveFoldable ''T4Slip
Rank2.TH.deriveTraversable ''T4Slip
Rank2.TH.deriveFunctor ''T4
Rank2.TH.deriveApply ''T4
Rank2.TH.deriveApplicative ''T4
Rank2.TH.deriveFoldable ''T4
Rank2.TH.deriveTraversable ''T4
Transformation.Shallow.TH.deriveAll ''T4Slip
Transformation.Shallow.TH.deriveAll ''T4

t4Fields :: T4 FieldConst
t4Fields = within "form1" . within "Page1" Rank2.<$> T4{
  slip1 = within "Slip1" Rank2.<$> t4SlipFields,
  slip2 = within "Slip2" Rank2.<$> t4SlipFields}

t4SlipFields :: T4Slip FieldConst
t4SlipFields = T4Slip{
   year = Field ["Year", "Slip1Year"] Year,
   box10_provinceOfEmployment = Field ["Box10", "Slip1Box10"] Province,
   box12_SIN = Field ["Box12", "Slip1Box12"] Textual,
   employerName = Field ["EmployersName", "Slip1EmployersName"] Textual,
   employeeFirstName = Field ["Employee", "FirstName", "Slip1FirstName"] Textual,
   employeeLastName = Field ["Employee", "LastName", "Slip1LastName"] Textual,
   employeeInitial = Field ["Employee", "Initial", "Slip1Initial"] Textual,
   employeeAddress = Field ["Employee", "Slip1Address"] Textual,
   box14_employmentIncome = Field ["Box14", "Slip1Box14"] Amount,
   box16_employeeCPP = Field ["Box16", "Slip1Box16"] Amount,
   box16a_employeeCPP = Field ["Box16A", "Slip1Box16A"] Amount,
   box17_employeeQPP = Field ["Box17", "Slip1Box17"] Amount,
   box17a_employeeQPP = Field ["Box17A", "Slip1Box17A"] Amount,
   box18_employeeEI = Field ["Box18", "Slip1Box18"] Amount,
   box20_employeeRPP = Field ["Box20", "Slip1Box20"] Amount,
   box22_incomeTaxDeducted = Field ["Box22", "Slip1Box22"] Amount,
   box24_insurableEarnings = Field ["Box24", "Slip1Box24"] Amount,
   box26_pensionableEarnings = Field ["Box26", "Slip1Box26"] Amount,
   box29_employmentCode = Field ["Box29", "Slip1Box29"] Textual,
   box44_unionDues = Field ["Box44", "Slip1Box44"] Amount,
   box45_dental = Field ["Box45", "DropDownList"] Textual,
   box46_charityDonations = Field ["Box46", "Slip1Box46"] Amount,
   box50_rppNumber = Field ["Box50", "Slip1Box50"] Textual,
   box52_pensionAdjustment = Field ["Box52", "Slip1Box52"] Amount,
   box54_employersAccount = Field ["EmployersAccount", "Slip1Box54"] Textual,
   box55_premiumPPIP = Field ["Box55", "Slip1Box55"] Amount,
   box56_insurableEarningsPPIP = Field ["Box56", "Slip1Box56"] Amount,
   otherInformation = (within "OtherInformation" Rank2.<$>) <$> ZipList [
       Rank2.Only (Field ["Box1", "Slip1Box1"] Textual)
       `Pair`
       Rank2.Only (Field ["Amount1", "Slip1Amount1"] Amount),
       Rank2.Only (Field ["Box2", "Slip1Box2"] Textual)
       `Pair`
       Rank2.Only (Field ["Amount2", "Slip1Amount2"] Amount),
       Rank2.Only (Field ["Box3", "Slip1Box3"] Textual)
       `Pair`
       Rank2.Only (Field ["Amount3", "Slip1Amount3"] Amount),
       Rank2.Only (Field ["Box4", "Slip1Box4"] Textual)
       `Pair`
       Rank2.Only (Field ["Amount4", "Slip1Amount4"] Amount),
       Rank2.Only (Field ["Box5", "Slip1Box5"] Textual)
       `Pair`
       Rank2.Only (Field ["Amount5", "Slip1Amount5"] Amount),
       Rank2.Only (Field ["Box6", "Slip1Box6"] Textual)
       `Pair`
       Rank2.Only (Field ["Amount6", "Slip1Amount6"] Amount)]}
