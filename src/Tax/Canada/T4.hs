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

module Tax.Canada.T4 (t4Fields, T4(..)) where

import Data.Fixed (Centi)
import Data.CAProvinceCodes qualified as Province
import Rank2 qualified
import Rank2.TH qualified
import Transformation.Shallow.TH qualified

import Tax.FDF (FieldConst (Field, NoField), Entry (Amount, Province), within)

data T4 line = T4{
   box10_provinceOfEmployment :: line Province.Code,
   box14_employmentIncome :: line Centi,
   box16_employeeCPP :: line Centi,
   box16a_employeeCPP :: line Centi,
   box17_employeeQPP :: line Centi,
   box17a_employeeQPP :: line Centi,
   box18_employeeEI :: line Centi,
   box20_employeeRPP :: line Centi,
   box22_incomeTaxDeducted :: line Centi,
   box39_securityOptions :: line Centi,
   box41_securityOptions :: line Centi,
   box42_employmentCommisions :: line Centi,
   box43_armedForces :: line Centi,
   box44_unionDues :: line Centi,
   box45_dental :: line Centi,
   box52_pensionAdjustment :: line Centi,
   box55_PPIP :: line Centi,
   box66_eligibleRetiringAllowance :: line Centi,
   box67_nonEligibleRetiringAllowance :: line Centi,
   box77_repaidToEmployer :: line Centi}

deriving instance (Show (line Centi), Show (line Province.Code)) => Show (T4 line)
deriving instance (Eq (line Centi), Eq (line Province.Code)) => Eq (T4 line)

Rank2.TH.deriveAll ''T4
Transformation.Shallow.TH.deriveAll ''T4

t4Fields :: T4 FieldConst
t4Fields = within "form1" . within "Page1" Rank2.<$> T4{
   box10_provinceOfEmployment = Field ["Slip1", "Box10", "Slip1Box10"] Province,
   box14_employmentIncome = Field ["Slip1", "Box14", "Slip1Box14"] Amount,
   box16_employeeCPP = Field ["Slip1", "Box16", "Slip1Box16"] Amount,
   box16a_employeeCPP = Field ["Slip1", "Box16A", "Slip1Box16A"] Amount,
   box17_employeeQPP = Field ["Slip1", "Box17", "Slip1Box17"] Amount,
   box17a_employeeQPP = Field ["Slip1", "Box17A", "Slip1Box17A"] Amount,
   box18_employeeEI = Field ["Slip1", "Box18", "Slip1Box18"] Amount,
   box20_employeeRPP = Field ["Slip1", "Box20", "Slip1Box20"] Amount,
   box22_incomeTaxDeducted = Field ["Slip1", "Box22", "Slip1Box22"] Amount,
   box39_securityOptions = Field ["Slip1", "Box39", "Slip1Box39"] Amount,
   box41_securityOptions = Field ["Slip1", "Box41", "Slip1Box41"] Amount,
   box42_employmentCommisions = Field ["Slip1", "Box42", "Slip1Box42"] Amount,
   box43_armedForces = Field ["Slip1", "Box43", "Slip1Box43"] Amount,
   box44_unionDues = Field ["Slip1", "Box44", "Slip1Box44"] Amount,
   box45_dental = Field ["Slip2", "Box45"] Amount,
   box52_pensionAdjustment = Field ["Slip1", "Box52", "Slip1Box52"] Amount,
   box55_PPIP = Field ["Slip1", "Box55", "Slip1Box55"] Amount,
   box66_eligibleRetiringAllowance = Field ["Slip1", "Box66", "Slip1Box66"] Amount,
   box67_nonEligibleRetiringAllowance = Field ["Slip1", "Box67", "Slip1Box67"] Amount,
   box77_repaidToEmployer = Field ["Slip1", "Box77", "Slip1Box77"] Amount}
