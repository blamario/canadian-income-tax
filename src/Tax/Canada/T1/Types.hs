{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Tax.Canada.T1.Types where

import Data.Fixed (Centi)
import Data.Monoid (Ap(Ap, getAp))
import Data.Text (Text)
import Data.Time (Day)
import Data.CAProvinceCodes qualified as Province
import Language.Haskell.TH qualified as TH
import Rank2.TH qualified

data T1 line = T1 {
   page1 :: Page1 line,
   page2 :: Page2 line,
   page3 :: Page3 line,
   page4 :: Page4 line,
   page5 :: Page5 line,
   page6 :: Page6 line,
   page7 :: Page7 line,
   page8 :: Page8 line}

data Page1 line = Page1 {
   identification :: Identification line,
   residence :: Residence line,
   spouse :: Spouse line}

data Identification line = Identification {
   emailAddress :: line Text,
   dateDeath :: line Day,
   postalCode :: line Text,
   your_Language :: line LanguageOfCorrespondence,
   id_City :: line Text,
   sin :: line Text,
   id_LastName :: line Text,
   dateBirth :: line Day,
   id_FirstNameInitial :: line Text,
   id_MailingAddress :: line Text,
   maritalStatus :: line MaritalStatus,
   id_RuralRoute :: line Text,
   id_POBox :: line Text,
   prov_DropDown :: line Province.Code}

data LanguageOfCorrespondence = English | French deriving (Bounded, Eq, Enum, Show)

data MaritalStatus = Married | LivingCommonLaw | Widowed | Divorced | Separated | Single
   deriving (Bounded, Eq, Enum, Show)

data Residence line = Residence {
   prov_DropDown :: line Text,
   prov_DropDown_Business :: line Province.Code,
   prov_DropDown_Residence :: line Province.Code,
   date_Departure :: line Day,
   date_Entry :: line Day}

data Spouse line = Spouse {
   line23600 :: line Centi,
   self_employment :: line Bool,
   spouse_First_Name :: line Text,
   line11700 :: line Centi,
   line21300 :: line Centi,
   sin :: line Text}

data Page2 line = Page2 {
   foreign_property :: line Bool,
   tax_exempt :: line Bool,
   electionsCanada :: ElectionsCanada line,
   cai :: line Bool,
   organ_donor :: line Bool}

data ElectionsCanada line = ElectionsCanada {
   citizenship :: line Bool,
   authorization :: line Bool
   }

data Page3 line = Page3 {
   line_10400_OtherEmploymentIncome :: line Centi,
   line_12100_InvestmentIncome :: line Centi,
   line_11300_OldAgeSecurityPension :: line Centi,
   line_11500_OtherPensions :: line Centi,
   line_13000_OtherIncome :: line Centi,
   line_13000_OtherIncomeSource :: line Text,
   line_11410_DisabilityBenefits :: line Centi,
   line_14400_WorkersCompBen :: line Centi,
   line_11400_CPP_QPP :: line Centi,
   line_25_sum :: line Centi,
   line_25_cont :: line Centi,
   line_11700_UCCB :: line Centi,
   line_10100_EmploymentIncome :: line Centi,
   line_12700_TaxableCapitalGains :: line Centi,
   line_13700_Amount :: line Centi,
   line_13699_Amount :: line Centi,
   line_11900_EmploymentInsurance :: line Centi,
   line_14100_Amount :: line Centi,
   line_14099_Amount :: line Centi,
   line_10120_Commissions :: line Centi,
   line_11600_ElectedSplitPension :: line Centi,
   line_14700_EqualsAmount :: line Centi,
   line_14700_PlusAmount :: line Centi,
   line_13010_Taxablescholarship :: line Centi,
   line_19 :: line Centi,
   line_12599_12600_RentalIncome :: line Centi,
   line_12600_Amount :: line Centi,
   line_12200_PartnershipIncome :: line Centi,
   line_11905_Employmentmaternity :: line Centi,
   line_14500_SocialAssistPay :: line Centi,
   line_15000_TotalIncome :: line Centi,
   line_10105_Taxexemptamount :: line Centi,
   line_12900_RRSPIncome :: line Centi,
   line_12500_RDSP :: line Centi,
   line_26 :: line Centi,
   line_12000_TaxableDividends :: line Centi,
   line_14600_NetFedSupplements :: line Centi,
   line_10130_sf :: line Centi,
   line_12010_OtherTaxableDividends :: line Centi,
   line_11701_UCCBDesignated :: line Centi,
   line_13499_Amount :: line Centi,
   line_13500_Amount :: line Centi,
   line_12800_Amount :: line Centi,
   line_12799_Amount :: line Centi,
   line_14299_Amount :: line Centi,
   line_14300_Amount :: line Centi,
   line_13900_Amount :: line Centi,
   line_13899_Amount :: line Centi}

data Page4 line = Page4 {
   line_21000_SplitPensionDeduction :: line Centi,
   line_23500_SocialBenefits :: line Centi,
   line_23400_NetBeforeAdjust :: line Centi,
   line_22100_CarryingChargesInterest :: line Centi,
   line_21500_DisabilityDeduction :: line Centi,
   line_15000_TotalIncome_2 :: line Centi,
   line_22900_OtherEmployExpenses :: line Centi,
   line_20800_RRSPDeduction :: line Centi,
   line_22000_Amount :: line Centi,
   line_21999_Amount :: line Centi,
   line_21699_Amount :: line Centi,
   line_21700_Amount :: line Centi,
   line_23210 :: line Centi,
   line_20810_PRPP :: line Centi,
   line_20700_RPPDeduction :: line Centi,
   line_22215_DeductionCPP_QPP :: line Centi,
   line_23300_sum :: line Centi,
   line_23300_cont :: line Centi,
   line_23600_NetIncome :: line Centi,
   line_21200_Dues :: line Centi,
   line_20600_PensionAdjustment :: line Centi,
   line_22400_XplorationDevExpenses :: line Centi,
   line_23200_OtherDeductions :: line Centi,
   line_23200_Specify :: line Text,
   line_21900_MovingExpenses :: line Centi,
   line_21400_ChildCareExpenses :: line Centi,
   line_22200_CPP_QPP_Contributions :: line Centi,
   line_21300_UCCBRepayment :: line Centi,
   line_23100_ClergyResDeduction :: line Centi}

data Page5 line = Page5 {
   step4_TaxableIncome :: Step4 line,
   partA_FederalTax :: Page5PartA line,
   partB_FederalTaxCredits :: Page5PartB line}

data Step4 line = Step4 {
   line_25000_OtherPayDeductions :: line Centi,
   line_25100_PartnershipLosses :: line Centi,
   line_25600_AdditionalDeductions_Specify :: line Text,
   line_25600_AdditionalDeductions_Amount :: line Centi,
   line_25300_NetCapitalLosses :: line Centi,
   line_25200_NoncapitalLosses :: line Centi,
   line_24400_MilitaryPoliceDeduction :: line Centi,
   line_26000_TaxableIncome :: line Centi,
   line_23600_NetIncome_2 :: line Centi,
   line_25400_CapitalGainsDeduction :: line Centi,
   line_24900_SecurityDeductions :: line Centi,
   line_25500_NorthernDeductions :: line Centi,
   line_25700_AddLines_sum :: line Centi,
   line_25700_AddLines_cont :: line Centi}

data Page5PartA line = Page5PartA {
   column1 :: TaxIncomeBracket line,
   column2 :: TaxIncomeBracket line,
   column3 :: TaxIncomeBracket line,
   column4 :: TaxIncomeBracket line,
   column5 :: TaxIncomeBracket line}

data Page5PartB line = Page5PartB {
   line30000 :: line Centi,
   line30100 :: line Centi,
   line30300 :: line Centi,
   line30400 :: line Centi,
   line30425 :: line Centi,
   line30450 :: line Centi,
   line30499_ChildrenNum :: line Word,
   line30500 :: line Centi,
   line_81 :: line Centi}

data TaxIncomeBracket line = TaxIncomeBracket {
   line67_income :: line Centi,
   line69_overThreshold :: line Centi,
   line71_timesRate :: line Centi,
   line73_equalsTax :: line Centi
   }

data Page6 line = Page6 {
   -- CPP_QPP
   line30800 :: line Centi,
   line31000 :: line Centi,
   -- EI
   line31200 :: line Centi,
   line31217 :: line Centi,
   line31220 :: line Centi,
   line31240 :: line Centi,
   line31260 :: line Centi,
   line31270 :: line Centi,
   line31285 :: line Centi,
   line31300 :: line Centi,
   line31350 :: line Centi,
   line94_sum :: line Centi,
   line94_cont :: line Centi,
   line31400 :: line Centi,
   line96 :: line Centi,
   line31600 :: line Centi,
   line31800 :: line Centi,
   line99 :: line Centi,
   line31900 :: line Centi,
   line32300 :: line Centi,
   line32400 :: line Centi,
   line32600 :: line Centi,
   line104 :: line Centi,
   medical_expenses :: MedicalExpenses line,
   line33200_sum :: line Centi,
   line33200_cont :: line Centi,
   line33500 :: line Centi,
   line33800 :: line Centi,
   line_79 :: line Centi,
   line34900 :: line Centi,
   line35000 :: line Centi}

data MedicalExpenses line = MedicalExpenses {
   familyExpenses :: line Centi,
   taxableIncome :: line Centi,
   taxableIncomeFraction :: line Centi,
   threshold :: line Centi,
   difference :: line Centi,
   otherDependants :: line Centi}

data Page7 line = Page7 {
   partC_NetFederalTax :: Page7PartC line,
   step6_RefundOrBalanceOwing :: Page7Step6 line}

data Page7PartC line = Page7PartC {
   line116 :: line Centi,
   line40424 :: line Centi,
   line40400 :: line Centi,
   line119 :: line Centi,
   line40425 :: line Centi,
   line40427 :: line Centi,
   line122_sum :: line Centi,
   line122_cont :: line Centi,
   line123 :: line Centi,
   line124 :: line Centi,
   line125 :: line Centi,
   line40500 :: line Centi,
   line128 :: line Centi,
   line129 :: line Centi,
   line130 :: line Centi,
   line40600 :: line Centi,
   line40900 :: line Centi,
   line41000 :: line Centi,
   line41200 :: line Centi,
   line41300 :: line Centi,
   line41400 :: line Centi,
   line41600_sum :: line Centi,
   line41600_cont :: line Centi,
   line41700 :: line Centi,
   line41500 :: line Centi,
   line41800 :: line Centi,
   line42000 :: line Centi}

data Page7Step6 line = Page7Step6 {
   line140 :: line Centi,
   line_42100_CPPContributions :: line Centi,
   line_42120_EIPremiums :: line Centi,
   line_42200_SocialBenefits :: line Centi,
   line_42800_ProvTerrTax :: line Centi,
   line_43500_TotalPayable :: line Centi}

data Page8 line = Page8 {
   step6_RefundOrBalanceOwing :: Page8Step6 line,
   line48400_Refund :: line Centi,
   line48500_BalanceOwing :: line Centi,
   telephone :: line Centi,
   date :: line Centi,
   taxPreparer :: TaxPreparer line,
   line_1_ONOpportunitiesFund :: line Centi,
   line_46500 :: line Centi,
   line_46600 :: line Centi}

data Page8Step6 line = Page8Step6 {
   line_43500_totalpayable :: line Centi,
   line_43700_Total_income_tax_ded :: line Centi,
   line_44000Sub :: line Centi,
   line_44800_CPPOverpayment :: line Centi,
   line_45000_EIOverpayment :: line Centi,
   line_45200_MedicalExpense :: line Centi,
   line_45300_CWB :: line Centi,
   line_45350_CTC :: line Centi,
   line_45400_InvestmentTaxCredit :: line Centi,
   line_45600_TrustTaxCredit :: line Centi,
   line_45700_GST_HST_Rebate :: line Centi,
   line_46800 :: line Centi,
   line_46900 :: line Centi,
   line_47555_TaxPaid :: line Centi,
   line_47556 :: line Centi,
   line_47557 :: line Centi,
   line_47600_TaxPaid :: line Centi,
   line_47900_ProvTerrCredits :: line Centi,
   line_48200_sum :: line Centi,
   line_48200_cont :: line Centi,
   line164_Refund_or_BalanceOwing :: line Centi}

data TaxPreparer line = TaxPreparer {
   eFileNumber :: line Text,
   nameOfPreparer :: line Text,
   telephoneOfPreparer :: line Text,
   line49000_WasAFeeCharged :: line Bool}

$(foldMap
   (\t-> [d|
           deriving instance (Show (line Bool), Show (line Centi), Show (line Word), Show (line Text),
                              Show (line Province.Code), Show (line Day),
                              Show (line LanguageOfCorrespondence), Show (line MaritalStatus))
                          => Show ($(TH.conT t) line)
           deriving instance (Eq (line Bool), Eq (line Centi), Eq (line Word), Eq (line Text),
                              Eq (line Province.Code), Eq (line Day),
                              Eq (line LanguageOfCorrespondence), Eq (line MaritalStatus))
                          => Eq ($(TH.conT t) line)
    |])
   [''T1, ''ElectionsCanada, ''Identification, ''MedicalExpenses,
    ''Page1, ''Page2, ''Page3, ''Page4, ''Page5, ''Page6, ''Page7, ''Page8,
    ''Step4, ''Page5PartA, ''Page5PartB, ''Page7PartC, ''Page7Step6, ''Page8Step6,
    ''Residence, ''Spouse, ''TaxIncomeBracket, ''TaxPreparer])

$(foldMap Rank2.TH.deriveAll
   [''T1, ''ElectionsCanada, ''Identification, ''MedicalExpenses,
    ''Page1, ''Page2, ''Page3, ''Page4, ''Page5, ''Page6, ''Page7, ''Page8,
    ''Step4, ''Page5PartA, ''Page5PartB, ''Page7PartC, ''Page7Step6, ''Page8Step6,
    ''Residence, ''Spouse, ''TaxIncomeBracket, ''TaxPreparer])
