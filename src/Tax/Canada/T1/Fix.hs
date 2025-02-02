{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tax.Canada.T1.Fix (T1, fixT1) where

import Control.Applicative ((<|>))
import Data.Fixed (Centi)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Calendar (Year, dayPeriod)
import GHC.Stack (HasCallStack)
import Rank2 qualified

import Tax.Canada.T1.Types
import Tax.Canada.Shared (fixSubCalculation, fixTaxIncomeBracket, SubCalculation(result), TaxIncomeBracket (equalsTax))
import Tax.Util (difference, fixEq, fractionOf, nonNegativeDifference, totalOf)

fixT1 :: HasCallStack => T1 Maybe -> T1 Maybe
fixT1 = fixEq $ \t1@T1{..}-> T1{page1 = fixPage1 page1,
                                page2 = fixPage2 page2,
                                page3 = fixPage3 page3,
                                page4 = fixPage4 t1 page4,
                                page5 = fixPage5 t1 page5,
                                page6 = fixPage6 t1 page6,
                                page7 = fixPage7 t1 page7,
                                page8 = fixPage8 t1 page8}

fixPage1 :: Page1 Maybe -> Page1 Maybe
fixPage1 = id

fixPage2 :: Page2 Maybe -> Page2 Maybe
fixPage2 = id

fixPage3 :: Page3 Maybe -> Page3 Maybe
fixPage3 = fixEq $ \page@Page3{selfEmployment=SelfEmploymentIncome{..}, ..}-> page{
   line16_difference = fixSubCalculation id $
                       nonNegativeDifference line_12700_TaxableCapitalGains line_12701_CapitalGainsReduction,
   line23_sum = totalOf [line_10100_EmploymentIncome ,
                         line_10400_OtherEmploymentIncome,
                         line_11300_OldAgeSecurityPension,
                         line_11400_CPP_QPP,
                         line_11500_OtherPensions,
                         line_11600_ElectedSplitPension,
                         line_11700_UCCB,
                         line_11900_EmploymentInsurance,
                         line_12000_TaxableDividends,
                         line_12100_InvestmentIncome,
                         line_12200_PartnershipIncome,
                         line_12500_RDSP,
                         line_12600_Amount,
                         line16_difference.result,
                         line_12800_Amount,
                         line_12900_RRSPIncome,
                         line_12905_FHSAIncome,
                         line_12906_OtherFHSAIncome,
                         line_13000_OtherIncome,
                         line_13010_TaxableScholarship],
   line29_sum = fixSubCalculation id $
                totalOf [line_13500_Amount,
                         line_13700_Amount,
                         line_13900_Amount,
                         line_14100_Amount,
                         line_14300_Amount],
   line30_sum = totalOf [line23_sum, line29_sum.result],
   line_14700_sum = fixSubCalculation id $
                    totalOf [line_14400_WorkersCompBen,
                             line_14500_SocialAssistPay,
                             line_14600_NetFedSupplements],
   line_15000_TotalIncome = totalOf [line30_sum, line_14700_sum.result]}

fixPage4 :: T1 Maybe -> Page4 Maybe -> Page4 Maybe
fixPage4 t1 = fixEq $ \page@Page4{..}-> page{
   line_15000_TotalIncome_2 = t1.page3.line_15000_TotalIncome,
   line_23300_sum = fixSubCalculation id $
                    totalOf [line_20700_RPPDeduction,
                             line_20800_RRSPDeduction,
                             line_20805_FHSADeduction,
                             line_21000_SplitPensionDeduction,
                             line_21200_Dues,
                             line_21300_UCCBRepayment,
                             line_21400_ChildCareExpenses,
                             line_21500_DisabilityDeduction,
                             line_21700_Amount,
                             line_21900_MovingExpenses,
                             line_22000_Amount,
                             line_22100_CarryingChargesInterest,
                             line_22200_CPP_QPP_Contributions,
                             line_22215_DeductionCPP_QPP,
                             line_22300_DeductionPPIP,
                             line_22400_XplorationDevExpenses,
                             line_22900_OtherEmployExpenses,
                             line_23100_ClergyResDeduction,
                             line_23200_OtherDeductions],
   line_23400_NetBeforeAdjust = nonNegativeDifference line_15000_TotalIncome_2 line_23300_sum.result,
   line_23600_NetIncome = nonNegativeDifference line_23400_NetBeforeAdjust line_23500_SocialBenefits}

fixPage5 :: HasCallStack => T1 Maybe -> Page5 Maybe -> Page5 Maybe
fixPage5 t1 = fixEq $ \Page5{..}-> Page5{
   step4_TaxableIncome = fixStep4 t1 step4_TaxableIncome,
   partA_FederalTax = fixPage5PartA t1 partA_FederalTax,
   partB_FederalTaxCredits = fixPage5PartB t1 partB_FederalTaxCredits}

fixPage6 :: T1 Maybe -> Page6 Maybe -> Page6 Maybe
fixPage6 t1 = fixEq $ \page@Page6{..}-> page{
   pageBreakCarry = t1.page5.partB_FederalTaxCredits.pageBreakSummary,
   line_31260 = minimum [Just 1368,
                        totalOf [t1.page3.line_10100_EmploymentIncome, t1.page3.line_10400_OtherEmploymentIncome]],
   line102_sum = fixSubCalculation id $
                 totalOf [line_30800,
                          line_31000,
                          line_31200,
                          line_31205,
                          line_31210,
                          line_31215,
                          line_31217,
                          line_31220,
                          line_31240,
                          line_31260,
                          line_31270,
                          line_31285,
                          line_31300,
                          line_31350],
   line104_sum = totalOf [pageBreakCarry, line102_sum.result, line_31400],
   line107_sum = totalOf [line104_sum, line_31600, line_31800],
   line112_sum = totalOf [line107_sum, line_31900, line_32300, line_32400, line_32600],
   medical_expenses = fixMedicalExpenses t1 medical_expenses,
   line_33200_sum = fixSubCalculation id $ totalOf [medical_expenses.difference, medical_expenses.otherDependants],
   line_33500 = totalOf [line112_sum, line_33200_sum.result],
   line_33800 = line120_taxCreditRate `fractionOf` line_33500,
   line_35000 = totalOf [line_33800, line_34900]}

fixPage7 :: T1 Maybe -> Page7 Maybe -> Page7 Maybe
fixPage7 t1 = fixEq $ \Page7{partC_NetFederalTax, step6_RefundOrBalanceOwing}-> Page7{
   partC_NetFederalTax = fixPage7PartC t1 partC_NetFederalTax,
   step6_RefundOrBalanceOwing = fixPage7Step6 t1 step6_RefundOrBalanceOwing}

fixPage8 :: T1 Maybe -> Page8 Maybe -> Page8 Maybe
fixPage8 t1 = fixEq $ \page@Page8{..}-> Page8{
   step6_RefundOrBalanceOwing = fixPage8Step6 t1 step6_RefundOrBalanceOwing,
   line_48400_Refund = step6_RefundOrBalanceOwing.line164_Refund_or_BalanceOwing
                      >>= \x-> if x < 0 then Just (negate x) else Nothing,
   line_48500_BalanceOwing = step6_RefundOrBalanceOwing.line164_Refund_or_BalanceOwing
                            >>= \x-> if x > 0 then Just x else Nothing,
   line_46600 = (-) <$> line_48400_Refund <*> line_46500,
   ..}

fixStep4 :: T1 Maybe -> Step4 Maybe -> Step4 Maybe
fixStep4 t1 = fixEq $ \step@Step4{..}-> step{
   line_23600_NetIncome_2 = t1.page4.line_23600_NetIncome,
   line_25700_sum = fixSubCalculation id $
                    totalOf [line_24400_MilitaryPoliceDeduction,
                             line_24900_SecurityDeductions,
                             line_24901_SecurityDeductions,
                             line_25000_OtherPayDeductions,
                             line_25100_PartnershipLosses,
                             line_25200_NoncapitalLosses,
                             line_25300_NetCapitalLosses,
                             line_25395_BusinessTransfer,
                             line_25400_CapitalGainsDeduction,
                             line_25500_NorthernDeductions,
                             line_25600_AdditionalDeductions_Amount],
   line72_difference = nonNegativeDifference line_23600_NetIncome_2 line_25700_sum.result,
   line_26000_TaxableIncome = totalOf [line72_difference, line_25999_CapitalGainsReductionAddBack]}

fixPage5PartA :: HasCallStack => T1 Maybe -> Page5PartA Maybe -> Page5PartA Maybe
fixPage5PartA t1 = fixEq $ \part@Page5PartA{..}-> part{
   column1 = fixTaxIncomeBracket income (Just part.column2) part.column1,
   column2 = fixTaxIncomeBracket income (Just part.column3) part.column2,
   column3 = fixTaxIncomeBracket income (Just part.column4) part.column3,
   column4 = fixTaxIncomeBracket income (Just part.column5) part.column4,
   column5 = fixTaxIncomeBracket income Nothing             part.column5}
   where income = t1.page5.step4_TaxableIncome.line_26000_TaxableIncome

fixPage5PartB :: T1 Maybe -> Page5PartB Maybe -> Page5PartB Maybe
fixPage5PartB t1 = fixEq $ \part@Page5PartB{..}-> part{
   line_30000 = let income = fromMaybe 0 t1.page4.line_23600_NetIncome
                    threshold = 165_430
                    ceiling = 235_675
                in if income <= threshold then Just 15_000
                   else if income >= ceiling then Just 13_520
                        else Just $ 15_000 - (15_000 - 13_250) * (income - threshold) / (ceiling - threshold),
   line_30100 = if any ((<= (1958 :: Year)) . dayPeriod) t1.page1.identification.dateBirth
                then let income = fromMaybe 0 t1.page4.line_23600_NetIncome
                         threshold = 42_335
                         ceiling = 98_309
                     in if income <= threshold then Just 8396
                        else if income >= ceiling then Just 0
                             else Just (8396 - (income - threshold) * 0.15)
                else Nothing,
   line_30500 =  ((* 2499) . fromIntegral) <$> line_30499_ChildrenNum,
   pageBreakSummary = totalOf [line_30000,
                               line_30100,
                               line_30300,
                               line_30400,
                               line_30425,
                               line_30450,
                               line_30500]}

fixMedicalExpenses :: T1 Maybe -> MedicalExpenses Maybe -> MedicalExpenses Maybe
fixMedicalExpenses t1 = fixEq $ \expenses@MedicalExpenses{familyExpenses, taxableIncome,
                                                          taxableIncomeFraction, threshold}-> expenses{
   taxableIncome = t1.page4.line_23600_NetIncome,
   taxableIncomeFraction = (* 0.03) <$> taxableIncome,
   threshold = min 2635 <$> taxableIncomeFraction,
   difference = nonNegativeDifference familyExpenses threshold}

fixPage7PartC :: T1 Maybe -> Page7PartC Maybe -> Page7PartC Maybe
fixPage7PartC t1 = fixEq $ \part@Page7PartC{..}-> part{
   tax_copy = let partA = t1.page5.partA_FederalTax
              in partA.column1.equalsTax
                 <|> partA.column2.equalsTax
                 <|> partA.column3.equalsTax
                 <|> partA.column4.equalsTax
                 <|> partA.column5.equalsTax,
   line_40400 = totalOf [tax_copy, line_40424],
   credits_copy = t1.page6.line_35000,
   line_40425,
   line_40427,
   line130_sum = fixSubCalculation id $ totalOf [credits_copy, line_40425, line_40427],
   line_42900 = nonNegativeDifference line_40400 line130_sum.result,
   line133_sum = totalOf [line_42900, line132_foreignSurtax],
   line135_difference = difference line133_sum line_40500,
   line137_sum = totalOf [line135_difference, line136_recapture],
   line_40600 = nonNegativeDifference line137_sum line138_logging,
   line_41000 = case line_40900
               of Just x
                    | x <=  400 -> Just (x * 0.75)
                    | x <=  750 -> Just ((x - 400) * 0.5 + 300)
                    | x <= 1275 -> Just ((x - 750) * 0.3333 + 475)
                    | otherwise -> Just 650
                  Nothing -> Nothing,
   line_41600_sum = fixSubCalculation id $ totalOf [line_41000, line_41200, line_41400],
   line_41700 = nonNegativeDifference line_40600 line_41600_sum.result,
   line_42000 = totalOf [line_41700, line_41500, line_41800]}

fixPage7Step6 :: T1 Maybe -> Page7Step6 Maybe -> Page7Step6 Maybe
fixPage7Step6 t1 = fixEq $ \step@Page7Step6{..}-> step{
   tax_copy = t1.page7.partC_NetFederalTax.line_42000,
   line_43500_TotalPayable = totalOf [tax_copy, line_42100_CPPContributions, line_42120_EIPremiums,
                                      line_42200_SocialBenefits, line_42800_ProvTerrTax]}

fixPage8Step6 :: T1 Maybe -> Page8Step6 Maybe -> Page8Step6 Maybe
fixPage8Step6 t1 = fixEq $ \step@Page8Step6{..}-> step{
   line_43500_totalpayable = t1.page7.step6_RefundOrBalanceOwing.line_43500_TotalPayable,
   line_46900 = (* 0.25) <$> line_46800,
   line_43850_diff = fixSubCalculation id $ difference line_43700_Total_income_tax_ded line_43800_TaxTransferQC,
   line_31210_copy = t1.page6.line_31210,
   line_45100_diff = fixSubCalculation id $ difference line_45000_EIOverpayment line_31210_copy,
   line_48200_sum = fixSubCalculation id $
                    totalOf [line_43850_diff.result,
                             line_44000,
                             line_45100_diff.result,
                             line_44800_CPPOverpayment,
                             line_45000_EIOverpayment,
                             line_45200_MedicalExpense,
                             line_45300_CWB,
                             line_45350_CTC,
                             line_45355_MHRTC,
                             line_45400_InvestmentTaxCredit,
                             line_45600_TrustTaxCredit,
                             line_45700_GST_HST_Rebate,
                             line_46900,
                             line_47555_TaxPaid,
                             line_47556,
                             line_47557,
                             line_47600_TaxPaid,
                             line_47900_ProvTerrCredits],
   line164_Refund_or_BalanceOwing = difference line_43500_totalpayable line_48200_sum.result}
