{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Tax.Canada.Federal.Schedule6 where

import Data.Fixed (Centi)
import Language.Haskell.TH qualified as TH
import Rank2 qualified
import Rank2.TH qualified
import Transformation.Shallow.TH qualified

import Tax.Canada.Shared (SubCalculation(result), fixSubCalculation, subCalculationFields)
import Tax.Canada.T1.Types (T1)
import Tax.Canada.T1.Types qualified as T1
import Tax.FDF (Entry (Amount, Constant, Percent, Switch'), FieldConst (Field), within)
import Tax.Util (fixEq, fractionOf, difference, nonNegativeDifference, totalOf)

data Schedule6 line = Schedule6{
   page2 :: Page2 line,
   page3 :: Page3 line,
   page4 :: Page4 line,
   page5 :: Page5 line}

data Page2 line = Page2{
   questions :: Questions line,
   partA_self :: PartAColumn line,
   partA_spouse :: PartAColumn line,
   line6_sum :: line Centi}

data Questions line = Questions{
   line_38100 :: line Bool,
   line_38101 :: line Bool,
   line_38102 :: line Bool,
   line_38103 :: line Bool,
   line_38104 :: line Bool,
   line_38105 :: line Bool}

data PartAColumn line = PartAColumn{
   line1_employmentIncome_copy :: line Centi,
   line_38106_grants_copy :: line Centi,
   line3_selfEmploymentIncome_sum :: line Centi,
   line_38107_exemptIncome :: line Centi,
   line_38108_sum :: line Centi}

data Page3 line = Page3{
   partB_self :: PartBColumn line,
   partB_spouse :: PartBColumn line,
   line13_sum :: line Centi,
   line14_least :: line Centi,
   line15_difference :: line Centi}

data PartBColumn line = PartBColumn{
   line7_netIncome_copy :: line Centi,
   line_38109_exempt :: line Centi,
   line9_uccbRdspRepayments :: line Centi,
   line10_sum :: line Centi,
   line11_uccbRdspIncome :: line Centi,
   line_38110_difference :: line Centi}

data Page4 line = Page4{
   step2 :: Step2 line,
   step3 :: Step3 line}

data Step2 line = Step2{
   line16_copy :: line Centi,
   line17_threshold :: line Centi,
   line18_difference :: line Centi,
   line19_rate :: line Rational,
   line20_fraction :: line Centi,
   line21_ceiling :: line Centi,
   line22_least :: line Centi,
   line23_copy :: line Centi,
   line24_threshold :: line Centi,
   line25_difference :: line Centi,
   line26_rate :: line Rational,
   line27_fraction :: SubCalculation line,
   line28_difference :: line Centi}

data Step3 line = Step3{
   line29_copy :: line Centi,
   line30_threshold :: line Centi,
   line31_difference :: line Centi,
   line32_rate :: line Rational,
   line33_fraction :: line Centi,
   line34_capped :: line Centi,
   line35_copy :: line Centi,
   line36_threshold :: line Centi,
   line37_difference :: line Centi,
   line38_rate :: line Rational,
   line39_fraction :: SubCalculation line,
   line40_difference :: line Centi,
   line41_copy :: line Centi,
   line42_sum :: line Centi}

data Page5 line = Page5{
   line43_greatest :: line Centi,
   line_38120_acwb_self :: line Centi,
   line_38121_acwb_spouse :: line Centi,
   line46_sum :: line Centi,
   line_38122_acwb_disability :: line Centi,
   line48_sum :: line Centi,
   line49_least :: line Centi}

$(foldMap
   (\t-> concat <$> sequenceA [
       [d|
           deriving instance (Show (line Bool), Show (line Centi), Show (line Rational)) => Show ($(TH.conT t) line)
           deriving instance (Eq (line Bool), Eq (line Centi), Eq (line Rational)) => Eq ($(TH.conT t) line)
       |],
       Rank2.TH.deriveAll t,
       Transformation.Shallow.TH.deriveAll t])
   [''Schedule6, ''Page2, ''Page3, ''Page4, ''Page5, ''Questions, ''PartAColumn, ''PartBColumn, ''Step2, ''Step3])

fixSchedule6 :: Maybe (T1 Maybe) -> T1 Maybe -> Schedule6 Maybe -> Schedule6 Maybe
fixSchedule6 t1spouse t1  =
   fixEq $ \Schedule6{page2, page3, page4=Page4{step2, step3}, page5} ->
            let eitherEligible = or page2.questions.line_38100 || or page2.questions.line_38101 in Schedule6{
   page2 = Page2{
      questions = page2.questions,
      partA_self = fixPartAColumn t1 page2.partA_self,
      partA_spouse = maybe id fixPartAColumn t1spouse page2.partA_spouse,
      line6_sum = totalOf [page2.partA_self.line_38108_sum, page2.partA_spouse.line_38108_sum]},
   page3 = let Page3{..} = page3 in Page3{
      partB_self = fixPartBColumn t1 partB_self,
      partB_spouse = maybe id fixPartBColumn t1spouse partB_spouse,
      line13_sum = totalOf [partB_self.line_38110_difference, partB_spouse.line_38110_difference],
      line14_least = min 15_955 <$>
                     if or ((<) <$> page2.partA_self.line_38108_sum <*> page2.partA_spouse.line_38108_sum)
                     then min page2.partA_self.line_38108_sum partB_self.line_38110_difference
                     else min page2.partA_spouse.line_38108_sum partB_spouse.line_38110_difference,
      line15_difference = difference line13_sum line14_least},
   page4 = Page4{
      step2 = if all not page2.questions.line_38102 then Rank2.pure Nothing else let Step2{..} = step2 in step2{
         line16_copy = page2.line6_sum,
         line18_difference = nonNegativeDifference line16_copy line17_threshold,
         line20_fraction = line19_rate `fractionOf` line18_difference,
         line21_ceiling = if eitherEligible then Just 2_739 else Just 1_590,
         line22_least = min line20_fraction line21_ceiling,
         line23_copy = page3.line15_difference,
         line24_threshold = if eitherEligible then Just 29_833 else Just 26_149,
         line25_difference = nonNegativeDifference line23_copy line24_threshold,
         line27_fraction = fixSubCalculation id $ line26_rate `fractionOf` line25_difference,
         line28_difference = nonNegativeDifference line22_least line27_fraction.result},
      step3 = if all not page2.questions.line_38103 then Rank2.pure Nothing else let Step3{..} = step3 in step3{
         line29_copy = page2.partA_self.line_38108_sum,
         line31_difference = nonNegativeDifference line29_copy line30_threshold,
         line33_fraction = line32_rate `fractionOf` line31_difference,
         line34_capped = min 821 <$> line33_fraction,
         line35_copy = page3.line15_difference,
         line36_threshold = if eitherEligible then Just 48_091 else Just 36_748,
         line37_difference = nonNegativeDifference line35_copy line36_threshold,
         line38_rate = if or page2.questions.line_38104 then Just 0.075 else Just 0.15,
         line39_fraction = fixSubCalculation id $ line38_rate `fractionOf` line37_difference,
         line40_difference = nonNegativeDifference line34_capped line39_fraction.result,
         line41_copy = if or page2.questions.line_38102 then step2.line28_difference else Just 0,
         line42_sum = totalOf [line40_difference, line41_copy]}},
   page5 = let Page5{..} = page5 in page5{
      line43_greatest = max step2.line28_difference step3.line42_sum,
      line46_sum = totalOf [line_38120_acwb_self, line_38121_acwb_spouse],
      line48_sum = totalOf [line46_sum, line_38122_acwb_disability],
      line49_least = min line43_greatest line48_sum}}

fixPartAColumn :: T1 Maybe -> PartAColumn Maybe -> PartAColumn Maybe
fixPartAColumn t1 PartAColumn{..} = PartAColumn{
   line1_employmentIncome_copy =
      totalOf [t1.page3.line_10100_EmploymentIncome, t1.page3.line_10400_OtherEmploymentIncome],
   line_38106_grants_copy = t1.page3.line_13010_TaxableScholarship,
   line3_selfEmploymentIncome_sum =
      let T1.SelfEmploymentIncome{..} = t1.page3.selfEmployment
      in totalOf [line_13500_Amount, line_13700_Amount, line_13900_Amount, line_14100_Amount, line_14300_Amount],
   line_38107_exemptIncome = line_38107_exemptIncome,
   line_38108_sum = totalOf [line1_employmentIncome_copy, line_38106_grants_copy,
                              line3_selfEmploymentIncome_sum, line_38107_exemptIncome]}

fixPartBColumn :: T1 Maybe -> PartBColumn Maybe -> PartBColumn Maybe
fixPartBColumn t1 column@PartBColumn{..} = column{
      line7_netIncome_copy = t1.page4.line_23600_NetIncome,
--      line9_uccbRdspRepayments = totalOf [t1.page4.line_21300_UCCBRepayment, t1.page4.line_23200],
      line10_sum = totalOf [line7_netIncome_copy, line_38109_exempt, line9_uccbRdspRepayments],
      line11_uccbRdspIncome = totalOf [t1.page3.line_11700_UCCB, t1.page3.line_12500_RDSP],
      line_38110_difference = nonNegativeDifference line10_sum line11_uccbRdspIncome}

schedule6Fields :: Schedule6 FieldConst
schedule6Fields = within "form1" Rank2.<$> Schedule6{
   page2 = within "Page2" Rank2.<$> Page2{
      questions = within "Step1_tickboxes" . within "Questions" Rank2.<$> Questions{
         line_38100 = Field ["Line38100", "Line38100_CheckBoxGroup"] $ Switch' "Line38100_CheckBox_EN",
         line_38101 = Field ["Line38101", "Line38101_CheckBoxGroup"] $ Switch' "Line38101_CheckBox_EN",
         line_38102 = Field ["Line38102", "Line38102_CheckBoxGroup"] $ Switch' "Line38102_CheckBox_EN",
         line_38103 = Field ["Line38103", "Line38103_CheckBoxGroup"] $ Switch' "Line38103_CheckBox_EN",
         line_38104 = Field ["Line38104", "Line38104_CheckBoxGroup"] $ Switch' "Line38104_CheckBox_EN",
         line_38105 = Field ["Line38105", "Line38105_CheckBoxGroup"] $ Switch' "Line38105_CheckBox_EN"},
      partA_self = within "PartA" . within "Chart" Rank2.<$> PartAColumn{
         line1_employmentIncome_copy = Field ["Line1", "Amount1", "Amount"] Amount,
         line_38106_grants_copy = Field ["Line2", "Amount1", "Amount"] Amount,
         line3_selfEmploymentIncome_sum = Field ["Line3", "Amount1", "Amount"] Amount,
         line_38107_exemptIncome = Field ["line4", "Amount1", "Amount"] Amount,
         line_38108_sum = Field ["Line5_Sub", "Amount1", "Amount"] Amount},
      partA_spouse = within "PartA" . within "Chart" Rank2.<$> PartAColumn{
         line1_employmentIncome_copy = Field ["Line1", "Amount2", "Amount"] Amount,
         line_38106_grants_copy = Field ["Line2", "Amount2", "Amount"] Amount,
         line3_selfEmploymentIncome_sum = Field ["Line3", "Amount2", "Amount"] Amount,
         line_38107_exemptIncome = Field ["line4", "Amount2", "Amount"] Amount,
         line_38108_sum = Field ["Line5_Sub", "Amount2", "Amount"] Amount},
      line6_sum = Field ["PartA", "Line6", "Amount"] Amount},
   page3 = within "Page3" . within "PartB" Rank2.<$> Page3{
      partB_self = within "Chart" Rank2.<$> PartBColumn{
         line7_netIncome_copy = Field ["Line7", "Amount1", "Amount"] Amount,
         line_38109_exempt = Field ["Line8", "Amount1", "Amount"] Amount,
         line9_uccbRdspRepayments = Field ["Line9", "Amount1", "Amount"] Amount,
         line10_sum = Field ["Line10", "Amount1", "Amount"] Amount,
         line11_uccbRdspIncome = Field ["Line11", "Amount1", "Amount"] Amount,
         line_38110_difference = Field ["Line12", "Amount1", "Amount"] Amount},
      partB_spouse = within "Chart" Rank2.<$> PartBColumn{
         line7_netIncome_copy = Field ["Line7", "Amount2", "Amount"] Amount,
         line_38109_exempt = Field ["Line8", "Amount2", "Amount"] Amount,
         line9_uccbRdspRepayments = Field ["Line9", "Amount2", "Amount"] Amount,
         line10_sum = Field ["Line10", "Amount2", "Amount"] Amount,
         line11_uccbRdspIncome = Field ["Line11", "Amount2", "Amount"] Amount,
         line_38110_difference = Field ["Line12", "Amount2", "Amount"] Amount},
      line13_sum = Field ["Line13", "Amount"] Amount,
      line14_least = Field ["Line14", "Amount"] Amount,
      line15_difference = Field ["Line15", "Amount"] Amount},
   page4 = within "Page4" Rank2.<$> Page4{
      step2 = within "Step2" Rank2.<$> Step2{
         line16_copy = Field ["Line16", "Amount"] Amount,
         line17_threshold = Field ["Line17", "Amount"] $ Constant 3_000 Amount,
         line18_difference = Field ["Line18", "Amount"] Amount,
         line19_rate = Field ["Line19", "Percent"] $ Constant 0.27 Percent,
         line20_fraction = Field ["Line20", "Amount"] Amount,
         line21_ceiling = Field ["Line21", "Amount"] Amount,
         line22_least = Field ["Line22", "Amount"] Amount,
         line23_copy = Field ["Line23", "Amount"] Amount,
         line24_threshold = Field ["Line24", "Amount"] Amount,
         line25_difference = Field ["Line25", "Amount"] Amount,
         line26_rate = Field ["Line26", "Percent"] $ Constant 0.15 Percent,
         line27_fraction = subCalculationFields "Line27" ["Amount1"] ["Amount2"],
         line28_difference = Field ["Line28", "Amount"] Amount},
      step3 = within "Step3" Rank2.<$> Step3{
         line29_copy = Field ["Line29", "Amount"] Amount,
         line30_threshold = Field ["Line30", "Amount"] $ Constant 1_150 Amount,
         line31_difference = Field ["Line31", "Amount"] Amount,
         line32_rate = Field ["Line32", "Percent"] $ Constant 0.27 Percent,
         line33_fraction = Field ["Line33", "Amount"] Amount,
         line34_capped = Field ["Line34", "Amount"] Amount,
         line35_copy = Field ["Line35", "Amount"] Amount,
         line36_threshold = Field ["Line36", "Amount"] Amount,
         line37_difference = Field ["Line37", "Amount"] Amount,
         line38_rate = Field ["Line38", "Percent"] Percent,
         line39_fraction = subCalculationFields "Line39" ["Amount1"] ["Amount2"],
         line40_difference = Field ["Line40", "Amount"] Amount,
         line41_copy = Field ["Line41", "Amount"] Amount,
         line42_sum = Field ["Line42", "Amount"] Amount}},
   page5 = within "Page5" . within "Step4" Rank2.<$> Page5{
      line43_greatest = Field ["Line43", "Amount"] Amount,
      line_38120_acwb_self = Field ["Line44", "Amount"] Amount,
      line_38121_acwb_spouse = Field ["Line45", "Amount"] Amount,
      line46_sum = Field ["Line46", "Amount"] Amount,
      line_38122_acwb_disability = Field ["Line47", "Amount"] Amount,
      line48_sum = Field ["Line48", "Amount"] Amount,
      line49_least = Field ["Line49", "Amount"] Amount}}

