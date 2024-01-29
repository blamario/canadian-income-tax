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
import Data.Text (Text)
import Language.Haskell.TH qualified as TH
import Rank2 qualified
import Rank2.TH qualified
import Transformation.Shallow.TH qualified

import Tax.Canada.T1.Types (T1)
import Tax.Canada.T1.Types qualified as T1
import Tax.FDF (Entry (Amount, Constant, Percent, Switch'), FieldConst (Field), within)
import Tax.Util (fixEq, fractionOf, difference, nonNegativeDifference, totalOf)

data Schedule6 line = Schedule6{
   page2 :: Page2 line,
   page3 :: Page3 line,
   page4 :: Page4 line}

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
   line27_fraction :: line Centi,
   line27_cont :: line Centi,
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
   line39_fraction :: line Centi,
   line39_cont :: line Centi,
   line40_difference :: line Centi,
   line41_copy :: line Centi,
   line42_sum :: line Centi}

$(foldMap
   (\t-> concat <$> sequenceA [
       [d|
           deriving instance (Show (line Bool), Show (line Centi), Show (line Rational)) => Show ($(TH.conT t) line)
           deriving instance (Eq (line Bool), Eq (line Centi), Eq (line Rational)) => Eq ($(TH.conT t) line)
       |],
       Rank2.TH.deriveAll t,
       Transformation.Shallow.TH.deriveAll t])
   [''Schedule6, ''Page2, ''Page3, ''Page4, ''Questions, ''PartAColumn, ''PartBColumn, ''Step2, ''Step3])

fixSchedule6 :: T1 Maybe -> T1 Maybe -> Schedule6 Maybe -> Schedule6 Maybe
fixSchedule6 t1 t1spouse =
   fixEq $ \Schedule6{page2, page3, page4=Page4{step2, step3}} ->
            let eitherEligible = or page2.questions.line_38100 || or page2.questions.line_38101 in Schedule6{
   page2 = Page2{
      questions = page2.questions,
      partA_self = fixPartAColumn t1 page2.partA_self,
      partA_spouse = fixPartAColumn t1spouse page2.partA_spouse},
   page3 = let Page3{..} = page3 in Page3{
      partB_self = fixPartBColumn t1 partB_self,
      partB_spouse = fixPartBColumn t1spouse partB_spouse,
      line13_sum = totalOf [partB_self.line_38110_difference, partB_spouse.line_38110_difference],
      line14_least = max 14_336 <$>
                     if or ((<) <$> page2.partA_self.line_38108_sum <*> page2.partA_spouse.line_38108_sum)
                     then min page2.partA_self.line_38108_sum partB_self.line_38110_difference
                     else min page2.partA_spouse.line_38108_sum partB_spouse.line_38110_difference,
      line15_difference = difference line13_sum line14_least},
   page4 = Page4{
      step2 = if all not page2.questions.line_38102 then Rank2.pure Nothing else let Step2{..} = step2 in step2{
         line16_copy = page2.line6_sum,
         line18_difference = nonNegativeDifference line16_copy line17_threshold,
         line20_fraction = line19_rate `fractionOf` line18_difference,
         line21_ceiling = if eitherEligible then Just 2_461 else Just 1_428,
         line22_least = max line20_fraction line21_ceiling,
         line23_copy = page3.line15_difference,
         line24_threshold = if eitherEligible then Just 26_805 else Just 23_495,
         line25_difference = nonNegativeDifference line23_copy line24_threshold,
         line27_fraction = line26_rate `fractionOf` line25_difference,
         line27_cont = line27_fraction,
         line28_difference = nonNegativeDifference line22_least line27_fraction},
      step3 = if all not page2.questions.line_38103 then Rank2.pure Nothing else let Step3{..} = step3 in step3{
         line29_copy = page2.partA_self.line_38108_sum,
         line31_difference = nonNegativeDifference line29_copy line30_threshold,
         line33_fraction = line32_rate `fractionOf` line31_difference,
         line34_capped = min 737 <$> line33_fraction,
         line35_copy = page3.line15_difference,
         line36_threshold = if eitherEligible then Just 43_210 else Just 33_018,
         line37_difference = nonNegativeDifference line35_copy line36_threshold,
         line38_rate = if or page2.questions.line_38104 then Just 0.075 else Just 0.15,
         line39_fraction = line38_rate `fractionOf` line37_difference,
         line39_cont = line39_fraction,
         line40_difference = nonNegativeDifference line34_capped line39_cont,
         line41_copy = if or page2.questions.line_38102 then step2.line28_difference else Just 0,
         line42_sum = totalOf [line40_difference, line41_copy]}}}

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
      questions = within "Questions" Rank2.<$> Questions{
         line_38100 = Field ["Line38100_Sub", "Line38100_CheckBoxGroup"] $ Switch' "Line38100_CheckBox_EN",
         line_38101 = Field ["Line38101_Sub", "Line38101_CheckBoxGroup"] $ Switch' "Line38101_CheckBox_EN",
         line_38102 = Field ["Line38102_Sub", "Line38102_CheckBoxGroup"] $ Switch' "Line38102_CheckBox_EN",
         line_38103 = Field ["Line38103_Sub", "Line38103_CheckBoxGroup"] $ Switch' "Line38103_CheckBox_EN",
         line_38104 = Field ["Line38104_Sub", "Line38104_CheckBoxGroup"] $ Switch' "Line38104_CheckBox_EN",
         line_38105 = Field ["Line38105_Sub"] $ Switch' "Line38105_CheckBox_EN"},
      partA_self = within "PartA_sub" Rank2.<$> PartAColumn{
         line1_employmentIncome_copy = Field ["Line1_Sub", "Line1_Amount1_Sub", "Line1Amount1"] Amount,
         line_38106_grants_copy = Field ["Line2_Sub", "Line2_Amount1_Sub", "Line2Amount1"] Amount,
         line3_selfEmploymentIncome_sum = Field ["Line3_Sub", "Line3_Amount1_Sub", "Line3Amount1"] Amount,
         line_38107_exemptIncome = Field ["line4_Sub", "Line4_Amount1_Sub", "Line4Amount1"] Amount,
         line_38108_sum = Field ["Line5_Sub", "Line5_Amount1_Sub", "Line5Amount1"] Amount},
      partA_spouse = within "PartA_sub" Rank2.<$> PartAColumn{
         line1_employmentIncome_copy = Field ["Line1_Sub", "Line1_Amount2_Sub", "Line1Amount2"] Amount,
         line_38106_grants_copy = Field ["Line2_Sub", "Line2_Amount2_Sub", "Line2Amount2"] Amount,
         line3_selfEmploymentIncome_sum = Field ["Line3_Sub", "Line3_Amount_Sub", "Line3Amount2"] Amount,
         line_38107_exemptIncome = Field ["line4_Sub", "Line4_Amount2_Sub", "Line4Amount2"] Amount,
         line_38108_sum = Field ["Line5_Sub", "Line5_Amount2_Sub", "Line5Amount2"] Amount},
      line6_sum = Field ["PartA_sub", "Line6_Sub", "Line6Amount"] Amount},
   page3 = within "Page3" . within "PartB" Rank2.<$> Page3{
      partB_self = PartBColumn{
         line7_netIncome_copy = Field ["Line7_Sub", "Line7_Amount1_Sub", "Line7_Amount1"] Amount,
         line_38109_exempt = Field ["Line8_Sub", "Line8_Amount_Sub", "Line8_Amount"] Amount,
         line9_uccbRdspRepayments = Field ["Line9_Sub", "Line9_Amount1_Sub", "Line9_Amount"] Amount,
         line10_sum = Field ["Line10_Sub", "LIne10_Amount1_Sub", "Line10_Amount"] Amount,
         line11_uccbRdspIncome = Field ["Line11_Sub", "Line11_Amount1_Sub", "Line11_Amount"] Amount,
         line_38110_difference = Field ["Line12_Sub", "Line12_Amount1_Sub", "Line12_Amount"] Amount},
      partB_spouse = PartBColumn{
         line7_netIncome_copy = Field ["Line7_Sub", "Line7_Amount2_Sub", "Line7_Amount"] Amount,
         line_38109_exempt = Field ["Line8_Sub", "Line8_AmountSub", "Line8_Amount"] Amount,
         line9_uccbRdspRepayments = Field ["Line9_Sub", "Line9_Amount2_Sub", "Line9_Amount"] Amount,
         line10_sum = Field ["Line10_Sub", "Line10_Amount2_Sub", "Line10_Amount"] Amount,
         line11_uccbRdspIncome = Field ["Line11_Sub", "Line11_Amount2_Sub", "Line11_Amount"] Amount,
         line_38110_difference = Field ["Line12_Sub", "Line12_Amount2_Sub", "Line12_Amount"] Amount},
      line13_sum = Field ["Line13_Sub", "Line13_Amount_Sub", "Line12_Amount"] Amount,
      line14_least = Field ["Line14_Sub", "LIne14_Amount_Sub", "Line14_Amount"] Amount,
      line15_difference = Field ["LIne15_Sub", "Line15_Amount"] Amount},
   page4 = within "Page4" Rank2.<$> Page4{
      step2 = within "Step2" Rank2.<$> Step2{
         line16_copy = Field ["Line16_Sub", "Line16Amount"] Amount,
         line17_threshold = Field ["Line17_Sub", "Line17Amount"] $ Constant 3_000 Amount,
         line18_difference = Field ["Line18_Sub", "Line18Amount"] Amount,
         line19_rate = Field ["Line19_Sub", "Line19Rate"] $ Constant 0.27 Percent,
         line20_fraction = Field ["Line20_Sub", "Line20Amount"] Amount,
         line21_ceiling = Field ["Line21_Sub", "Line21Amount"] Amount,
         line22_least = Field ["Line22_sub", "Line22Amount2"] Amount,
         line23_copy = Field ["Line23_Sub", "Line23Amount"] Amount,
         line24_threshold = Field ["Line24_Sub", "Line24Amount"] Amount,
         line25_difference = Field ["Line25_sub", "Line25Amount"] Amount,
         line26_rate = Field ["Line26_Sub", "Line26Rate"] $ Constant 0.15 Percent,
         line27_fraction = Field ["Line27_Sub", "Line27Amount1"] Amount,
         line27_cont = Field ["Line27_Sub", "Line27Amount2"] Amount,
         line28_difference = Field ["Line28_Sub", "Line28Amount"] Amount},
      step3 = within "Step3" Rank2.<$> Step3{
         line29_copy = Field ["Line29_Sub", "Line29Amount"] Amount,
         line30_threshold = Field ["Line30_Sub", "Line30Amount"] $ Constant 1_150 Amount,
         line31_difference = Field ["Line31_Sub", "Line31Amount"] Amount,
         line32_rate = Field ["Line32_Sub", "Line32Rate"] $ Constant 0.27 Percent,
         line33_fraction = Field ["Line33_Sub", "Line33Amount"] Amount,
         line34_capped = Field ["Line34_Sub", "Line34Amount2"] Amount,
         line35_copy = Field ["Line35_Sub", "Line35Amount"] Amount,
         line36_threshold = Field ["Line36_Sub", "Line36Amount"] Amount,
         line37_difference = Field ["Line37_Sub", "Line37Amount"] Amount,
         line38_rate = Field ["Line38_Sub", "Line38Rate"] Percent,
         line39_fraction = Field ["Line39_Sub", "Line39Amount1"] Amount,
         line39_cont = Field ["Line39_Sub", "Line39Amount2"] Amount,
         line40_difference = Field ["Line40_Sub", "Line40Amount"] Amount,
         line41_copy = Field ["Line41_Sub", "Line41Amount"] Amount,
         line42_sum = Field ["Line42_Sub", "Line42Amount"] Amount}}}

