{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Tax.Canada.Province.ON (Returns(..), ON428, ON479, formFileNames, fixON428, fixON479, fixReturns,
                               returnFields, t1Fields, on428Fields, on479Fields) where

import Data.CAProvinceCodes (Code(ON))
import Data.Map (Map, fromList)
import Data.Text (Text)
import Rank2 qualified
import Rank2.TH qualified
import Transformation.Shallow.TH qualified

import Tax.Canada.Federal qualified as Federal
import Tax.Canada.Federal (Forms(t1), fixFederalForms)
import Tax.Canada.Federal.Schedule9 qualified as Schedule9
import Tax.Canada.FormKey (FormKey)
import Tax.Canada.FormKey qualified as FormKey
import Tax.Canada.T1.Types (T1 (T1, page7, page8), Page7(Page7, step6_RefundOrBalanceOwing), Page8(Page8))
import Tax.Canada.T1.Types qualified as T1
import Tax.Canada.T1.Types qualified as Page8 (Page8(..))
import Tax.Canada.T1.FieldNames.ON (t1Fields)

import Tax.Canada.Province.ON.ON428.Types qualified as ON
import Tax.Canada.Province.ON.ON428.Types qualified as ON.Page1 (Page1(..))
import Tax.Canada.Province.ON.ON428.Types qualified as ON.Page2 (Page2(..))
import Tax.Canada.Province.ON.ON428.Types (ON428 (ON428))
import Tax.Canada.Province.ON.ON428.Fix (fixON428)
import Tax.Canada.Province.ON.ON428.FieldNames (on428Fields)
import Tax.Canada.Province.ON.ON479.Types (ON479 (page2), Page2(line23_credits))
import Tax.Canada.Province.ON.ON479.Fix (fixON479)
import Tax.Canada.Province.ON.ON479.FieldNames (on479Fields)

import Tax.Canada.Shared(MedicalExpenses(..), BaseCredit(..))
import Tax.FDF (FieldConst, within)
import Tax.Util (fixEq)

data Returns line = Returns {
  federal :: Federal.Forms line,
  on428 :: ON428 line,
  on479 :: ON479 line}

deriving instance (Show (Federal.Forms line), Show (ON428 line), Show (ON479 line)) => Show (Returns line)
deriving instance (Eq (Federal.Forms line), Eq (ON428 line), Eq (ON479 line)) => Eq (Returns line)
Rank2.TH.deriveFunctor ''Returns
Rank2.TH.deriveApply ''Returns
Rank2.TH.deriveApplicative ''Returns
Rank2.TH.deriveFoldable ''Returns
Rank2.TH.deriveTraversable ''Returns
Transformation.Shallow.TH.deriveAll ''Returns

fixReturns :: Federal.InputForms Maybe -> Returns Maybe -> Returns Maybe
fixReturns inputs =
  fixEq $ \Returns{federal = ff@Federal.Forms{t1 = t1@T1{page7 = page7@Page7{step6_RefundOrBalanceOwing},
                                                         page8 = page8@Page8{step6_RefundOrBalanceOwing = page8step6}},
                                              schedule9},
                   on428 = on428@ON428{page1 = page1@ON.Page1{partB = partB1@ON.Page1PartB{spouseAmount}},
                                       page2 = page2@ON.Page2{ON.partB = partB2@ON.Page2PartB{ON.medicalExpenses},
                                                              ON.partC}},
                   on479}
          -> Returns{federal = fixFederalForms ON inputs $
                               ff{t1 =
                                  t1{page7 =
                                     page7{step6_RefundOrBalanceOwing =
                                           step6_RefundOrBalanceOwing{T1.line_42800_ProvTerrTax= on428.page4.line90}},
                                     page8 =
                                     page8{Page8.step6_RefundOrBalanceOwing =
                                           page8step6{T1.line_47900_ProvTerrCredits = on479.page2.line23_credits}}}},
                     on428 = fixON428 on428{
                        ON.page1 =
                            page1{ON.line1 = t1.page5.step4_TaxableIncome.line_26000_TaxableIncome,
                                  ON.Page1.partB = partB1{ON.spouseAmount = spouseAmount{reduction = t1.page1.spouse.line_23600},
                                                          ON.line19_cppQpp = t1.page6.line_30800,
                                                          ON.line20_cppQpp = t1.page6.line_31000,
                                                          ON.line21_employmentInsurance = t1.page6.line_31200,
                                                          ON.line22_employmentInsurance = t1.page6.line_31217}},
                        ON.page2 =
                            page2{ON.Page2.partB = partB2{ON.line32_interest = t1.page6.line_31900,
                                                          ON.medicalExpenses =
                                                             medicalExpenses{
                                                             netIncome = t1.page4.line_23600_NetIncome},
                                                          ON.donations = partB2.donations{
                                                             ON.line47_base = schedule9.page1.line13_min,
                                                             ON.line48_base = schedule9.page1.line14_difference}},
                                  ON.partC = partC{ON.line59_copy = t1.page7.partC_NetFederalTax.line_40427}}},
                     on479}

returnFields :: Returns FieldConst
returnFields = Returns{
  federal = Federal.formFieldsForProvince ON,
  on428 = within "Provincial428" Rank2.<$> on428Fields,
  on479 = within "Provincial479" Rank2.<$> on479Fields}

formFileNames :: Map FormKey Text
formFileNames = fromList [
  (FormKey.T1, "T1/5006-r"),
  (FormKey.Provincial428, "428/5006-c"),
  (FormKey.Provincial479, "479/5006-tc")]
