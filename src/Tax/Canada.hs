{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Tax.Canada () where

import Tax.Canada.T1.Types qualified as T1
import Tax.Canada.T1.Fix (fixT1)
import Tax.Canada.Province.MB qualified as MB
import Tax.Canada.Province.ON qualified as ON
