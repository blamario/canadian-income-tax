module Tax.Canada.FormKey where

-- | The type of form keys for use in 'Tax.Canada.FDF.FDFs' 
data FormKey = T1 | T4 | Schedule6 | Schedule7 | Schedule8 | Schedule9 | Schedule11 | Provincial428 | Provincial479
             deriving (Eq, Ord, Read, Show)

