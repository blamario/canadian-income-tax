-- | Shared utility functions used by other modules
module Tax.Util where

import Data.Fixed (Centi)
import Data.Maybe (fromMaybe)

-- | Repeatedly apply the function to the argument until it reaches the fixed point.
fixEq :: Eq a => (a -> a) -> a -> a
fixEq f a
   | a == a' = a
   | otherwise = fixEq f a'
   where a' = f a

-- | Sum the list of arguments; return 'Nothing' iff all items are 'Nothing'.
totalOf :: (Foldable f, Num a) => f (Maybe a) -> Maybe a
totalOf = foldl' add Nothing
  where add Nothing x = x
        add x Nothing = x
        add (Just x) (Just y) = Just $! x+y

-- | Subtraction under 'Maybe'
difference :: Maybe Centi -> Maybe Centi -> Maybe Centi
difference Nothing Nothing = Nothing
difference a b = Just (fromMaybe 0 a - fromMaybe 0 b)

-- | Non-negative subtraction under 'Maybe', returning @Just 0@ instead of negative results
nonNegativeDifference :: Maybe Centi -> Maybe Centi -> Maybe Centi
nonNegativeDifference Nothing Nothing = Nothing
nonNegativeDifference Nothing (Just x) | x >= 0 = Nothing
nonNegativeDifference x y = Just (max 0 $ fromMaybe 0 x - fromMaybe 0 y)

-- | Multiplication under 'Maybe'
fractionOf :: Maybe Rational -> Maybe Centi -> Maybe Centi
fractionOf (Just x) (Just amt) = Just $ fromRational (x * toRational amt)
fractionOf _ _ = Nothing
