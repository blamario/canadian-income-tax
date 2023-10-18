{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Tax.Canada (fixOntarioReturns)
import Tax.Canada.T1.Fix (T1, fixT1)
import Tax.Canada.ON428.Fix (ON428, fixON428)

import Test.Transformations qualified as Transformations

import Data.Functor.Const (Const (Const))
import Rank2 qualified
import Transformation.Shallow qualified as Shallow

import Hedgehog (Gen, Property, assert, forAll, property)
import Test.Tasty
import Test.Tasty.Hedgehog

main = defaultMain properties

properties :: TestTree
properties =
  testGroup "Properties" [
    testGroup "idempotence" [
      testProperty "T1" (checkFormIdempotent fixT1),
      testProperty "ON428" (checkFormIdempotent fixON428),
      testProperty "T1+ON428" (checkFormIdempotent fixOntarioReturns')]]
  where fixOntarioReturns' :: Rank2.Product T1 ON428 Maybe -> Rank2.Product T1 ON428 Maybe
        fixOntarioReturns' (Rank2.Pair x y) = uncurry Rank2.Pair $ fixOntarioReturns (x, y)

checkFormIdempotent :: forall g. (Eq (g Maybe), Show (g Maybe),
                                  Rank2.Applicative g, Shallow.Traversable Transformations.Gen g)
                    => (g Maybe -> g Maybe) -> Property
checkFormIdempotent f = checkIdempotent generateForm f
  where generateForm :: Gen (g Maybe)
        generateForm = Shallow.traverse Transformations.Gen (Rank2.pure Nothing)

checkIdempotent :: (Eq a, Show a) => Gen a -> (a -> a) -> Property
checkIdempotent gen f = property $ forAll gen >>= \x-> let x' = f x in assert (f x' == x')

