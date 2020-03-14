{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solved.Exercise04 where

import ClassyPrelude
import Data.Aeson (decode, encode)
import Test.Hspec (Spec, describe, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, arbitrary, elements)

import Solved.Exercise02
import Solved.Exercise03

deriveEnumInstances ''Pet

instance Arbitrary Pet where
  arbitrary = elements [PetDog, PetCat, PetTeddyBear]

-- |Fill in the spec bodies with the tests we want to run.
thEnumSpec :: Spec
thEnumSpec = describe "TH Enums" $ do
  prop "always round trips JSON instances" $ \ (x :: Pet) ->
    decode (encode x) `shouldBe` Just x

  prop "always encodes to something we expect" $ \ (x :: Pet) ->
    encode x `shouldSatisfy` flip elem ["\"dog\"", "\"cat\"", "\"teddyBear\""]
