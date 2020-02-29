module Solved.Exercise04 where

import ClassyPrelude
import Data.Aeson (decode, encode)
import Test.Hspec (Spec, describe, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, arbitrary, elements)

import Solved.Exercise03

data Fantastic
  = FantasticMrFox
  | FantasticBeasts
  | FantasticFantasia
  deriving (Eq, Show)

data Incorrect
  = Correct
  deriving (Eq, Show)

deriveEnumInstances ''Fantastic

-- Should fail because the prefix is wrong. Can't figure out a way to run a macro down in `Q [Dec]` because `runQ` on
-- something that `fail`s will always fail before you can catch it. In any rate, you could also test this by putting
-- it in a separate project or file, compiling it independently, and grepping for compilation errors.
-- deriveEnumInstances ''Incorrect

instance Arbitrary Fantastic where
  arbitrary = elements [FantasticMrFox, FantasticBeasts, FantasticFantasia]

-- |Fill in the spec bodies with the tests we want to run.
thEnumSpec :: Spec
thEnumSpec = describe "TH Enums" $ do
  prop "always round trips JSON instances" $ \ (x :: Fantastic) ->
    decode (encode x) `shouldBe` Just x

  prop "always encodes to something we expect" $ \ (x :: Fantastic) ->
    encode x `shouldSatisfy` flip elem ["\"mrFox\"", "\"beasts\"", "\"fantasia\""]
