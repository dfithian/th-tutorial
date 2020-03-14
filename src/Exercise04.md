```haskell
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Exercise04 where

import ClassyPrelude
import Data.Aeson (decode, encode)
import Test.Hspec (Spec, describe, hspec, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, arbitrary, elements)

import Exercise02
import Exercise03
```

Now, let's write some tests so that we can do this once and we never have to think about it ever again, even when we
refactor. We create a new enumeration, generate the instances, and test the instances. That way we're testing that (1)
the template haskell even compiles, because that would be terrible if it didn't, and (2) that the generated code does
what we want whenever we generate the way we expect.

```haskell
-- TODO uncomment
-- deriveEnumInstances ''Pet

instance Arbitrary Pet where
  arbitrary = elements [PetDog, PetCat, PetTeddyBear]
```

## Exercises

### Write tests for the instances we derived

```haskell
-- |Fill in the spec bodies with the tests we want to run.
-- We should be able to call it like this:
--
-- @
-- hspec thEnumSpec
-- @
thEnumSpec :: Spec
thEnumSpec = describe "TH Enums" $ do
  prop "always round trips JSON instances" $ \ (x :: Pet) ->
    fail "TODO fill me in" :: IO ()

  prop "always encodes to something we expect" $ \ (x :: Pet) ->
    fail "TODO fill me in" :: IO ()
```
