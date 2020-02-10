```haskell
module Exercise05 where

import ClassyPrelude
import Data.Aeson (decode, encode)
import Test.Hspec (Spec, describe, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, arbitrary, elements)

import Exercise04
```

# Exercise 06

It worked! Now, let's write some tests so that we can do this once and we never have to think about it ever again, even
when we refactor. We create a new enumeration, generate the instances, and test the instances. That way we're testing
that (1) the template haskell even compiles, because that would be terrible if it didn't, and (2) that the generated
code does what we want whenever we generate the way we expect.

```haskell
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
    decode (encode x) `shouldBe` Just x -- TODO fill this in

  prop "always encodes to something we expect" $ \ (x :: Fantastic) ->
    encode x `shouldSatisfy` flip elem ["\"mrFox\"", "\"beasts\"", "\"fantasia\""] -- TODO fill this in
```

Now to test.

```bash
stack ghci
import Test.Hspec
hspec thEnumSpec
```
