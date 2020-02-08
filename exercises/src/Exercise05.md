```haskell
module Exercise05 where

import ClassyPrelude
import qualified Data.Aeson as A
import qualified Test.Hspec as H
import qualified Test.Hspec.QuickCheck as HQ
import qualified Test.QuickCheck as Q

import Exercise03
```

# Exercise 05

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

deriveEnumInstances ''Fantastic

instance Q.Arbitrary Fantastic where
  arbitrary = Q.elements [FantasticMrFox, FantasticBeasts, FantasticFantasia]

thEnumSpec :: H.Spec
thEnumSpec = H.describe "TH Enums" $ do
  HQ.prop "always round trips JSON instances" $ \ (x :: Fantastic) ->
    A.decode (A.encode x) `H.shouldBe` Just x

  HQ.prop "always encodes to something we expect" $ \ (x :: Fantastic) ->
    A.encode x `H.shouldSatisfy` flip elem ["\"mrFox\"", "\"beasts\"", "\"fantasia\""]
```

Now to test.

```bash
stack ghci
import Test.Hspec
hspec thEnumSpec
```
