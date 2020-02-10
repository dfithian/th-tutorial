```haskell
module Exercise05 where

import ClassyPrelude

import Exercise04
```

# Exercise 05

Did it work? Let's find out.

```haskell
-- |Write a new enumeration that follows the conventions laid out, and call the new derivation function on it.
-- TODO fill this in
data Animal
  = AnimalLion
  | AnimalZebra
  | AnimalCatDog
  deriving (Eq, Show)

deriveEnumInstances ''Animal
```

```bash
stack ghci
import Data.Aeson

prettyShow AnimalLion
prettyShow AnimalCatDog
encode AnimalLion
eitherDecodeStrict' "\"catDog\"" :: Either String Animal
```
