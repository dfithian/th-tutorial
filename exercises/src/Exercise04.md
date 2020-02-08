```haskell
module Exercise04 where

import ClassyPrelude

import Exercise03
```

# Exercise 04

Did it work? Let's find out.

```haskell
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
