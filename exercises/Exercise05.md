```haskell
module Exercise05 where

import ClassyPrelude

import Solved.Exercise04
```

# Exercise 05

Did it work? Let's find out.

## Exercises

### Create a new type and derive instances for it

```haskell
-- |Write a new enumeration that follows the conventions laid out, and call the new derivation function on it.
data Animal -- TODO fill me in with a Lion and CatDog constructor and derive enum instances
```

## Testing

```bash
stack ghci exercises/Exercise05.lhs
import Data.Aeson

prettyShow AnimalLion
prettyShow AnimalCatDog
encode AnimalLion
eitherDecodeStrict' "\"catDog\"" :: Either String Animal
```
