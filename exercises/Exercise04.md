```haskell
module Exercise04 where

import ClassyPrelude
import Data.Aeson (ToJSON, FromJSON, Value (String), parseJSON, toJSON, withText)
import Language.Haskell.TH

import Solved.Exercise03
```

# Exercise 04

Recall our helper functions `trimAndLowerTH`, `extractConstructors`, `spliceConstructors`, `spliceValues`. We can
combine them to generate instances for `FromJSON`, `ToJSON`, and `PrettyShow`.

## Exercises

### Derive enum instances for the `Pet` type

```haskell
-- |`deriveEnumInstances tyName` takes a type name and derives three instances: `ToJSON`, `FromJSON`, `PrettyShow`. In
-- order to derive those instances we need to extract the constructors and invoke the `spliceConstructors` or
-- `spliceValues` function depending on what type of instance it is (showing or parsing, respectively). For the `Pet`
-- example you would pass in something like:
--
-- @
-- deriveEnumInstances ''Pet
-- @
--
-- and get something like:
--
-- @
-- Instance ToJSON Pet where
--   toJSON = \ case
--     PetDog -> String "dog"
--     PetCat -> String "cat"
--     PetTeddyBear -> String "teddyBear"
-- Instance FromJSON Pet where
--   parseJSON = withText "Pet" $ \ case
--     "dog" -> pure PetDog
--     "cat" -> pure PetCat
--     "teddyBear" -> pure PetTeddyBear
--     other -> fail $ "I don't know about " <> other
-- Instance PrettyShow Pet where
--   prettyShow = \ case
--     PetDog -> "dog"
--     PetCat -> "cat"
--     PetTeddyBear -> "teddyBear"
-- @
--
-- Fill in the body given the function arguments.
deriveEnumInstances :: Name -> Q [Dec]
deriveEnumInstances tyName = do
  conNames <- fail "TODO fill me in"
  [d| instance ToJSON $(conT tyName) where
        toJSON = error "TODO fill me in"
      instance FromJSON $(conT tyName) where
        parseJSON = error "TODO fill me in"
      instance PrettyShow $(conT tyName) where
        prettyShow = error "TODO fill me in"
    |]
```

## Testing

We can look at what we did and see if it's reasonable.

```bash
stack ghci exercises/Exercise04.lhs
:set -ddump-splices
import Language.Haskell.TH
$(stringE . show =<< deriveEnumInstances ''Pet) :: String
```
