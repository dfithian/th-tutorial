module Solved.Exercise04 where

import ClassyPrelude
import Data.Aeson (ToJSON, FromJSON, Value (String), parseJSON, toJSON, withText)
import Language.Haskell.TH

import Solved.Exercise03

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
  conNames <- extractConstructors tyName
  [d| instance ToJSON $(conT tyName) where
        toJSON =
          $(spliceConstructors
              (\ conName -> [| String $(stringE =<< trimAndLowerTH tyName conName) |])
              conNames
           )
      instance FromJSON $(conT tyName) where
        parseJSON = withText $(stringE (show tyName))
          $(spliceValues
              (\ conName -> [| pure $(conE conName) |])
              (\ other -> [| fail $ "unknown " <> $(stringE (show tyName)) <> " type " <> show $(varE other) |])
              tyName conNames
           )
      instance PrettyShow $(conT tyName) where
        prettyShow = $(spliceConstructors (stringE <=< trimAndLowerTH tyName) conNames)
    |]
