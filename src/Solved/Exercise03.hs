module Solved.Exercise03 where

import ClassyPrelude hiding (stripPrefix)
import Data.List (stripPrefix)
import Language.Haskell.TH

class PrettyShow a where
  prettyShow :: a -> Text

-- |Trim and lower a string by removing its prefix.
trimAndLowerTH :: Name -> Name -> Q String
trimAndLowerTH tyName conName =
  let tyStr = show tyName
      conStr = show conName
  in case stripPrefix tyStr conStr of
    Nothing -> fail $ tyStr <> " not a prefix of " <> conStr
    Just suffix -> case suffix of
      c:cs -> pure $ (charToLower c):cs
      _ -> fail $ tyStr <> " not a proper prefix of " <> conStr

-- |Extract the constructors.
-- Fill in the pattern match statement.
extractConstructors :: Name -> Q [Name]
extractConstructors tyName = do
  info <- reify tyName
  case info of
    TyConI (DataD _cxt _name _tyVarBndrs_ _kindMay constructors _derivClauses) -> for constructors $ \ case
      NormalC conName [] -> pure conName
      other -> fail $ "type " <> show tyName <> " had a nontrivial constructor: " <> show other
    other -> fail $ "type " <> show tyName <> " was not defined with `data`: " <> show other

-- |`spliceConstructors f conNames` takes a list of constructor names `conNames` and a function `f` applied to each
-- constructor name. It splices them in a `\ case` expression. For the `Pet` example you would pass in something like:
--
-- @
-- spliceConstructors (stringE . show) [''PetDog, ''PetCat, ''PetTeddyBear]
-- @
--
-- and get something like:
--
-- @
-- \ case
--   PetDog -> "PetDog"
--   PetCat -> "PetCat"
--   PetTeddyBear -> "PetTeddyBear"
-- @
--
-- Fill in the match statement given the function arguments.
spliceConstructors :: (Name -> Q Exp) -> [Name] -> Q Exp
spliceConstructors effect conNames =
  let happyPath = map $ \ conName ->
        match (conP conName []) (normalB $ effect conName) []
  in lamCaseE (happyPath conNames)

-- |`spliceValues f g tyName conNames` takes a list of constructor names `conNames` as well as a matching function `f`
-- for the constructor names, a fallback `g` function for the catch-all case, and a type name `tyName`. It splices them
-- in a `\ case` expression. For the `Pet` example you would pass in something like:
--
-- @
-- spliceValues
--   (\ c -> [| pure $(conE c) |])
--   (\ other -> [| fail $ "Don't know what " <> show $(varE other) <> " is" |])
--   ''Pet
--   [''PetDog, ''PetCat, ''PetTeddyBear]
-- @
--
-- and get something like:
--
-- @
-- \ case
--   "PetDog" -> pure PetDog
--   "PetCat" -> pure PetCat
--   "PetTeddyBear" -> pure PetTeddyBear
--   other -> fail $ "Don't know what " <> other <> " is"
-- @
--
-- Fill in the match statement given the function arguments.
spliceValues :: (Name -> Q Exp) -> (Name -> Q Exp) -> Name -> [Name] -> Q Exp
spliceValues effect fallback tyName conNames = do
  let happyPath = map $ \ conName -> do
        trimmed <- trimAndLowerTH tyName conName
        match (litP (stringL trimmed)) (normalB $ effect conName) []
      sadPath x = match (varP x) (normalB (fallback x)) []
  otherName <- newName "other"
  lamCaseE (happyPath conNames <> [sadPath otherName])
