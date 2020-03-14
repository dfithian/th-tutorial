module Solved.Exercise02 where

import ClassyPrelude hiding (stripPrefix)
import Data.List (stripPrefix)
import Language.Haskell.TH

data Pet
  = PetDog
  | PetCat
  | PetTeddyBear
  deriving (Eq, Show)

class PrettyShow a where
  prettyShow :: a -> Text

trimAndLowerTH :: Name -> Name -> Q String
trimAndLowerTH tyName conName =
  let tyStr = show tyName
      conStr = show conName
  in case stripPrefix tyStr conStr of
    Nothing -> fail $ tyStr <> " not a prefix of " <> conStr
    Just suffix -> case suffix of
      c:cs -> pure $ (charToLower c):cs
      _ -> fail $ tyStr <> " not a proper prefix of " <> conStr

extractConstructors :: Name -> Q [Name]
extractConstructors tyName = do
  info <- reify tyName
  case info of
    TyConI (DataD _cxt _name _tyVarBndrs_ _kindMay constructors _derivClauses) -> for constructors $ \ case
      NormalC conName [] -> pure conName
      other -> fail $ "type " <> show tyName <> " had a nontrivial constructor: " <> show other
    other -> fail $ "type " <> show tyName <> " was not defined with `data`: " <> show other

spliceConstructors :: (Name -> Q Exp) -> [Name] -> Q Exp
spliceConstructors effect conNames =
  let happyPath = map $ \ conName ->
        match (conP conName []) (normalB $ effect conName) []
  in lamCaseE (happyPath conNames)

spliceValues :: (Name -> Q Pat) -> [Name] -> Q Exp
spliceValues effect conNames = do
  let happyPath = map $ \ conName -> do
        match (effect conName) (normalB [| pure $(conE conName) |]) []
      sadPath x = match (varP x) (normalB [| fail $ "Don't know what " <> show $(varE x) <> " is" |]) []
  otherName <- newName "other"
  lamCaseE (happyPath conNames <> [sadPath otherName])
