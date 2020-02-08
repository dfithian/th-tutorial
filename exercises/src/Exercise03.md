```haskell
module Exercise03 where

import ClassyPrelude
import qualified Data.Aeson as A
import qualified Data.Char as C
import qualified Data.Text as T
import Language.Haskell.TH
```

# Exercise 03

Now we have a background in Template Haskell, so what can we do with it? Say we have some boilerplate-y thing we often
do and we want to generate that using a macro.

```haskell
data Pet
  = PetDog
  | PetCat
  | PetTeddyBear
  deriving (Eq, Show)
```

This is a pattern that I find very common: some enumeration, with constructors prefixed with the type name to avoid
ambiguity (due to Haskell's namespacing woes). Inevitably someone will want JSON instances.

```haskell
instance A.ToJSON Pet where
  toJSON = \ case
    PetDog -> A.String "dog"
    PetCat -> A.String "cat"
    PetTeddyBear -> A.String "teddyBear"
instance A.FromJSON Pet where
  parseJSON = A.withText "Pet" $ \ case
    "dog" -> pure PetDog
    "cat" -> pure PetCat
    "teddyBear" -> pure PetTeddyBear
    other -> fail $ "unknown pet type " <> unpack other
```

Maybe on top of that you also generate similar instances for `PersistField` or some prettier `Show`.

```haskell
class PrettyShow a where
  prettyShow :: a -> Text

instance PrettyShow Pet where
  prettyShow = \ case
    PetDog -> "dog"
    PetCat -> "cat"
    PetTeddyBear -> "teddyBear"
```

Pretty soon you have a bunch of enumerations and every time you have to write out five or six different instances, and
any time you refactor a function common to them all you have to go change the code in the exact same way in now every
single one of those instances. Terrible.

Where all this is leading should be pretty obvious: the point is that we want to generate some boilerplate instances for
enumerations. Eventually, we'll want a function `deriveEnumInstances :: Name -> Q [Dec]` that will generate these
instances for us. On the way, we need to define a few helper functions.

First, we need a way of trimming a `Show` instance and lowercasing the first letter. Since we're in Template Haskell
land, we must use `String`, though we can convert to `Text`.

```haskell
trimAndLower :: Name -> Name -> String
trimAndLower tyName conName =
  let tyStr = show tyName
      conStr = show conName
  in case T.stripPrefix (pack tyStr) (pack conStr) of
    Nothing -> case conStr of
      c:cs -> (C.toLower c):cs
      "" -> ""
    Just suffix -> case unpack suffix of
      c:cs -> (C.toLower c):cs
      "" -> ""
```

Next we need a way to extract constructors from a type in Template Haskell. Since we're in the `Q` monad, a call to
`fail` makes compilation fail. To do this we'll need `reify`, which looks up and provides information about a type,
value, class, you name it. What we're looking for in our case is a `data` type with N constructors, none of which take
any extra arguments.

```haskell
extractConstructors :: Name -> Q [Name]
extractConstructors tyName = do
  info <- reify tyName
  case info of
    TyConI (DataD _cxt _name _tyVarBndrs_ _kindMay constructors _derivClauses) -> for constructors $ \ case
      NormalC conName [] -> pure conName
      other -> fail $ "type " <> show tyName <> " had a nontrivial constructor: " <> show other
    other -> fail $ "type " <> show tyName <> " was not defined with `data`: " <> show other
```

Then we need a way to iterate over the list of constructors and strings as the body of a `case` statement.

```haskell
deployConstructors :: (Name -> Q Exp) -> [Name] -> Q Exp
deployConstructors effect conNames =
  let happyPath = map $ \ conName ->
        match (conP conName []) (normalB $ effect conName) []
  in lamCaseE (happyPath conNames)

deployValues :: (Name -> Q Exp) -> (Name -> Q Exp) -> Name -> [Name] -> Q Exp
deployValues effect fallback tyName conNames = do
  let happyPath = map $ \ conName ->
        match (litP (stringL (trimAndLower tyName conName))) (normalB $ effect conName) []
      sadPath x = match (varP x) (normalB (fallback x)) []
  otherName <- newName "other"
  lamCaseE (happyPath conNames <> [sadPath otherName])
```

We can put it together:

```haskell
deriveEnumInstances :: Name -> Q [Dec]
deriveEnumInstances tyName = do
  conNames <- extractConstructors tyName
  [d| instance A.ToJSON $(conT tyName) where
        toJSON =
          $(deployConstructors
              (\ conName -> [| A.String $(stringE (trimAndLower tyName conName)) |])
              conNames
           )
      instance A.FromJSON $(conT tyName) where
        parseJSON = A.withText $(stringE (show tyName))
          $(deployValues
              (\ conName -> [| pure $(conE conName) |])
              (\ other -> [| fail $ "unknown " <> $(stringE (show tyName)) <> " type " <> show $(varE other) |])
              tyName conNames
           )
      instance PrettyShow $(conT tyName) where
        prettyShow = $(deployConstructors (stringE . trimAndLower tyName) conNames)
    |]
```

We can look at what we did and see if it's reasonable.

```bash
stack ghci
:set -ddump-splices
import Language.Haskell.TH
$(stringE . show =<< deriveEnumInstances ''Pet) :: String
```
