```haskell
module Exercise02 where

import ClassyPrelude hiding (stripPrefix)
import Data.Aeson (FromJSON, ToJSON, Value (String), parseJSON, toJSON, withText)
import Data.List (stripPrefix)
import Language.Haskell.TH
```

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

```
instance ToJSON Pet where
  toJSON = \ case
    PetDog -> String "dog"
    PetCat -> String "cat"
    PetTeddyBear -> String "teddyBear"
instance FromJSON Pet where
  parseJSON = withText "Pet" $ \ case
    "dog" -> pure PetDog
    "cat" -> pure PetCat
    "teddyBear" -> pure PetTeddyBear
    other -> fail $ "unknown pet type " <> unpack other
```

Maybe on top of that you also generate similar instances for `PersistField` or some prettier `Show`.

```haskell
class PrettyShow a where
  prettyShow :: a -> Text
```

```
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

## Exercises

### Trim and lowercasing first letter in a constructor

We need a way of trimming a type and lowercasing the first letter in each constructor. This function is provided for you
so you can use it in later exercises.

```haskell
-- |Trim and lower a string by removing its prefix.
-- Pass in something like:
--
-- @
-- putStrLn $(stringE =<< trimAndLowerTH ''Pet 'PetDog)
-- @
--
-- and get something like
--
-- @
-- dog
-- @
trimAndLowerTH :: Name -> Name -> Q String
trimAndLowerTH tyName conName =
  let tyStr = show tyName
      conStr = show conName
  in case stripPrefix tyStr conStr of
    Nothing -> fail $ tyStr <> " not a prefix of " <> conStr
    Just suffix -> case suffix of
      c:cs -> pure $ (charToLower c):cs
      _ -> fail $ tyStr <> " not a proper prefix of " <> conStr
```

### Extracting constructors

Next we need a way to extract constructors from a type in Template Haskell. Since we're in the `Q` monad, a call to
`fail` makes compilation fail. To do this we'll need `reify`, which looks up and provides information about a type,
value, class, you name it. What we're looking for in our case is a `data` type with N constructors, none of which take
any extra arguments.

```haskell
-- |Extract the constructors for a type.
-- Fill in the pattern match statement. Pass in something like:
--
-- @
-- putStrLn $(stringE . show =<< extractConstructors ''Pet)
-- @
--
-- and get something like:
--
-- @
-- [PetDog, PetCat, PetTeddyBear]
-- @
extractConstructors :: Name -> Q [Name]
extractConstructors tyName = do
  info <- reify tyName
  case info of
    _ -> fail "TODO fill me in"
```

### Iterating over constructors and values

Then we need a way to iterate over the list of constructors and values as the body of a `case` statement. Note that here
we should look up `lamCaseE` to figure out the appropriate shape for `happyPath`.

```haskell
-- |`spliceConstructors f conNames` takes a list of constructor names `conNames` and a function `f` applied to each
-- constructor name. It splices them in a `\ case` expression. For the `Pet` example you would pass in something like:
--
-- @
-- putStrLn $(stringE . pprint =<< spliceConstructors (stringE <=< trimAndLowerTH ''Pet) ['PetDog, 'PetCat, 'PetTeddyBear])
-- @
--
-- and get something like:
--
-- @
-- \ case
--   PetDog -> "dog"
--   PetCat -> "cat"
--   PetTeddyBear -> "teddyBear"
-- @
--
-- Fill in the match statement given the function arguments.
spliceConstructors :: (Name -> Q Exp) -> [Name] -> Q Exp
spliceConstructors effect conNames =
  let happyPath = fail "TODO fill this in"
  in lamCaseE (happyPath conNames)

-- |`spliceValues f g tyName conNames` takes a list of constructor names `conNames` as well as a matching function `f`
-- for the constructor names, and a type name `tyName`. It splices them in a `\ case` expression. For the `Pet` example
-- you would pass in something like:
--
-- @
-- putStrLn $(stringE . pprint =<< spliceValues (litP . StringL <=< trimAndLowerTH ''Pet) ['PetDog, 'PetCat, 'PetTeddyBear])
-- @
--
-- and get something like:
--
-- @
-- \ case
--   "dog" -> pure PetDog
--   "cat" -> pure PetCat
--   "teddyBear" -> pure PetTeddyBear
--   other -> fail $ "Don't know what " <> other <> " is"
-- @
--
-- Fill in the match statement given the function arguments.
spliceValues :: (Name -> Q Pat) -> [Name] -> Q Exp
spliceValues effect conNames = do
  let happyPath = fail "TODO fill me in"
      sadPath x = match (varP x) (normalB [| fail $ "Don't know what " <> show $(varE x) <> " is" |]) []
  otherName <- newName "other"
  lamCaseE (happyPath conNames <> [sadPath otherName])
```
