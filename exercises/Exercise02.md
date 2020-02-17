```haskell
module Exercise02 where

import ClassyPrelude
import Language.Haskell.TH

import Solved.Exercise01
```

# Exercise 02

All right, remember `helloWorldD` from before? Let's see if we can call it _now_.

## Hello World Remix

```haskell
-- $(helloWorldE) -- fails because this is `Exp`
$(helloWorldD)
```

```bash
stack ghci exercises/Exercise02.lhs
:type helloWorld
helloWorld
```

So we have expressions and declarations, and they are invoked at different times.

So far we have only used QuasiQuotes, which is a way to "lift" regular Haskell expressions into Template Haskell. We can
also invoke Template Haskell directly. Let's try:

```haskell
helloWorldD' :: Q [Dec]
helloWorldD' = do
  helloWorldName <- newName "helloWorld"
  Just ioName <- lookupTypeName "IO"
  Just putStrLnName <- lookupValueName "putStrLn"
  sequence $
    [ sigD helloWorldName (appT (conT ioName) (tupleT 0))
    , valD (varP helloWorldName) (normalB (appE (varE putStrLnName) (litE (stringL "i'm too far gone!!")))) []
    ]
```

## Testing

```bash
stack ghci exercises/Exercise02.lhs
runQ helloWorldD'
$(stringE . show =<< helloWorldD') :: String
```

And now we have a debugging utility.

## Other Notes

* You can mix QuasiQuotes and splices together: `[| $(StringL "foo") :: String |]` and `$(sigE (StringL "foo") [t|
  String |])` are both valid.
* Lowercase TH functions and uppercase TH constructors are related.
    * `varP :: Name -> PatQ`
    * `type PatQ = Q Pat`
    * `data Pat = ... | VarP Name | ...`
* Constructors and functions always end with the first letter of their type.
    * `varE :: Name -> ExpQ`
    * `varT :: Name -> TypeQ`
    * `varP :: Name -> PatQ`

