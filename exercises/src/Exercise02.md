```haskell
module Exercise02 where

import ClassyPrelude
import Language.Haskell.TH

import Exercise01
```

# Exercise 02

All right, remember `helloWorldD` from before? Let's see if we can call it _now_.

```haskell
-- $(helloWorldE)
$(helloWorldD)
```

```bash
stack ghci
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

```bash
stack ghci
runQ helloWorldD'
$(stringE . show =<< helloWorldD') :: String
```

And now we have a debugging utility.

Other notes:

* you can mix QuasiQuotes and splices together
* lowercase TH function and uppercase TH constructor similarities/differences
    * `Q [Dec]` vs `DecsQ`
* constructors/functions always end with the first letter of their type
    * `varE` is `Exp` but `varT` is `Type` etc

