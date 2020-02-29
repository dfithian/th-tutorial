```haskell
module Exercise01 where

import ClassyPrelude
import Language.Haskell.TH
```

# Exercise 01

Let's start with a simple Template Haskell example. Goals of this exercise:

1. Write a simple Hello World splice using the `Q` monad
2. Debug our splices in GHCi

## Hello World

Consider the following code:

```haskell
helloWorldE :: Q Exp
helloWorldE = [| putStrLn "hello world" |]

helloWorldD :: Q [Dec]
helloWorldD =
  [d| helloWorld :: IO ()
      helloWorld = putStrLn "i'm trapped in a splice!"
    |]

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

The above functions do more or less the same thing, but with some interesting differences. `helloWorldE` is an `Exp`
(expression), or code that can be evaluated direcly. `helloWorldD` and `helloWorldD'` are `Dec`s (declarations) which
define new top-level declarations within a module. `helloWorldD` and `helloWorldD'` are different in that the former is
QuasiQuoted (using the `d` identifier) while the latter is written directly in Template Haskell.

## Configuring GHCi

Let's load this into GHCi:

```bash
cat > .ghci <<EOF
:set prompt "λ → "
:set prompt-cont "λ ‖ "
:set -pgmL markdown-unlit
EOF

stack ghci
:l exercises/Exercise01.lhs
```

## Exercises

### Calling TH splices in GHCi

In general, TH splices are called with `$(<splice>)`.

We can't invoke all TH splices in GHCi. We can run `$(helloWorldE)` but we can't run `$(helloWorldD)`. In order to use
the declarations provided by `$(helloWorldD)` we'd have to put it in a file and reload the module. In addition, the
splice has to be declared in a separate file from the one in which it is invoked. Try it out for yourself, if you'd
like.

### Debugging expressions in GHCi

Whenever we want, we can debug our TH splices using a utility. Try it out by replacing `<splice>` with a function of
your choice.

```bash
putStrLn $(stringE . pprint =<< <splice>)
```

## Other Notes

* There are multiple QuasiQuote identifiers - `[| ... |]` or `[e| ... |]` for expressions, `[d| ... |]` for
  declarations, `[t| ... |]` for types, and `[p| ... |]` for patterns.
* `Name`s are identifiers for both values and types. We can promote a type to a `Name` to names with two single quotes:
  `''MyType`, and a value to a name with one single quote: `'MyDataConstructor` or even `'myVariable`.
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
