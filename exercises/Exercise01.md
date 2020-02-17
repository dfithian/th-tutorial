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
```

We have `Exp` expressions and `Dec` declarations. How are they different? What else can we define in Template Haskell?

## Configuring GHCi

Let's load this into GHCi:

```bash
cat > .ghci <<EOF
:set prompt "λ → "
:set prompt-cont "λ ‖ "
:set -pgmL markdown-unlit
EOF

stack ghci exercises/Exercise01.lhs
```

## Exercises

Quiz time! What do we think these all return?

```bash
:type helloWorldE
helloWorldE
runQ helloWorldE
:type $(helloWorldE)
$(helloWorldE)

:type helloWorldD
helloWorldD
runQ helloWorldD
:type $(helloWorldD)
$(helloWorldD)
```
