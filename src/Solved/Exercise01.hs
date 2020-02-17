module Solved.Exercise01 where

import ClassyPrelude
import Language.Haskell.TH

helloWorldE :: Q Exp
helloWorldE = [| putStrLn "hello world" |]

helloWorldD :: Q [Dec]
helloWorldD =
  [d| helloWorld :: IO ()
      helloWorld = putStrLn "i'm trapped in a splice!"
    |]
