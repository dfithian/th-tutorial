module Solved.Exercise02 where

import ClassyPrelude
import Language.Haskell.TH

import Solved.Exercise01

-- $(helloWorldE) -- fails because this is `Exp`
$(helloWorldD)

helloWorldD' :: Q [Dec]
helloWorldD' = do
  helloWorldName <- newName "helloWorld"
  Just ioName <- lookupTypeName "IO"
  Just putStrLnName <- lookupValueName "putStrLn"
  sequence $
    [ sigD helloWorldName (appT (conT ioName) (tupleT 0))
    , valD (varP helloWorldName) (normalB (appE (varE putStrLnName) (litE (stringL "i'm too far gone!!")))) []
    ]
