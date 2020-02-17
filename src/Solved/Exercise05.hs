module Solved.Exercise05 where

import ClassyPrelude

import Solved.Exercise04

-- |Write a new enumeration that follows the conventions laid out, and call the new derivation function on it.
data Animal
  = AnimalLion
  | AnimalZebra
  | AnimalCatDog
  deriving (Eq, Show)

deriveEnumInstances ''Animal
