module Solved.Exercise03 where

import ClassyPrelude
import Data.Aeson (ToJSON, FromJSON, Value (String), parseJSON, toJSON, withText)
import Language.Haskell.TH

import Solved.Exercise02

deriveEnumInstances :: Name -> Q [Dec]
deriveEnumInstances tyName = do
  conNames <- extractConstructors tyName
  [d| instance ToJSON $(conT tyName) where
        toJSON =
          $(spliceConstructors (\ c -> [| String $(stringE =<< trimAndLowerTH tyName c) |]) conNames)
      instance FromJSON $(conT tyName) where
        parseJSON = withText $(stringE (show tyName))
          $(spliceValues (litP . StringL <=< trimAndLowerTH tyName) conNames)
      instance PrettyShow $(conT tyName) where
        prettyShow = $(spliceConstructors (stringE <=< trimAndLowerTH tyName) conNames)
    |]
