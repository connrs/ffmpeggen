module Command where

import           Data.Maybe
import           Data.List

data CommandTemplate =
  CommandTemplate { prefix :: Maybe String
                  , commandComponents :: [CommandComponent]
                  , suffix :: Maybe String }
  deriving (Eq, Ord, Show)

newtype CommandComponent = CommandComponent { getComponents :: [String] }
  deriving (Eq, Ord, Show)

buildCommand :: CommandTemplate -> String
buildCommand t =
  let internals = unwords . concatMap getComponents . commandComponents $ t
      template = [fromMaybe [] (prefix t), internals, fromMaybe [] (suffix t)]
  in unwords template