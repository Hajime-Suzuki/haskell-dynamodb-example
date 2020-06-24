module Config where

import           ClassyPrelude
import           Repositories.Common
import           Types

getConfig :: IO Config
getConfig =
  Config <$> dbEnv <*> pure "haskell-dynamodb-example" <*> pure "GSI1"
