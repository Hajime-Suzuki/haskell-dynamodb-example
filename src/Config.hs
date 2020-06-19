module Config where

import           ClassyPrelude
import           Repositories.Common
import           Types

getConfig :: IO Config
getConfig = Config <$> dbEnv
