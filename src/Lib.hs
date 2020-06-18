
module Lib where

import           ClassyPrelude
import           Types
import           Text.Pretty.Simple             ( pPrint )
import           Data.Aeson
import           Repositories.Common
import           Network.AWS                    ( runAWS )
import           Network.AWS.DynamoDB.ListTables
import           Control.Monad.Trans.AWS

orderTest :: IO ()
orderTest = do
    env <- dbEnv
    v   <- runResourceT $ runAWS env $ send listTables
    pPrint v
