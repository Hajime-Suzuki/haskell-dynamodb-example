
module Lib where

import           ClassyPrelude
import           Text.Pretty.Simple             ( pPrint )
import           Network.AWS                    ( runAWS )
import           Network.AWS.DynamoDB.ListTables
import           Control.Monad.Trans.AWS
import           Types
import           Control.Lens
import           Config

getTablesUseCaseTest :: UseCase ()
getTablesUseCaseTest = do
    v <- getTables
    pPrint v


getTables :: Repository [Text]
getTables = do
    config <- ask
    res    <- runResourceT $ runAWS (config ^. configEnv) $ send listTables
    pPrint res
    return $ res ^. ltrsTableNames
