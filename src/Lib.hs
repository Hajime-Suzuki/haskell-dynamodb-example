
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ConstrainedClassMethods  #-}

module Lib where

import           ClassyPrelude
import           Text.Pretty.Simple             ( pPrint )
import           Network.AWS                    ( runAWS
                                                , AWS
                                                , MonadAWS
                                                )
import           Network.AWS.DynamoDB.ListTables
import           Control.Monad.Trans.AWS
import           Types
import           Control.Lens
import           Config
import           Control.Monad.Catch
import           Repositories.Common
import           Domain.Order
import           Data.Aeson                     ( encode )
import           Data.Either.Validation


getTablesTest :: Repository' ()
getTablesTest = do
    liftIO domainTest
    v <- getTables
    pPrint v



getTables :: Repository' [Text]
getTables = do
    res <- handleReq listTables
    pPrint res
    return $ res ^. ltrsTableNames


domainTest :: IO ()
domainTest = do
    let order = mkOrder "12345" "test@test.com"
    case order of
        Failure e     -> pPrint e
        Success order -> pPrint $ encode order
