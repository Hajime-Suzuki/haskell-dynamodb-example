{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Repositories.Order where

import           ClassyPrelude
import           Text.Pretty.Simple             ( pPrint )
import           Data.Either.Validation
import           Types
import           Repositories.Common
import           Control.Lens
import           Control.Monad.Trans.AWS
import           Control.Monad.Catch
import           Config
import           Control.Exception
import           Control.Monad.Except
import           Network.AWS.DynamoDB.PutItem
import           Network.AWS.DynamoDB
import           Domain.Order
import           Control.Monad.Trans.Resource


saveOrder :: Repository m => Order -> m ()
saveOrder order = do
  res <- handleReq =<< req
  pPrint $ res ^. pirsAttributes
 where
  req = do
    tableName <- asks (^. configTableName)
    return $ putItem tableName & piItem .~ item
  item = mapFromList
    [ ("PK"     , attrS . Just $ order ^. orderId)
    , ("SK"     , attrS $ Just "order")
    , ("userId" , attrS . Just $ order ^. orderUserId)
    , ("status" , attrS . Just . tshow $ order ^. orderStatus)
    , ("email"  , attrS . Just $ order ^. orderEmail . rawEmail)
    , ("address", attrS . Just $ order ^. orderAddress)
    ]

attrS v = attributeValue & avS .~ v

