{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Repositories.Order where

import           ClassyPrelude
import           Text.Pretty.Simple             ( pPrint )
import           Data.Either.Validation
import           Data.Maybe
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
import           Prelude                        ( read )

type PK = Text

save :: Repository m => Order -> m ()
save order = do
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

findByOrderId :: Repository m => PK -> m (Maybe Order)
findByOrderId orderId = do
  res <- handleReq =<< req
  let resData = res ^. girsItem
  return $ if null resData
    then Nothing
    else maybe (error "parse error") return (fromDB resData) -- TODO: remove error and add exception

 where
  req = do
    tableName <- asks (^. configTableName)
    return $ getItem tableName & giKey .~ keyCondition
  keyCondition = mapFromList
    [("PK", attrS . Just $ orderId), ("SK", attrS . Just $ "order")]


fromDB :: FromDB Order
fromDB dbData = do
  pk      <- lookup "PK" dbData >>= (^. avS)
  sk      <- lookup "SK" dbData >>= (^. avS)
  status  <- lookup "status" dbData >>= (^. avS) >>= readMay . unpack
  address <- lookup "address" dbData >>= (^. avS)
  email   <- lookup "email" dbData >>= (^. avS)
  return $ Order pk sk status address (Email email)



