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
    [ ("PK"     , attrS . Just . mkPK $ order ^. orderId)
    , ("SK"     , attrS . Just $ mkSK)
    , ("userId" , attrS . Just $ order ^. orderUserId)
    , ("status" , attrS . Just . tshow $ order ^. orderStatus)
    , ("email"  , attrS . Just $ order ^. orderEmail . rawEmail)
    , ("address", attrS . Just $ order ^. orderAddress)
    ]

getByOrderId :: Repository m => PK -> m (Maybe Order)
getByOrderId orderId = do
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
    [("PK", attrS . Just . mkPK $ orderId), ("SK", attrS . Just $ mkSK)]

updateStatus :: Repository m => PK -> OrderStatus -> m ()
updateStatus orderId status = do
  res <- handleReq =<< req
  pPrint $ res ^. uirsAttributes
 where
  req = do
    tableName <- asks (^. configTableName)
    return
      $  updateItem tableName
      &  uiKey
      .~ keys
      &  uiUpdateExpression
      .~ expression
      &  uiExpressionAttributeNames
      .~ expressionName
      &  uiExpressionAttributeValues
      .~ values
  keys = mapFromList
    [("PK", attrS . Just . mkPK $ orderId), ("SK", attrS . Just $ mkSK)]
  expression     = Just "SET #status = :orderStatus"
  expressionName = mapFromList [("#status", "status")]
  values         = mapFromList [(":orderStatus", attrS . Just $ tshow status)]


mkPK :: Text -> Text
mkPK orderId = "order_" <> orderId

mkSK :: Text
mkSK = "order"

fromDB :: FromDB Order
fromDB dbData = do
  pk      <- lookup "PK" dbData >>= (^. avS)
  sk      <- lookup "SK" dbData >>= (^. avS)
  status  <- lookup "status" dbData >>= (^. avS) >>= readMay . unpack
  address <- lookup "address" dbData >>= (^. avS)
  email   <- lookup "email" dbData >>= (^. avS)
  return $ Order pk sk status address (Email email)



