{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Repositories.Order where

import           ClassyPrelude
import           Config
import           Control.Exception
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Resource
import           Data.Either.Validation
import           Data.List                      ( foldl )
import           Domain.Order
import           Network.AWS.DynamoDB
import           Network.AWS.DynamoDB.PutItem
import           Prelude                        ( read )
import           Repositories.Common
import           Text.Pretty.Simple             ( pPrint )
import           Types
import           Repositories.Types
import qualified Data.Map.Strict               as M
import           Data.Text                      ( splitOn )



save :: Repository m => Order -> m ()
save order = do
  res <- handleReq =<< req
  pPrint $ res ^. pirsAttributes
 where
  req = do
    tableName <- asks (^. configTableName)
    return $ putItem tableName & piItem .~ item
  item = mapFromList
    [ ("PK"     , attrSJust . fromOrderId $ order ^. orderId)
    , ("SK"     , attrSJust mkSK)
    , ("GSI1_PK", attrSJust . fromUserId $ order ^. orderUserId)
    , ("status" , attrSJust . tshow $ order ^. orderStatus)
    , ("email"  , attrSJust $ order ^. orderEmail . rawEmail)
    , ("address", attrSJust $ order ^. orderAddress)
    ]

getByOrderId :: Repository m => Text -> m (Maybe Order)
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
    [("PK", attrSJust . fromOrderId $ orderId), ("SK", attrSJust mkSK)]


getByUserId :: Repository m => Text -> m [Order] -- TODO: add error when parse does not work
getByUserId userId = do
  res <- handleReq =<< req
  let resData = res ^. qrsItems
  return . catMaybes $ fromDB <$> resData
 where
  req = do
    tableName <- asks (^. configTableName)
    gsiName   <- asks (^. configGSI1Name)
    return
      $  query tableName
      &  qIndexName
      ?~ gsiName
      &  qKeyConditionExpression
      ?~ keyCondition
      &  qExpressionAttributeValues
      .~ values
  keyCondition = "GSI1_PK = :GSI_PK"
  values       = mapFromList [(":GSI_PK", attrSJust $ fromUserId userId)]


updateStatus :: Repository m => Text -> OrderStatus -> m Order
updateStatus orderId status = do
  res <- handleReq =<< req
  let mayOrder = fromDB $ res ^. uirsAttributes
  case mayOrder of
    Nothing    -> throwString "something wrong with data in DB. could not parse"
    Just order -> return order
 where
  req = do
    tableName <- asks (^. configTableName)
    return
      $  updateItem tableName
      &  uiKey
      .~ keys
      &  uiUpdateExpression
      ?~ expression
      &  uiExpressionAttributeNames
      .~ expressionName
      &  uiExpressionAttributeValues
      .~ values
      &  uiReturnValues
      ?~ AllNew
  keys = mapFromList
    [("PK", attrSJust . fromOrderId $ orderId), ("SK", attrSJust mkSK)]
  expression     = "SET #status = :status"
  expressionName = mapFromList [("#status", "status")]
  values         = mapFromList [(":status", attrSJust $ tshow status)]


fromOrderId :: Text -> Text
fromOrderId orderId = "order_" <> orderId

mkSK :: Text
mkSK = "order"

fromUserId :: Text -> Text
fromUserId userId = "user_" <> userId

fromDB :: FromDB Order
fromDB dbData = do
  orderId <- lookup "PK" dbData >>= (^. avS) >>= return . drop 6
  userId  <- lookup "GSI1_PK" dbData >>= (^. avS) >>= return . drop 5
  status  <- lookup "status" dbData >>= (^. avS) >>= readMay . unpack
  address <- lookup "address" dbData >>= (^. avS)
  email   <- lookup "email" dbData >>= (^. avS)
  return $ Order orderId userId status address (Email email)



