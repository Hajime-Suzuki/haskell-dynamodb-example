module Adapters.Order where

import           ClassyPrelude
import           Control.Lens
import           Config
import           Types
import           Domain.UseCases.GetOrder
import           Domain.UseCases.GetOrdersByUserId
import           Domain.UseCases.UpdateStatus
import           Domain.UseCases.CreateOrder
import           Data.Aeson
import           Data.Maybe
import           AWSLambda.Events.APIGateway
import           Text.Pretty.Simple
import           Domain.Order

-- getOrderAdapter :: Adapter' () Value
getOrderAdapter req = do
  config <- getConfig
  let orderId = fromMaybe (error "id not found")
                          (lookup "id" $ req ^. agprqPathParameters)
  mayOrder <- runUseCase2 config (getOrderUseCase orderId)
  let resBody = object [("order", toJSON mayOrder)]
  return $ responseOK & responseBodyEmbedded ?~ resBody


-- getOrderByUserIdAdapter :: Adapter' () Value
getOrderByUserIdAdapter req = do
  config <- getConfig
  let userId = fromMaybe (error "id not found")
                         (lookup "id" $ req ^. agprqPathParameters)
  mayOrder <- runUseCase2 config (getOrdersByUserIdUseCase userId)
  let resBody = object [("orders", toJSON mayOrder)]
  return $ responseOK & responseBodyEmbedded ?~ resBody


-- updateStatusAdapter :: Adapter' UpdateStatusPayload Value
updateStatusAdapter req = do
  config   <- getConfig
  mayOrder <- runUseCase2 config (updateStatusUseCase "1" Delivered)
  return $ responseOK & responseBodyEmbedded ?~ object
    [("order", toJSON mayOrder)]


-- createOrderAdapter :: Adapter' () Value
createOrderAdapter req = do
  config <- getConfig
  -- flip runReaderT config (createOrder "123456" "test@test.com")
  -- v      <- runUseCase config (createOrder "123456" "test@test.com")
  order  <- runUseCase2 config (createOrderUseCase "123456" "test@test.com")
  return $ responseOK & responseBodyEmbedded ?~ object [("order", toJSON order)]

