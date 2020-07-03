module Adapters.Order where

import           ClassyPrelude
import           Control.Lens
import           Config
import           Types
import           Domain.UseCases
import           Data.Aeson
import           Data.Maybe
import           AWSLambda.Events.APIGateway
import           Text.Pretty.Simple
import           Domain.Order
import           Domain.Types
import           Adapters.Types
import           App


getOrderAdapter :: Adapter () Value
getOrderAdapter req = do
  config <- getConfig
  let orderId = fromMaybe (error "id not found")
                          (lookup "id" $ req ^. agprqPathParameters)
  mayOrder <- runUseCase config (getOrderUseCase orderId)
  let resBody = object [("order", toJSON mayOrder)]
  return $ responseOK & responseBodyEmbedded ?~ resBody

createOrderAdapter :: Adapter CreateOrderPayload Value
createOrderAdapter req = do
  config <- getConfig

  let payload = fromJust $ req ^. requestBodyEmbedded -- TODO: add exception

  order <- runUseCase config $ createOrderUseCase payload
  return $ responseOK & responseBodyEmbedded ?~ object [("order", toJSON order)]

getOrderByUserIdAdapter :: Adapter () Value
getOrderByUserIdAdapter req = do
  config <- getConfig
  let userId = fromMaybe (error "id not found")
                         (lookup "id" $ req ^. agprqPathParameters)
  mayOrder <- runUseCase config (getOrdersByUserIdUseCase userId)
  let resBody = object [("orders", toJSON mayOrder)]
  return $ responseOK & responseBodyEmbedded ?~ resBody


updateStatusAdapter :: Adapter UpdateStatusPayload Value
updateStatusAdapter req = do
  config <- getConfig
  let payload = fromJust $ req ^. requestBodyEmbedded
      orderId = fromMaybe (error "id not found")
                          (lookup "id" $ req ^. agprqPathParameters)
  mayOrder <- runUseCase config $ updateStatusUseCase orderId payload
  case mayOrder of
    Left e -> return $ response 500 & responseBodyEmbedded ?~ object
      [("error", toJSON e)]
    Right order -> return $ responseOK & responseBodyEmbedded ?~ object
      [("order", toJSON order)]


