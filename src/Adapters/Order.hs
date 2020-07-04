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
import           Data.Aeson.Embedded


getOrderAdapter :: Adapter () Value
getOrderAdapter req = do
  config <- getConfig
  let orderId = fromMaybe (error "id not found")
                          (lookup "id" $ req ^. agprqPathParameters)
  mayOrder <- runUseCase config (getOrderUseCase orderId)
  handleResponse "order" mayOrder

createOrderAdapter :: Adapter CreateOrderPayload Value
createOrderAdapter req = do
  config <- getConfig

  let payload = fromJust $ req ^. requestBodyEmbedded -- TODO: add exception

  order <- runUseCase config $ createOrderUseCase payload
  handleResponse "order" order

getOrderByUserIdAdapter :: Adapter () Value
getOrderByUserIdAdapter req = do
  config <- getConfig
  let userId = fromMaybe (error "id not found")
                         (lookup "id" $ req ^. agprqPathParameters)
  mayOrders <- runUseCase config (getOrdersByUserIdUseCase userId)
  handleResponse "orders" mayOrders

updateStatusAdapter :: Adapter UpdateStatusPayload Value
updateStatusAdapter req = do
  config <- getConfig
  let payload = fromJust $ req ^. requestBodyEmbedded
      orderId = fromMaybe (error "id not found")
                          (lookup "id" $ req ^. agprqPathParameters)
  mayOrder <- runUseCase config $ updateStatusUseCase orderId payload
  handleResponse "order" mayOrder



handleResponse
  :: (ToJSON a, ToJSON e)
  => Text
  -> Either e a
  -> IO (APIGatewayProxyResponse (Embedded Value))

handleResponse _ (Left e) =
  return $ response 500 & responseBodyEmbedded ?~ object [("error", toJSON e)]
handleResponse key (Right val) =
  return $ response 500 & responseBodyEmbedded ?~ object [(key, toJSON val)]


