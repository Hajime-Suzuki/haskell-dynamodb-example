module Adapters.Order where

import           ClassyPrelude
import           Control.Lens
import           Control.Exception              ( throw )
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
  let mayOrderId = lookup "id" $ req ^. agprqPathParameters
  case mayOrderId of
    Nothing      -> throwString "userId not found in pathParams"
    Just orderId -> do
      mayOrder <- runUseCase config (getOrderUseCase orderId)
      handleResponse "order" mayOrder

createOrderAdapter :: Adapter CreateOrderPayload Value
createOrderAdapter req = do
  config <- getConfig
  let mayPayload = req ^. requestBodyEmbedded
  case mayPayload of
    Nothing      -> throwString "body is empty"
    Just payload -> do
      order <- runUseCase config $ createOrderUseCase payload
      handleResponse "order" order

getOrderByUserIdAdapter :: Adapter () Value
getOrderByUserIdAdapter req = errorHandler $ do
  config <- getConfig
  let mayUserId = (lookup "id" $ req ^. agprqPathParameters)

  when (isNothing mayUserId) $ throwString "userId not found"

  case mayUserId of
    Nothing     -> throwString "userId not found in pathParams"
    Just userId -> do
      mayOrders <- runUseCase config (getOrdersByUserIdUseCase userId)
      handleResponse "orders" mayOrders

updateStatusAdapter :: Adapter UpdateStatusPayload Value
updateStatusAdapter req = errorHandler $ do
  config <- getConfig
  let (mayPayload, mayOrderId) =
        (req ^. requestBodyEmbedded, lookup "id" $ req ^. agprqPathParameters)

  case (mayPayload, mayOrderId) of
    (Nothing, _) -> throwString "body is empty"
    (_, Nothing) -> throwString "order id is not found in pathParams"
    (Just payload, Just orderId) -> do
      mayOrder <- runUseCase config $ updateStatusUseCase orderId payload
      handleResponse "order" mayOrder

handleResponse
  :: (ToJSON a, ToJSON e, Exception e)
  => Text
  -> Either e a
  -> IO (APIGatewayProxyResponse (Embedded Value))

handleResponse _ (Left e) = return $ throw e
handleResponse key (Right val) =
  return $ response 500 & responseBodyEmbedded ?~ object [(key, toJSON val)]

-- TODO: wrap this with ExceptT or EitherT
errorHandler
  :: (MonadUnliftIO m)
  => m (APIGatewayProxyResponse (Embedded Value))
  -> m (APIGatewayProxyResponse (Embedded Value))
errorHandler = flip
  catchAny
  (\e -> do
    print e
    return $ response 500 & responseBodyEmbedded ?~ object
      [("error", toJSON $ tshow e)]
  )

