module Adapters.Order where

import           ClassyPrelude
import           Control.Lens
import           Config
import           Types
import           Domain.UseCases.GetOrder
import           Data.Aeson
import           AWSLambda.Events.APIGateway
import           Text.Pretty.Simple

getOrderAdapter :: Adapter
getOrderAdapter req = do
  config <- getConfig
  let orderId = fromMaybe (error "id not found")
                          (lookup "id" $ req ^. agprqPathParameters)
  mayOrder <- runUseCase2 config (getOrderUseCase orderId)
  let resBody = object [("order", toJSON mayOrder)]
  return $ responseOK & responseBodyEmbedded ?~ resBody
