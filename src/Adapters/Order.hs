module Adapters.Order where

import           ClassyPrelude
import           Control.Lens
import           Config
import           Types
import           Domain.UseCases.GetOrder
import           Data.Aeson
import           AWSLambda.Events.APIGateway


getOrderAdapter :: Adapter
getOrderAdapter req = do
  config   <- getConfig
  mayOrder <- runUseCase2 config (getOrderUseCase "1")
  print "test"
  let resBody = object [("order", toJSON mayOrder)]
  return $ responseOK & responseBodyEmbedded ?~ resBody
