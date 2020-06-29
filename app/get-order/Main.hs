module Main where

import qualified Data.Aeson                    as Aeson
import           AWSLambda
import           AWSLambda.Events.APIGateway
import           ClassyPrelude
import           Config
import           Types
import           Adapters.Order
import           APIGateway.Handler
import           Control.Lens
import           APIGateway.MockRequest

main :: IO ()
main = handleRequest
  getOrderAdapter
  (Just $ defaultMockRequest & agprqPathParameters .~ mapFromList [("id", "1")])



