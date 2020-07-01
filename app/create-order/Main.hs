module Main where

import qualified Data.Aeson                    as Aeson
import           AWSLambda
import           ClassyPrelude
import           Config
import           Domain.UseCases.CreateOrder
import           Types
import           APIGateway.Handler
import           APIGateway.MockRequest
import           Adapters.Order

main :: IO ()
main = handler

handler = handleRequest createOrderAdapter (Just defaultMockRequest)

