module Main where


import           AWSLambda
import           AWSLambda.Events.APIGateway
import           ClassyPrelude
import           Config
import           Domain.UseCases.GetOrdersByUserId
import           Types
import           APIGateway.Handler
import           Control.Lens
import           APIGateway.MockRequest
import           Adapters.Order

main :: IO ()
main = handleRequest
  getOrderByUserIdAdapter
  (Just $ defaultMockRequest & agprqPathParameters .~ mapFromList
    [("id", "1234")]
  )
