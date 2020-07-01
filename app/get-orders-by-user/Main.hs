module Main where


import           AWSLambda
import           AWSLambda.Events.APIGateway
import           ClassyPrelude
import           Config
import           Domain.UseCases
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
