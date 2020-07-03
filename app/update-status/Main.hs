
module Main where

import           Adapters.Order
import           APIGateway.Handler
import           APIGateway.MockRequest
import           AWSLambda
import           AWSLambda.Events.APIGateway
import           ClassyPrelude
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Embedded
import           Data.Aeson.TextValue
import           Domain.Order
import           Domain.Types
import           Domain.UseCases
import           Types

localDevBody =
  defaultMockRequest
    &  agprqBody
    ?~ (TextValue . Embedded $ UpdateStatusPayload Paid)
    &  agprqPathParameters
    .~ mapFromList [("id", "1")]

main :: IO ()
main = handleRequest updateStatusAdapter $ Just localDevBody



