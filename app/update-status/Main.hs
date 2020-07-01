
module Main where

import           Data.Aeson
import           Data.Aeson.Embedded
import           Data.Aeson.TextValue
import           AWSLambda
import           AWSLambda.Events.APIGateway
import           ClassyPrelude
import           Domain.UseCases.UpdateStatus
import           Types
import           APIGateway.Handler
import           APIGateway.MockRequest
import           Adapters.Order
import           Domain.Order
import           Control.Lens

main :: IO ()
main = handleRequest
  updateStatusAdapter
  (  Just
  $  defaultMockRequest
  &  agprqBody
  ?~ (TextValue . Embedded . toJSON $ UpdateStatusPayload Paid)
  )



