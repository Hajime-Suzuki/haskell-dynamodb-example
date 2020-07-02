module Main where

import qualified Data.Aeson                    as Aeson
import           Data.Aeson.TextValue
import           Data.Aeson.Embedded
import           AWSLambda
import           AWSLambda.Events.APIGateway
import           ClassyPrelude
import           Config
import           Domain.UseCases
import           Types
import           APIGateway.Handler
import           APIGateway.MockRequest
import           Adapters.Order
import           Domain.Order
import           Control.Lens
import           Domain.Types

main :: IO ()
main = handler

handler = handleRequest
  createOrderAdapter
  (  Just
  $  defaultMockRequest
  &  agprqBody
  ?~ (TextValue . Embedded $ CreateOrderPayload "userId1234"
                                                "address"
                                                "test@test.com"
     )
  )

