module Adapters.Types where

import           ClassyPrelude
import           Data.Aeson
import           Data.Aeson.Embedded
import           Network.AWS
import           AWSLambda.Events.APIGateway

type Adapter a b
  =  APIGatewayProxyRequest (Embedded a)
  -> IO (APIGatewayProxyResponse (Embedded b))
