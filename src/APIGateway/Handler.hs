module APIGateway.Handler where

import           AWSLambda.Events.APIGateway
import           System.Environment             ( lookupEnv )
import           Network.AWS.Data
import           Data.Aeson.Embedded
import           Data.Aeson
import           ClassyPrelude
import           Text.Pretty.Simple
import           Data.Aeson.Encode.Pretty
import           APIGateway.MockRequest
import           Types
import           Data.Maybe
import           Adapters.Types

handleRequest
  :: (FromJSON a, ToJSON b)
  => Adapter a b
  -> Maybe (APIGatewayProxyRequest (Embedded a))
  -> IO ()
handleRequest adapter mockReq = do
  stage     <- lookupEnv "STAGE"
  isOffline <- lookupEnv "IS_OFFLINE"
  case (stage, isOffline) of
    (Nothing, Nothing) ->
      adapter (fromMaybe defaultMockRequest mockReq) >>= pPrint . encodePretty
    _ -> apiGatewayMain adapter
