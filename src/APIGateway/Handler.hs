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
import           AWSLambda.Events.APIGateway
import           Data.Maybe

handleRequest
  :: Adapter -> Maybe (APIGatewayProxyRequest (Embedded Value)) -> IO ()
handleRequest adapter mockReq = do
  stage     <- lookupEnv "STAGE"
  isOffline <- lookupEnv "IS_OFFLINE"
  case (stage, isOffline) of
    (Nothing, Nothing) ->
      adapter (maybe defaultMockRequest id mockReq) >>= pPrint . encodePretty
    _ -> apiGatewayMain adapter
