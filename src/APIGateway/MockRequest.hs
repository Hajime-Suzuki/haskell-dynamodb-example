module APIGateway.MockRequest where
import           AWSLambda.Events.APIGateway
import           ClassyPrelude

defaultMockRequest = APIGatewayProxyRequest
  { _agprqResource              = "resource"
  , _agprqPath                  = "path"
  , _agprqHttpMethod            = "GET"
  , _agprqHeaders               = [("header1", "some val")]
  , _agprqQueryStringParameters = []
  , _agprqPathParameters        = mapFromList []
  , _agprqStageVariables        = mapFromList []
  , _agprqRequestContext        = ProxyRequestContext
    { _prcPath         = Nothing
    , _prcAccountId    = ""
    , _prcResourceId   = ""
    , _prcStage        = ""
    , _prcRequestId    = ""
    , _prcIdentity     = RequestIdentity
                           { _riCognitoIdentityPoolId         = Nothing
                           , _riAccountId                     = Nothing
                           , _riCognitoIdentityId             = Nothing
                           , _riCaller                        = Nothing
                           , _riApiKey                        = Nothing
                           , _riSourceIp                      = Nothing
                           , _riCognitoAuthenticationType     = Nothing
                           , _riCognitoAuthenticationProvider = Nothing
                           , _riUserArn                       = Nothing
                           , _riUserAgent                     = Nothing
                           , _riUser                          = Nothing
                           }
    , _prcResourcePath = ""
    , _prcHttpMethod   = ""
    , _prcApiId        = ""
    , _prcProtocol     = ""
    , _prcAuthorizer   = Nothing
    }
  , _agprqBody                  = Nothing
  }
