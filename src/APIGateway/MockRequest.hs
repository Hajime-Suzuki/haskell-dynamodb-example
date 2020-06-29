module APIGateway.MockRequest where
import           AWSLambda.Events.APIGateway
import           ClassyPrelude

defaultMockRequest = APIGatewayProxyRequest
  { _agprqResource              = mempty
  , _agprqPath                  = mempty
  , _agprqHttpMethod            = mempty
  , _agprqHeaders               = mempty
  , _agprqQueryStringParameters = mempty
  , _agprqPathParameters        = mempty
  , _agprqStageVariables        = mempty
  , _agprqRequestContext        = ProxyRequestContext
    { _prcPath         = Nothing
    , _prcAccountId    = mempty
    , _prcResourceId   = mempty
    , _prcStage        = mempty
    , _prcRequestId    = mempty
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
    , _prcResourcePath = mempty
    , _prcHttpMethod   = mempty
    , _prcApiId        = mempty
    , _prcProtocol     = mempty
    , _prcAuthorizer   = Nothing
    }
  , _agprqBody                  = Nothing
  }
