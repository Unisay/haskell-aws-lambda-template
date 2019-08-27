{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Aws.Api.Gateway where

import Protolude

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Casing (aesonDrop, camelCase)
import Data.Aeson.TH (deriveFromJSON)
import Data.Aeson.TextValue (TextValue, unTextValue)
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IP
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Network.AWS.Data.Base64
import Network.AWS.Data.Text
import qualified Network.HTTP.Types as HTTP

type Method = Text

-- type HeaderName = CI Text
type HeaderName = Text --- XXX should be CI Text

type HeaderValue = Text

type QueryParamName = Text

type QueryParamValue = Text

type PathParamName = Text

type PathParamValue = Text

type StageVarName = Text

type StageVarValue = Text

data RequestIdentity = RequestIdentity
  { _riCognitoIdentityPoolId :: !(Maybe Text)
  , _riAccountId :: !(Maybe Text)
  , _riCognitoIdentityId :: !(Maybe Text)
  , _riCaller :: !(Maybe Text)
  , _riApiKey :: !(Maybe Text)
  , _riSourceIp :: !(Maybe IP)
  , _riCognitoAuthenticationType :: !(Maybe Text)
  , _riCognitoAuthenticationProvider :: !(Maybe Text)
  , _riUserArn :: !(Maybe Text)
  , _riUserAgent :: !(Maybe Text)
  , _riUser :: !(Maybe Text)
  }
  deriving (Eq, Show)

readParse :: Read a => String -> Text -> Parser a
readParse msg str = case readMaybe (Text.unpack str) of
  Just result -> pure result
  Nothing -> fail $ "Failed to parse an " ++ msg

instance FromJSON RequestIdentity where
  parseJSON = withObject "RequestIdentity" $ \o ->
    RequestIdentity <$> o .:? "cognitoIdentityPoolId" <*> o .:? "accountId"
    <*> o .:? "cognitoIdentityId" <*> o .:? "caller" <*> o .:? "apiKey"
    <*> (o .:? "sourceIp" >>= traverse (readParse "IP address"))
    <*> o .:? "cognitoAuthenticationType"
    <*> o .:? "cognitoAuthenticationProvider" <*> o .:? "userArn"
    <*> o .:? "userAgent" <*> o .:? "user"

$(makeLenses ''RequestIdentity)

data Authorizer = Authorizer { _aPrincipalId :: !(Maybe Text)
                             , _aClaims      :: !Object
                             , _aContext     :: !Object
                             }
  deriving (Eq, Show)

instance FromJSON Authorizer where
  parseJSON = withObject "Authorizer" $
    \o -> Authorizer <$> o .:? "principalId" <*> o .:? "claims" .!= mempty
    <*> (pure $ HashMap.delete "principalId" $ HashMap.delete "claims" o)

$(makeLenses ''Authorizer)

data ProxyRequestContext = ProxyRequestContext
  { _prcPath         :: !(Maybe Text)
  , _prcAccountId    :: !Text
  , _prcResourceId   :: !Text
  , _prcStage        :: !Text
  , _prcRequestId    :: !Text
  , _prcIdentity     :: !RequestIdentity
  , _prcResourcePath :: !Text
  , _prcHttpMethod   :: !Text
  , _prcApiId        :: !Text
  , _prcProtocol     :: !Text
  , _prcAuthorizer   :: !(Maybe Authorizer)
  }
  deriving (Eq, Show)

$(deriveFromJSON (aesonDrop 4 camelCase) ''ProxyRequestContext)

$(makeLenses ''ProxyRequestContext)

data ProxyRequest body = ProxyRequest
  { _proxyRequestResource :: !Text
  , _proxyRequestPath :: !ByteString
  , _proxyRequestHttpMethod :: !HTTP.Method
  , _proxyRequestHeaders :: !HTTP.RequestHeaders
  , _proxyRequestQueryStringParameters :: !HTTP.Query
  , _proxyRequestPathParameters :: !(HashMap PathParamName PathParamValue)
  , _proxyRequestStageVariables :: !(HashMap StageVarName StageVarValue)
  , _proxyRequestRequestContext :: !ProxyRequestContext
  , _proxyRequestBody :: !(Maybe (TextValue body))
  }
  deriving (Eq, Show, Generic)

instance FromText body => FromJSON (ProxyRequest body) where
  parseJSON = withObject "ProxyRequest" $ \o -> ProxyRequest <$> o .: "resource"
    <*> (encodeUtf8 <$> o .: "path") <*> (encodeUtf8 <$> o .: "httpMethod")
    <*> (fmap fromAWSHeaders <$> o .:? "headers") .!= mempty
    <*> (fmap fromAWSQuery <$> o .:? "queryStringParameters") .!= mempty
    <*> o .:? "pathParameters" .!= HashMap.empty
    <*> o .:? "stageVariables" .!= HashMap.empty <*> o .: "requestContext"
    <*> o .:? "body"
  -- Explicit type signatures so that we don't accidentally tell Aeson
  -- to try to parse the wrong sort of structure
   where
    fromAWSHeaders :: HashMap HeaderName HeaderValue -> HTTP.RequestHeaders
    fromAWSHeaders = fmap toHeader . HashMap.toList
     where
      toHeader = bimap (CI.mk . encodeUtf8) encodeUtf8

    fromAWSQuery :: HashMap QueryParamName QueryParamValue -> HTTP.Query
    fromAWSQuery = fmap toQueryItem . HashMap.toList
     where
      toQueryItem =
        bimap encodeUtf8
              (\x -> if Text.null x then Nothing else Just . encodeUtf8 $ x)

$(makeLenses ''ProxyRequest)

-- | Get the request body, if there is one
requestBody :: Getter (ProxyRequest body) (Maybe body)
requestBody = proxyRequestBody . mapping unTextValue

-- | Get the binary (decoded Base64) request body, if there is one
requestBodyBinary :: Getter (ProxyRequest Base64) (Maybe ByteString)
requestBodyBinary = requestBody . mapping _Base64

data ProxyResponse body = ProxyResponse
  { _proxyResponseStatusCode :: !Int
  , _proxyResponseHeaders    :: !HTTP.ResponseHeaders
  , _proxyResponseBody       :: !(Maybe (TextValue body))
  }
  deriving (Eq, Show, Generic)

instance ToText body => ToJSON (ProxyResponse body) where
  toJSON ProxyResponse{..} =
    object [ "statusCode" .= _proxyResponseStatusCode
           , "headers" .= toAWSHeaders _proxyResponseHeaders
           , "body" .= _proxyResponseBody
           ]
   where
    toAWSHeaders :: HTTP.ResponseHeaders -> HashMap HeaderName HeaderValue
    toAWSHeaders = HashMap.fromList
      . fmap (bimap (decodeUtf8 . CI.original) decodeUtf8)

instance FromText body => FromJSON (ProxyResponse body) where
  parseJSON = withObject "ProxyResponse" $ \o -> ProxyResponse
    <$> o .: "statusCode" <*> (fromAWSHeaders <$> o .: "headers")
    <*> o .:? "body"
  -- Explicit type signatures so that we don't accidentally tell Aeson
  -- to try to parse the wrong sort of structure
   where
    fromAWSHeaders :: HashMap HeaderName HeaderValue -> HTTP.RequestHeaders
    fromAWSHeaders = fmap toHeader . HashMap.toList
     where
      toHeader = bimap (CI.mk . encodeUtf8) encodeUtf8

$(makeLenses ''ProxyResponse)

response :: Int -> ProxyResponse body
response statusCode = ProxyResponse statusCode mempty Nothing

responseOK :: ProxyResponse body
responseOK = response 200

responseNotFound :: ProxyResponse body
responseNotFound = response 404

responseBadRequest :: ProxyResponse body
responseBadRequest = response 400

responseBody :: Setter' (ProxyResponse body) (Maybe body)
responseBody = proxyResponseBody . at () . mapping unTextValue

responseBodyBinary :: Setter' (ProxyResponse Base64) (Maybe ByteString)
responseBodyBinary = responseBody . mapping _Base64
