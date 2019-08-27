module Lib where

import Protolude

import qualified Aws.Api.Gateway as Gateway
import qualified Aws.Lambda as AWS (Context)
import Control.Lens ((&), (?~), (^.))
import Data.Aeson (eitherDecodeStrict, encode)
import Text.URI.QQ (uri)
import Types

handler :: MonadError Problem m
        => Gateway.ProxyRequest ByteString
        -> AWS.Context
        -> m (Gateway.ProxyResponse ByteString)
handler request _ = do
  reqBody <- maybe (throwError $ problemEmptyBody) pure $
    request ^. Gateway.requestBody
  person :: Person <- either (throwError . problemBadBody) pure $
    eitherDecodeStrict reqBody
  let respBody = toS $ encode person
  return $ Gateway.responseOK & Gateway.responseBody ?~ respBody

problemEmptyBody :: Problem
problemEmptyBody = Problem
  { problemType  =
      [uri|https://github.com/Unisay/haskell-aws-template/empty-body|]
  , problemTitle = "Request body is empty"
  }

problemBadBody :: String -> Problem
problemBadBody err = Problem
  { problemType  =
      [uri|https://github.com/Unisay/haskell-aws-template/bad-body|]
  , problemTitle = "Failed to unmarshal request body: " <> toS err
  }
