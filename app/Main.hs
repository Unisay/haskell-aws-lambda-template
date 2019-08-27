module Main where

import Protolude

import qualified Aws.Api.Gateway as Gateway
import qualified Aws.Lambda as AWS
import Control.Lens ((?~))
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, withExceptT)
import qualified Data.Aeson as Json
import qualified Lib
import Types (Problem)

-- https://theam.github.io/aws-lambda-haskell-runtime/index.html
main :: IO ()
main = AWS.runLambda run

run :: AWS.LambdaOptions -> IO (Either String AWS.LambdaResult)
run AWS.LambdaOptions{AWS.functionHandler, AWS.contextObject, AWS.eventObject} =
  case functionHandler of
    "src/Lib.handler" ->
      handleRequest (AWS.decodeObj eventObject) (AWS.decodeObj contextObject)
    _ -> pure $ Left $
      "Handler " <> functionHandler <> " does not exist on project"

handleRequest :: Gateway.ProxyRequest ByteString
              -> AWS.Context
              -> IO (Either String AWS.LambdaResult)
handleRequest request context =
  runExceptT $ withExceptT AWS.encodeObj $ AWS.LambdaResult . AWS.encodeObj
  <$> Lib.handler request context `catchE` badRequest
 where
  badRequest :: Problem -> ExceptT () IO (Gateway.ProxyResponse ByteString)
  badRequest problem = pure $ Gateway.responseBadRequest & Gateway.responseBody
    ?~ (toS $ Json.encode problem)
