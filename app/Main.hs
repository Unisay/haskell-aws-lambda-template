module Main where

import qualified Aws.Lambda as AWS
import qualified Lib

-- https://theam.github.io/aws-lambda-haskell-runtime/index.html
main :: IO ()
main = AWS.runLambda run

run :: AWS.LambdaOptions -> IO (Either String AWS.LambdaResult)
run AWS.LambdaOptions{AWS.functionHandler, AWS.contextObject, AWS.eventObject} =
  case functionHandler of
    "src/Lib.handler" ->
      either (Left . AWS.encodeObj) (Right . AWS.LambdaResult . AWS.encodeObj)
      <$> Lib.handler (AWS.decodeObj eventObject) (AWS.decodeObj contextObject)
    _ -> pure $ Left $
      "Handler " <> functionHandler <> " does not exist on project"
