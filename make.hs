#!/usr/bin/env stack
-- stack --resolver lts-14.2 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BlockArguments    #-}


import qualified Data.Text as T
import Options.Generic
import Protolude hiding (FilePath, die)
import Turtle

--------------------------------------------------------------------------------
----------------------------- Data types ---------------------------------------
--------------------------------------------------------------------------------

data Args 
  = Deploy 
    { verbose :: Bool
    } 
  | Invoke 
    { local :: Bool
    , verbose :: Bool
    , body :: Text
    }
  deriving (Generic, Show)

instance ParseRecord Args

--------------------------------------------------------------------------------
----------------------------- Command functions --------------------------------
--------------------------------------------------------------------------------

main :: IO ()
main = do
  x <- getRecord "Command line utility to automate deployments etc."
  unlessM (testfile "LICENSE") (die "Must be run from the project root")
  sh $ case x of
    Deploy verbose -> deploy verbose
    Invoke local verbose body -> invoke local verbose body

deploy :: Bool -> Shell ExitCode
deploy verbose = do
  run_ "stack clean"
  run_ "stack build"
  (_, root) <- call_ "stack path --local-install-root"
  run ["cp", T.strip root <> "/bin/bootstrap", "serverless"]
  cd "serverless"
  run $ catMaybes
    [ pure "serverless deploy"
    , if verbose then pure "--verbose" else mempty 
    ]

invoke :: Bool -> Bool -> Text -> Shell ExitCode
invoke local verbose body = do
  cd "serverless"
  run $ catMaybes
    [ pure "serverless invoke"
    , if local then pure "local" else mempty
    , pure "-f haskell-aws-lambda -l"
    , pure $ "-d '" <> body <> "'"
    , if verbose then pure "--verbose" else mempty
    ]

--------------------------------------------------------------------------------
----------------------------- Utility functions --------------------------------
--------------------------------------------------------------------------------

-- | Returs stdout
call_ :: Text -> Shell (ExitCode, Text)
call_ cmd = do
  putText $ "> " <> cmd
  shellStrict cmd empty

-- | Outputs stdout
run :: [Text] -> Shell ExitCode
run = run_ . unwords

run_ :: Text -> Shell ExitCode
run_ cmd = do
  putText $ "\n> " <> cmd
  shell cmd empty

unwords :: [Text] -> Text
unwords = T.intercalate " "