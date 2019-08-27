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
  = Package
  | Deploy
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
  unlessM serverlessInfo
    (die "Please make sure that serverless framework is installed")
  unlessM (testfile "LICENSE")
    (die "Must be run from the project root")
  sh $ case x of
    Package -> package
    Deploy verbose -> deploy verbose
    Invoke local verbose body -> invoke local verbose body

serverlessInfo :: IO Bool
serverlessInfo = do
  cd "serverless"
  r <- (==) ExitSuccess <$> shell "sls info" empty
  cd ".."
  return r

type BinaryPath = Text

clean :: Shell ExitCode
clean = run_ "stack clean"

build :: Shell BinaryPath
build = do
  run_ "stack build"
  snd <$> call_ "stack path --local-install-root"

copyBinary :: BinaryPath -> Shell ExitCode
copyBinary root =
  run ["cp", T.strip root <> "/bin/bootstrap", "serverless"]

slsDeploy :: Bool -> Shell ExitCode
slsDeploy verbose = do
  cd "serverless"
  run $ catMaybes
    [ pure "serverless deploy"
    , if verbose then pure "--verbose" else mempty
    ]

slsPackage :: Bool -> Shell ExitCode
slsPackage verbose = do
  cd "serverless"
  run $ catMaybes
    [ pure "serverless package"
    , if verbose then pure "--verbose" else mempty
    ]

deploy :: Bool -> Shell ExitCode
deploy verbose = do
  clean
  root <- build -- TODO using/managed
  copyBinary root
  slsDeploy verbose

package :: Shell ExitCode
package = do
  clean
  root <- build -- TODO using/managed
  copyBinary root
  slsPackage True

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
