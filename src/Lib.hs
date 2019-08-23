module Lib where

import Protolude

import qualified Aws.Lambda as AWS (Context)
import Data.Aeson (FromJSON, ToJSON)

data Person = Person { personName :: String, personAge :: Int }
  deriving (Generic)

instance FromJSON Person

instance ToJSON Person

handler :: Person -> AWS.Context -> IO (Either String Person)
handler person _ = if personAge person > 0
                   then return (Right person)
                   else return (Left "A person's age must be positive")
