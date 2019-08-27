module Types where

import Protolude

import Data.Aeson ((.=), FromJSON(..), ToJSON(..))
import qualified Data.Aeson as Json
import Text.URI (URI)
import qualified Text.URI as URI

data Person = Person { personName :: String, personAge :: Int }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | https://tools.ietf.org/html/rfc7807
data Problem = Problem { problemType :: URI, problemTitle :: Text }
  deriving stock (Eq, Show)

instance ToJSON Problem where
  toJSON p = Json.object [ "type" .= URI.render (problemType p)
                         , "title" .= problemTitle p
                         ]
