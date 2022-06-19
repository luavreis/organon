-- |

module Site.Static.Options where
import JSON

data Options = Options
  { mount :: FilePath
  , serveAt :: FilePath
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Options where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON Options where
  parseJSON = genericParseJSON customOptions
