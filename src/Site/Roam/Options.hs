-- |

module Site.Roam.Options where
import System.FilePattern (FilePattern)
import LaTeX (LaTeXOptions)
import JSON

data Options = Options
  { mount :: FilePath
  , serveAt :: FilePath
  , exclude :: [FilePattern]
  , orgAttachDir :: FilePath
  , publicTags :: [Text]
  , latexOptions :: LaTeXOptions
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Options where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON Options where
  parseJSON = genericParseJSON customOptions
