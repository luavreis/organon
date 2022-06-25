-- |

module Site.Content.Options where
import System.FilePattern (FilePattern)
import LaTeX (LaTeXOptions)
import JSON

data Options = Options
  { mount :: FilePath
  , exclude :: [FilePattern]
  , serveAt :: FilePath
  , latexOptions :: LaTeXOptions
  , orgAttachDir :: FilePath
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Options where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON Options where
  parseJSON = genericParseJSON customOptions
