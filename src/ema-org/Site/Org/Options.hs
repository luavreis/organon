module Site.Org.Options where

import Site.Org.JSON
import Site.Org.LaTeX.Types (LaTeXOptions)
import System.FilePattern (FilePattern)

data Options = Options
  { mount :: [FilePath],
    exclude :: [FilePattern],
    orgAttachDir :: FilePath,
    publicTags :: [Text],
    privateTags :: [Text],
    latexOptions :: LaTeXOptions
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Options where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON Options where
  parseJSON = genericParseJSON customOptions
