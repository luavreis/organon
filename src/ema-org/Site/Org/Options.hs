module Site.Org.Options where

import Site.Org.JSON
import Site.Org.LaTeX.Types (LaTeXOptions)
import System.FilePattern (FilePattern)
import Org.Exporters.Processing.OrgData (ExporterSettings)

data Source = Source {under :: Text, dir :: FilePath}
  deriving (Eq, Ord, Show, Generic)

srcToPair :: Source -> (Text, FilePath)
srcToPair (Source x y) = (x, y)

instance ToJSON Source where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON Source where
  parseJSON = genericParseJSON customOptions

data Options = Options
  { mount :: [Source],
    staticPatterns :: [FilePattern],
    exclude :: [FilePattern],
    orgAttachDir :: FilePath,
    exporterSettings :: ExporterSettings,
    latexOptions :: LaTeXOptions
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Options where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON Options where
  parseJSON = genericParseJSON customOptions
