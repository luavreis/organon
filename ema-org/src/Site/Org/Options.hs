{-# OPTIONS_GHC -Wno-orphans #-}
module Site.Org.Options where

import Site.Org.Utils.JSON
import System.FilePattern (FilePattern)
import Org.Exporters.Processing.OrgData (ExporterSettings)
import Org.Parser (OrgOptions)

data Source = Source {serveAt :: Text, dir :: FilePath, alias :: Text}
  deriving (Eq, Ord, Show, Read, Generic, NFData)

srcToAliasMap :: [Source] -> Map Text Text
srcToAliasMap = fromList . map (\s -> (s.alias, s.serveAt))

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
    parserSettings :: OrgOptions,
    fileProtocols :: [Text]
  }
  deriving (Eq, Ord, Show, Generic, NFData)

instance ToJSON Options where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON Options where
  parseJSON = genericParseJSON customOptions

instance NFData ExporterSettings
