{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Site.Organon.Config where

import Data.Aeson.KeyMap qualified as KM
import Data.Yaml as Y
import Org.Exporters.Processing (defaultExporterSettings)
import Site.Org.Options qualified as Org
import Site.Org.Utils.JSON
import Site.Org.Parsing.Options (parsingOptions)

data Config = Config
  { orgFiles :: Org.Options
  , templates :: FilePath
  , cacheFile :: FilePath
  , extraOptions :: Object
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Config where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON Config where
  parseJSON = genericParseJSON customOptions

loadConfigWithDefault :: MonadIO m => FilePath -> m Config
loadConfigWithDefault fp = do
  vals :: Value <- Y.decodeFileThrow fp
  pure $ adjustConfig defaultConfig vals

-- | Update config values, right biased. Left is reference.
adjustConfig :: Config -> Value -> Config
adjustConfig (toJSON -> v1) v2 =
  case Y.parseEither parseJSON $ adjust v1 v2 of
    Left e -> error $ "Could not convert to expected type: " <> toText e
    Right s -> s
  where
    adjust :: Value -> Value -> Value
    adjust (Object x) (Object y) = Object $ KM.unionWith adjust x y
    adjust _ y = y

defaultConfig :: Config
defaultConfig =
  Config
    { orgFiles =
        Org.Options
          { Org.mount = [Org.Source "" "content" "content" "data"]
          , Org.staticPatterns = ["**/*.png", "**/*.jpg", "**/*.svg"]
          , Org.exclude = defExclude
          , Org.exporterSettings = defaultExporterSettings
          , Org.parserSettings = parsingOptions
          , Org.fileProtocols = ["file", "pdf"]
          , Org.generatedMeta = mempty
          }
    , templates = "templates"
    , cacheFile = "site.cache"
    , extraOptions = mempty
    }
  where
    defExclude = ["**/.*/**/*"]
