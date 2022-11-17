{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Config where
import Data.Yaml as Y
import Data.Map (singleton)
import Site.Org.JSON
import Site.Org.LaTeX.Types
import Data.Aeson.KeyMap qualified as KM
import Site.Org.Options qualified as Org
import Org.Exporters.Processing (defaultExporterSettings)

data Config = Config
  { orgFiles :: Org.Options
  , templates :: FilePath
  , layouts :: FilePath
  , cacheFile :: FilePath
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
defaultConfig = Config
  { orgFiles = Org.Options
    { Org.orgAttachDir = "data"
    , Org.mount = [Org.Source "" "content", Org.Source "zettel" "zettel"]
    , Org.staticPatterns = ["**/*.png", "**/*.jpg", "**/*.svg"]
    , Org.exclude = defExclude
    , Org.latexOptions = defLaTeXOptions
    , Org.exporterSettings = defaultExporterSettings
    }
  , templates = "templates"
  , layouts = "layouts"
  , cacheFile = "site.cache"
  }
  where
    defExclude = ["**/.*/**/*"]
    defLaTeXOptions = LaTeXOptions
      { defaultLatexProcess = "latex"
      , latexProcesses = singleton "latex" MathLaTeXProcess
        { preamble = "\\documentclass{article}"
        , imageInputType = "dvi"
        , imageOutputType = "svg"
        , imageMIMEType = "image/svg+xml"
        , imageSizeAdjust = 1.7
        , latexCompiler = ["latex -interaction nonstopmode -output-directory %o %f"]
        , imageConverter = ["dvisvgm %f -n -b min -c %S -o %O"]
        }
      }
