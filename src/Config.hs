{-# LANGUAGE DuplicateRecordFields #-}

module Config where
import Data.Aeson
import Data.Yaml as Y
import Data.Map (singleton)
import JSON
import Data.Aeson.KeyMap qualified as KM
import Site.Roam.Options qualified as Roam
import Site.Static qualified as Static
import qualified Site.Content as Content

data Config = Config
  { name :: Text
  , sources :: Sources
  , defaultLatexProcess :: Text
  , latexProcesses :: Map Text MathLaTeXProcess
  } deriving Generic

instance ToJSON Config where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON Config where
  parseJSON = genericParseJSON customOptions

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
  { name = ""
  , sources = Sources
    { zettelkasten = Roam.Options
      { Roam.orgAttachDir = "data",
        Roam.mount = "zettel",
        Roam.serveAt = "zettel",
        Roam.rawInclude = ["**/*"],
        Roam.exclude = defExclude
      }
    , content = Content.Options
      { Content.mount = "content"
      , Content.exclude = defExclude
      , Content.serveAt = ""
      }
      -- { kind = Content
      -- , mount = one "content"
      -- , rawInclude = ["**/*"]
      -- , exclude = defExclude
      -- , serve = "/content"
      -- }
    , static = Static.Options
      { Static.mount = "assets",
        Static.serveAt = "assets"
      }
    , templates = "templates"
    }
  , defaultLatexProcess = "latex"
  , latexProcesses = singleton "latex" MathLaTeXProcess
    { preamble = "\\documentclass{article}"
    , imageInputType = "dvi"
    , imageOutputType = "svg"
    , imageSizeAdjust = 1.7
    , latexCompiler = ["latex -interaction nonstopmode -output-directory %o %f"]
    , imageConverter = ["dvisvgm %f -n -b min -c %S -o %O"]
    }
  }
  where defExclude = ["**/.*/**/*"]

data Sources = Sources
  { static :: Static.Options
  , zettelkasten :: Roam.Options
  , content :: Content.Options
  , templates :: FilePath
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Sources where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON Sources where
  parseJSON = genericParseJSON customOptions

data MathLaTeXProcess = MathLaTeXProcess
  { preamble :: Text
  , imageInputType :: String
  , imageOutputType :: String
  , imageSizeAdjust :: Float
  , latexCompiler :: [Text]
  , imageConverter :: [Text]
  } deriving Generic

instance ToJSON MathLaTeXProcess where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON MathLaTeXProcess where
  parseJSON = genericParseJSON customOptions

loadConfigWithDefault :: MonadIO m => FilePath -> m Config
loadConfigWithDefault fp = do
  vals :: Value <- Y.decodeFileThrow fp
  pure $ adjustConfig defaultConfig vals
