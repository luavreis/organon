{-# LANGUAGE DuplicateRecordFields #-}

module Config where
import Data.Aeson
import Data.Yaml as Y
import Data.Char (isUpper, toLower)
import Data.Map (singleton)
import Data.HashMap.Strict (unionWith)
import System.FilePattern (FilePattern)

data Config = Config
  { name :: Text
  , sources :: Map SourceKind Source
  , templates :: [FilePath]
  , defaultLatexProcess :: Text
  , latexProcesses :: Map Text MathLaTeXProcess
  } deriving Generic

instance ToJSON Config where
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
    adjust (Object x) (Object y) = Object $ unionWith adjust x y
    adjust _ y = y

defaultConfig :: Config
defaultConfig = Config
  { name = ""
  , sources = fromList
    [ (Zettelkasten, Source
      { mount = ["zettel"]
      , rawInclude = ["**/*"]
      , exclude = defExclude
      , serve = "/zettel"
      })
    , (Content, Source
      { mount = ["content"]
      , rawInclude = ["**/*"]
      , exclude = defExclude
      , serve = "/content"
      })
    , (CommonAssets, Source
      { mount = ["assets"]
      , rawInclude = ["**/*"]
      , exclude = defExclude
      , serve = "/assets"
      })
    ]
  , templates = ["templates"]
  , defaultLatexProcess = "latex"
  , latexProcesses = singleton "latex" MathLaTeXProcess
    { preamble = "\\documentclass{article}"
    , imageInputType = "dvi"
    , imageOutputType = "svg"
    , imageSizeAdjust = 1.7
    , latexCompiler = ["latex -interaction nonstopmode -output-directory %o %f"]
    , imageConverter = ["dvisvgm %f -n -b min -c %S -o %O"]
    }
  } where defExclude = ["**/.*/**/*"]

data SourceKind
  = CommonAssets
  | Zettelkasten
  | Content
  | Templates
  deriving (Eq, Ord, Show, Generic)

instance ToJSON SourceKind where
  toEncoding = genericToEncoding customOptions

instance ToJSONKey SourceKind

instance FromJSON SourceKind where
  parseJSON = genericParseJSON customOptions

instance FromJSONKey SourceKind

data Source = Source
  { mount :: [FilePath]
  , rawInclude :: [FilePattern]
  , exclude :: [FilePattern]
  , serve :: FilePath
  } deriving (Generic)

instance ToJSON Source where
  toEncoding = genericToEncoding customOptions

instance FromJSON Source where
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
  toEncoding = genericToEncoding customOptions

instance FromJSON MathLaTeXProcess where
  parseJSON = genericParseJSON customOptions

customOptions :: Options
customOptions = defaultOptions
                { fieldLabelModifier = hyphenizeCamelCase
                }

hyphenizeCamelCase :: String -> String
hyphenizeCamelCase "" = ""
hyphenizeCamelCase (y : ys) = toLower y : foldr go "" ys
  where
    go c xs | isUpper c = '-' : toLower c : xs
            | otherwise = c : xs

loadConfigWithDefault :: MonadIO m => FilePath -> m Config
loadConfigWithDefault fp = do
  vals :: Value <- Y.decodeFileThrow fp
  pure $ adjustConfig defaultConfig vals
