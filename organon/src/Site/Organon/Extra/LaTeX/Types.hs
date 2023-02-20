module Site.Organon.Extra.LaTeX.Types (
  LaTeXProcessSpec (..),
  LaTeXOptions (..),
  defLaTeXOptions,
  LaTeXCacheKey,
  LaTeXCacheVal,
  lookupLaTeXCache,
  insertLaTeXCache,
)
where

import Data.Binary (Binary, decode, encode)
import Data.HashMap.Strict qualified as HMap
import Data.Map qualified as Map
import Site.Org.Utils.JSON
import Site.Organon.Cache

data LaTeXProcessSpec = LaTeXProcessSpec
  { preamble :: Text
  , imageInputType :: String
  , imageOutputType :: String
  , imageMIMEType :: Text
  , imageSizeAdjust :: Float
  , latexCompiler :: [Text]
  , imageConverter :: [Text]
  }
  deriving (Eq, Ord, Show, Generic, Binary, Hashable)

instance ToJSON LaTeXProcessSpec where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON LaTeXProcessSpec where
  parseJSON = genericParseJSON customOptions

data LaTeXOptions = LaTeXOptions
  { defaultProcess :: Text
  , processes :: Map Text LaTeXProcessSpec
  }
  deriving (Eq, Ord, Show, Generic, Binary, Hashable)

instance ToJSON LaTeXOptions where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON LaTeXOptions where
  parseJSON = genericParseJSON customOptions

defLaTeXOptions :: LaTeXOptions
defLaTeXOptions =
  LaTeXOptions
    { defaultProcess = "latex"
    , processes =
        Map.singleton
          "latex"
          LaTeXProcessSpec
            { preamble = "\\documentclass{article}"
            , imageInputType = "dvi"
            , imageOutputType = "svg"
            , imageMIMEType = "image/svg+xml"
            , imageSizeAdjust = 1.7
            , latexCompiler = ["latex -interaction nonstopmode -output-directory %o %f"]
            , imageConverter = ["dvisvgm %f -n -b min -c %S -o %O"]
            }
    }

-- * Cache stuff

type LaTeXCacheKey = (Text, FilePath, LaTeXProcessSpec)

type LaTeXCacheVal = (Text, ByteString)

lookupLaTeXCache :: LaTeXCacheKey -> Cache -> Maybe LaTeXCacheVal
lookupLaTeXCache key Cache {cacheStore = store} =
  decode <$> HMap.lookup (encode key) store

insertLaTeXCache :: LaTeXCacheKey -> LaTeXCacheVal -> Cache -> Cache
insertLaTeXCache key val Cache {cacheStore = store} =
  Cache {cacheStore = HMap.insert (encode key) (encode val) store}
