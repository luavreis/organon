module Site.Org.LaTeX.Types where

import Data.Binary (Binary)
import Data.Binary.Instances.UnorderedContainers ()
import Site.Org.JSON

type LaTeXCache = HashMap (Text, Text, FilePath, LaTeXOptions) (Text, ByteString)

data MathLaTeXProcess = MathLaTeXProcess
  { preamble :: Text,
    imageInputType :: String,
    imageOutputType :: String,
    imageMIMEType :: Text,
    imageSizeAdjust :: Float,
    latexCompiler :: [Text],
    imageConverter :: [Text]
  }
  deriving (Eq, Ord, Show, Generic, Binary, Hashable)

instance ToJSON MathLaTeXProcess where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON MathLaTeXProcess where
  parseJSON = genericParseJSON customOptions

data LaTeXOptions = LaTeXOptions
  { defaultLatexProcess :: Text,
    latexProcesses :: Map Text MathLaTeXProcess
  }
  deriving (Eq, Ord, Show, Generic, Binary, Hashable)

instance ToJSON LaTeXOptions where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON LaTeXOptions where
  parseJSON = genericParseJSON customOptions
