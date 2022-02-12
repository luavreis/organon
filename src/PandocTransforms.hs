-- |

module PandocTransforms where
import PandocTransforms.Utilities hiding (getModificationTime)
import PandocTransforms.LaTeX
import PandocTransforms.Org
import PandocTransforms.Links
import Caching
import Text.Pandoc.Citeproc (processCitations)
import UnliftIO (MonadUnliftIO)
import System.FilePath
import Control.Monad.Logger
import qualified Models as M
import PandocTransforms.Emojis (convertEmojis)

-- | Types

data RawFormat = Image | Html | Other
  deriving (Eq,Ord,Show)

data MarkupFormat = Org | Md | Raw RawFormat
  deriving (Eq,Ord,Show)

-- | Walkable filters

transforms :: [Pandoc -> Pandoc]
transforms =
  [ headerShift 1
  , setMetaP "lang" "pt-BR"
  , setMetaP "csl" "data/citstyle.csl"
  , walk convertEmojis
  ]

convertIO
  :: (MonadUnliftIO m, MonadLogger m)
  => M.Place
  -> MarkupFormat
  -> Text
  -> CacheT M.Model m (Text, Text)
convertIO place _ txt = do
  let
    realFp = M.filepath place
    dir = takeDirectory realFp

  doc' <- liftIO $ runIOorExplode $ do
    setResourcePath [".", dir]
    parsed <- readOrg readerOptions [(realFp, txt)]
              <&> applyTransforms
              <&> setOrgVars
    if isJust . lookupMeta "bibliography" . getMeta $ parsed
    then processCitations parsed
    else pure parsed

  doc <- doc'
         & renderLaTeX dir
         >>= handleOrgLinks place

  let result = do
        let meta = getMeta doc
        title <- writeHtml $ Pandoc meta [Plain (docTitle meta)]
        body  <- writeHtml doc
        pure (title, body)

  return $ case runPure result of
    Left e -> error (show e)
    Right t -> t
  where
    applyTransforms p = foldr ($) p transforms
