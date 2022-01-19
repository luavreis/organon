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

data MarkupFormat
  = Org
  | Html
  | Md
  | Raw
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
  => FilePath
  -> M.Source
  -> MarkupFormat
  -> Text
  -> CacheT model m (Text, Text)
convertIO fp src _ fileText = do
  let dir = takeDirectory fp

  doc' <- liftIO $ runIOorExplode $ do
    setResourcePath [".", dir]
    parsed <- readOrg readerOptions [(fp, fileText)]
              <&> applyTransforms
              <&> setOrgVars
              <&> walk (fixLinks $ M.servePoint src)
    if isJust . lookupMeta "bibliography" . getMeta $ parsed
    then processCitations parsed
    else pure parsed

  doc <- renderLaTeX dir doc'

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
