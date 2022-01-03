-- |

module PandocTransforms where
import PandocTransforms.Utilities hiding (getModificationTime)
import PandocTransforms.LaTeX
import PandocTransforms.Org
import PandocTransforms.Links
import PandocTransforms.Emojis
import Caching
import Text.Pandoc.Citeproc (processCitations)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Directory
import System.FilePath
import Control.Monad.Logger

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
  [ walk convertEmojis
  , headerShift 1
  , setMetaP "lang" "pt-BR"
  , setMetaP "csl" "data/citstyle.csl"
  ]

convertIO
  :: (MonadUnliftIO m, MonadLogger m)
  => FilePath
  -> MarkupFormat
  -> Text
  -> CacheT model m (Text, Text)
convertIO fp ext fileText = do
  let dir = takeDirectory fp
      -- We use those two directories by convention.
      impDir = dir </> "imports"
      impDir' = dir </> takeBaseName fp
      linkDir = maybe dir takeDirectory (stripFilePrefix "content" fp)

  dirExists <- doesDirectoryExist impDir
  originalDir <- getCurrentDirectory

  tree <- liftIO $ do
          t <- addToFileTree mempty "data/citstyle.csl"
          setCurrentDirectory dir
          if dirExists
          then addToFileTree t "imports"
          else pure t

  doc <- renderLaTeX $ fromRight (error "") . runPure $ do
        modifyPureState $ \ps -> ps { stFiles = tree }
        parsed <- readOrg readerOptions fileText
                  <&> applyTransforms
                  <&> setOrgVars
                  <&> walk (fixLinks linkDir) -- Maybe we should move this out of this function and instead take a list of transforms
        if isJust . lookupMeta "bibliography" . getMeta $ parsed
        then processCitations parsed
        else pure parsed

  setCurrentDirectory originalDir

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
