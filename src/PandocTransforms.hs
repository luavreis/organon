-- |

module PandocTransforms where
import PandocTransforms.Utilities hiding (getModificationTime)
import PandocTransforms.LaTeX
import PandocTransforms.Org
import Caching
import Text.Pandoc.Builder as B (setMeta, image, str)
import Text.Pandoc.Shared (headerShift, isURI)
import Text.Pandoc.Citeproc (processCitations)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Directory
import System.FilePath
import Text.Emoji
import Control.Monad.Logger
import Control.Monad (foldM)
import System.Exit
import qualified Data.Text as T

-- | Types

data MarkupFormat
  = Org
  | Html
  | Md
  deriving (Eq,Ord,Show)

-- | Walkable filters

-- I am not sure how to implement this. The problem is that I would have to
-- implement some sort of crazy algorithm to make combining emojis work, as
-- they span multiple characters... so for now all emoji must be separated
-- by whitepace!
convertEmojis :: Inline -> Inline
convertEmojis (Str s) = case aliasesFromEmoji s of
  Nothing -> Str s
  Just [] -> Str s
  Just (a : _) ->
    RawInline (Format "html") ("<i class=\"twa twa-"
                               <> fixAlias a
                               <> "\"></i>")
  where
    fixAlias a = T.map (\case '_' -> '-'; x -> x) a
convertEmojis x = x

fixLink' root p =
  if isAbsolute (toString p) || isURI p
  then p
  else toText (normalise $ root </> toString p)

fixLink :: FilePath -> Text -> Text
fixLink root p =
  let p' = if liftT takeExtension p `elem` strippedFmts
           then liftT dropExtension p
           else p
  in case T.stripPrefix "file:" p' of
    Nothing -> fixLink' root p'
    Just c -> fixLink' root c
  where
    strippedFmts = [".org", ".md"]
    liftT f = toText . f . toString

fixLinks :: FilePath -> Inline -> Inline
fixLinks fp (Image f i (u,t)) = Image f i (fixLink' fp u, t)
fixLinks fp (Link a i (u,t)) = Link a i (fixLink fp u, t)
fixLinks fp x = x

getMeta :: Pandoc -> Meta
getMeta (Pandoc meta _) = meta

setMetaP :: Text -> Text -> Pandoc -> Pandoc
setMetaP k l (Pandoc meta blocks) =
  Pandoc (B.setMeta k l meta) blocks

transforms :: [Pandoc -> Pandoc]
transforms =
  [ walk convertEmojis
  , headerShift 1
  , setMetaP "lang" "pt-BR"
  , setMetaP "csl" "data/citstyle.csl"
  ]

readerOptions :: ReaderOptions
readerOptions = def {
  readerExtensions =
      extensionsFromList
      [ Ext_citations
      , Ext_smart
      ]
  }

writerOptions :: WriterOptions
writerOptions =
  def
  { writerHTMLMathMethod = MathJax ""
  -- , writerExtensions =
  --     extensionsFromList
  --     [ Ext_citations
  --     ]
  }

writeHtml :: PandocMonad m => Pandoc -> m Text
writeHtml = writeHtml5String writerOptions

addToFileTree' :: FilePath -> FilePath -> FileTree -> IO FileTree
addToFileTree' root fp tree = do
  isdir <- doesDirectoryExist fp
  if isdir
     then do -- recursively add contents of directories
       let isSpecial ".." = True
           isSpecial "."  = True
           isSpecial _    = False
       fs <- map (fp </>) . filter (not . isSpecial) <$> getDirectoryContents fp
       foldM (flip $ addToFileTree' root) tree fs
     else do
       contents <- readFileBS fp
       mtime <- getModificationTime fp
       let info = FileInfo{ infoFileMTime = mtime, infoFileContents = contents }
       return $ case stripFilePrefix root fp of
         Just s  -> insertInFileTree s info tree
         Nothing -> tree

convertIO
  :: (MonadUnliftIO m, MonadLogger m)
  => FilePath
  -> MarkupFormat
  -> Text
  -> CacheT m (Text, Text)
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
