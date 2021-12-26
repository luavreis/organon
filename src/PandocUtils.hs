{-# LANGUAGE QuasiQuotes #-}
-- |

module PandocUtils where
import qualified Data.Text as T
import NeatInterpolation (text)
import System.FilePath
import UnliftIO.Directory as D
import Text.Emoji
import Text.Pandoc
import Text.Pandoc.Builder as B (setMeta, image, str)
import Text.Pandoc.Shared (headerShift, isURI)
import Text.Pandoc.Walk
import Text.Regex.TDFA
import Text.Pandoc.Citeproc (processCitations)
import Data.List (stripPrefix)
import Text.Pandoc.SelfContained (makeDataURI)
import Caching
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger
import Control.Monad (foldM)
import Control.Monad.Catch
import UnliftIO.Process
import UnliftIO.Temporary
import qualified Data.Traversable as T
import qualified UnliftIO as UIO
import System.Exit

-- | Types

data MarkupFormat
  = Org
  | Html
  | Md
  deriving (Eq,Ord,Show)

-- | Utilities

stripFilePrefix :: FilePath -> FilePath -> Maybe FilePath
stripFilePrefix f g = stripPrefix (addTrailingPathSeparator $ normalise f) (normalise g)

isMathEnvironment :: Text -> Bool
isMathEnvironment s = "\\begin{" `T.isPrefixOf` s &&
                         envName `elem` mathmlenvs
  where envName = T.takeWhile (/= '}') (T.drop 7 s)
        mathmlenvs = [ "align"
                     , "align*"
                     , "alignat"
                     , "alignat*"
                     , "aligned"
                     , "alignedat"
                     , "array"
                     , "Bmatrix"
                     , "bmatrix"
                     , "cases"
                     , "CD"
                     , "eqnarray"
                     , "eqnarray*"
                     , "equation"
                     , "equation*"
                     , "gather"
                     , "gather*"
                     , "gathered"
                     , "matrix"
                     , "multline"
                     , "multline*"
                     , "pmatrix"
                     , "smallmatrix"
                     , "split"
                     , "subarray"
                     , "Vmatrix"
                     , "vmatrix" ]

-- | Concurrent walkM

pooledWalk
  :: (MonadUnliftIO m, Walkable a b, Traversable t)
  => Int
  -> (a -> m a)
  -> t b -> m (t b)
pooledWalk n f = UIO.pooledMapConcurrentlyN n (walkM f)

-- | Walkable filters

defLaTeXpackages :: [(Text, [Text])]
defLaTeXpackages =
  [ ("graphicx", [])
  , ("longtable", [])
  , ("rotating", [])
  , ("ulem", ["normalem"])
  , ("amsmath", [])
  , ("amssymb", [])
  ]

svgLaTeX
  :: forall m. (MonadUnliftIO m, MonadLogger m)
  => Text -> Text -> m ByteString
svgLaTeX preamble c = do
  let code =
        [text|
          \documentclass[varwidth]{standalone}
          $packages
          $preamble
          \begin{document}
          $c
          \end{document}
        |]
      packages = foldr (\(p,o) ->
                          (<> "\\usepackage["
                           <> T.intercalate "," o
                           <> "]{" <> p <> "}\n"))
                 "" defLaTeXpackages

      process :: FilePath -> m ByteString
      process tempDir = do
        let outpath = tempDir </> "out.svg"
        (c,_,e) <- readCreateProcessWithExitCode
          (shell
           $ "tectonic -r 0 -o "
           <> tempDir
           <> " - > /dev/null; pdf2svg "
           <> (tempDir </> "texput.pdf")
           <> " "
           <> outpath)
          (toString code)
        case c of
          ExitSuccess ->
            readFileBS outpath
          ExitFailure _ -> do
            logErrorNS "LaTeX Conversion" $ toText e
            pure mempty
  withSystemTempDirectory "tex-conversion" process

svgImage :: ByteString -> Inline
svgImage s = Image nullAttr [] (uri, "")
  where
    uri = makeDataURI ("image/svg+xml", s)

renderLaTeX :: (MonadUnliftIO m, MonadLogger m)
  => Pandoc -> CacheT m Pandoc
renderLaTeX (Pandoc meta blocks) =
  Pandoc meta <$> pooledWalk 6 (wBlocks preamble) blocks
  where
    preamble =
      lookupMeta "header-includes" meta
      & maybe "" (queryMetaValue query)
      where
        query :: Inline -> Text
        query (RawInline (Format "latex") s) = s <> "\n"
        query _ = T.empty

    cachedSvgLaTeX :: (MonadLogger m, MonadUnliftIO m)
      => Text -> CacheT m ByteString
    cachedSvgLaTeX = fromCacheOrCompute (svgLaTeX preamble)

    wInlines :: (MonadUnliftIO m, MonadLogger m)
      => Text -> Inline -> CacheT m Inline
    wInlines preamble (RawInline (Format "latex") s) =
      svgImage <$> cachedSvgLaTeX s
    wInlines _ x = pure x

    wBlocks :: (MonadUnliftIO m, MonadLogger m)
      => Text -> Block -> CacheT m Block
    wBlocks preamble (RawBlock (Format "latex") s) =
      if isMathEnvironment s
      then pure $ RawBlock (Format "latex") s
      else Para . one . svgImage <$> cachedSvgLaTeX s
    wBlocks preamble x = walkM (wInlines preamble) x


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

extractOrgVar :: Block -> [(Text,Text)]
extractOrgVar (RawBlock "org" s) =
  let (_,_,_,m) = s =~ ("^#\\+([[:alpha:]]+): *\"?([^\"]+)\"?$" :: Text) :: (Text,Text,Text,[Text])
  in case m of
    [a,b] -> [(a,b)]
    _ -> []
extractOrgVar _ = []

extractOrgVars :: Pandoc -> [(Text,Text)]
extractOrgVars = query extractOrgVar

getMeta :: Pandoc -> Meta
getMeta (Pandoc meta _) = meta

setMetaP :: Text -> Text -> Pandoc -> Pandoc
setMetaP k l (Pandoc meta blocks) =
  Pandoc (B.setMeta k l meta) blocks

setOrgVars :: Pandoc -> Pandoc
setOrgVars p =
  let Pandoc meta blocks = p
      vars = extractOrgVars p
      meta' = foldr (uncurry B.setMeta) meta vars
  in Pandoc meta' blocks

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
       mtime <- D.getModificationTime fp
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
