{-# LANGUAGE QuasiQuotes #-}
-- |

module PandocTransforms.LaTeX where
import PandocTransforms.Utilities
import Caching
import Text.Pandoc.SelfContained (makeDataURI)
import System.FilePath ((</>), (-<.>))
import UnliftIO
import UnliftIO.Process (readCreateProcessWithExitCode, shell)
import Control.Monad.Logger
import NeatInterpolation (text)
import System.Exit
import qualified Data.Text as T
import qualified Data.ByteString as B

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

defLaTeXpackages :: [(Text, [Text])]
defLaTeXpackages =
  [ ("graphicx", [])
  , ("longtable", [])
  , ("rotating", [])
  , ("ulem", ["normalem"])
  , ("amsmath", [])
  , ("amssymb", [])
  , ("tikz", [])
  , ("tikz-cd", [])
  ]

getPreamble :: Meta -> Text
getPreamble meta =
  lookupMeta "header-includes" meta
  & maybe "" (queryMetaValue queryPreamble)
  where
    queryPreamble :: Inline -> Text
    queryPreamble (RawInline (Format "latex") s) = s <> "\n"
    queryPreamble _ = T.empty

svgLaTeX
  :: forall m. (MonadUnliftIO m, MonadLogger m)
  => Text -> Text -> m (Maybe ByteString)
svgLaTeX preamble body = do
  let code =
        [text|
          \documentclass[varwidth]{standalone}
          $packages
          $preamble
          \begin{document}
          $body
          \end{document}
        |]
      packages = foldr (\(p,o) ->
                          (<> "\\usepackage["
                           <> T.intercalate "," o
                           <> "]{" <> p <> "}\n"))
                 "" defLaTeXpackages

      process :: FilePath -> m (Maybe ByteString)
      process tempDir = do
        let outpath = tempDir </> "out.svg"
        (exitc, _, e) <- readCreateProcessWithExitCode
          (shell
           $ "tectonic -r 0 -o "
           <> tempDir
           <> " - > /dev/null; pdf2svg "
           <> (tempDir </> "texput.pdf")
           <> " "
           <> outpath)
          (toString code)
        case exitc of
          ExitSuccess -> Just <$> readFileBS outpath
          ExitFailure _ -> do
            logErrorNS "LaTeX conversion" $ toText e
            logErrorNS "LaTeX conversion" $ "Problematic file:\n" <> code
            return Nothing
  withSystemTempDirectory "tex-conversion" process

svgImage :: ByteString -> Inline
svgImage s = Image nullAttr [] (uri, "")
  where
    uri = makeDataURI ("image/svg+xml", scaled)
    scaled = fromMaybe s $ do
      let (ini, s')   = B.breakSubstring "width=\"" s
          (w, s'')    = B.breakSubstring "pt\"" s'
          (mid, s''') = B.breakSubstring "height=\"" s''
          (h, rest)   = B.breakSubstring "pt\"" s'''
          factor = 1.3
      width  :: Float <- readMaybe $ decodeUtf8 $ B.drop 7 w
      height :: Float <- readMaybe $ decodeUtf8 $ B.drop 8 h
      return $ ini <> "width=\"" <> (show (width * factor))
            <> mid <> "height=\"" <> (show (height * factor)) <> rest

renderLaTeX :: (MonadUnliftIO m, MonadLogger m)
  => Pandoc -> CacheT model m Pandoc
renderLaTeX (Pandoc meta blocks) = do
  Pandoc meta <$> (lift $ pooledWalk 8 wInlines =<< pooledWalk 8 wBlocks blocks)
  where
    cachedSvgLaTeX :: (MonadLogger m, MonadUnliftIO m)
      => Text -> MReaderT Cache m ByteString
    cachedSvgLaTeX = fmap (fromMaybe mempty) . fromCacheOrCompute (lift . (svgLaTeX $ getPreamble meta))

    wInlines :: (MonadUnliftIO m, MonadLogger m)
      => Inline -> MReaderT Cache m Inline
    wInlines (RawInline (Format "latex") s) =
      svgImage <$> cachedSvgLaTeX s
    wInlines x = pure x

    wBlocks :: (MonadUnliftIO m, MonadLogger m)
      => Block -> MReaderT Cache m Block
    wBlocks (RawBlock (Format "latex") s) =
      if isMathEnvironment s
      then pure $ RawBlock (Format "latex") s
      else Para . one . svgImage <$> cachedSvgLaTeX s
    wBlocks x = pure x

preambilizeKaTeX :: forall m. PandocMonad m => Pandoc -> m Pandoc
preambilizeKaTeX (Pandoc meta blks) = Pandoc meta . (: blks) <$> preamble
  where
    hide :: Attr
    hide = ("", [], [("style", "display: none")])

    mathOp = "\\newcommand{\\DeclareMathOperator}[2]{\\newcommand{#1}{\\operatorname{#2}}}"

    preamble :: m Block
    preamble = do
      preamble' <- filteredPreamble
      pure $ Div hide [RawBlock "html" preamble']

    filteredPreamble :: m Text
    filteredPreamble = do
      lines' <- unlines . (mathOp :) . filter filt . concat
                <$> (mapM includeInput $ lines $ getPreamble meta)
      return $ [text|
                  <span class="math display">
                  \[
                  $lines'
                  \]
                  </span>
               |]

    includeInput :: Text -> m [Text]
    includeInput txt
      | "\\input" `T.isPrefixOf` txt = do
          file <- runMaybeT $ do
            withNoPrefix <- hoistMaybe $ T.stripPrefix "\\input{" txt
            file <- hoistMaybe $ T.stripSuffix "}" $ T.stripEnd withNoPrefix
            readFileLazy (toString file -<.> "tex")
          return $ maybe [txt] (lines . decodeUtf8) file
      | otherwise = return [txt]

    filt :: Text -> Bool
    filt txt =  "\\newcommand" `T.isPrefixOf` txt
             || "\\renewcommand" `T.isPrefixOf` txt
             || "\\def" `T.isPrefixOf` txt
             || "\\DeclareMathOperator" `T.isPrefixOf` txt
