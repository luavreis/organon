{-# LANGUAGE QuasiQuotes #-}
-- |

module PandocTransforms.LaTeX where
import PandocTransforms.Utilities
import Caching
import Text.Pandoc.SelfContained (makeDataURI)
import System.FilePath (FilePath, (</>))
import UnliftIO
import UnliftIO.Process (readCreateProcessWithExitCode, shell)
import Control.Monad.Logger
import NeatInterpolation (text)
import System.Exit
import qualified Data.Text as T

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
