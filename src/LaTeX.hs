{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
-- |

module LaTeX where
import Place
import Org.Types
import NeatInterpolation
import qualified Data.Map as M
import qualified Data.Text as T
import System.FilePath ((-<.>), (</>), takeDirectory)
import UnliftIO (IOException, catch, MonadUnliftIO, withSystemTempDirectory, hClose)
import UnliftIO.Process
import Control.Monad.Logger (logErrorNS, MonadLogger)
import Data.Text.IO (hPutStr)
import Data.ByteString.Base64 (encodeBase64)
import Data.Generics.Product
import Data.Binary.Instances.UnorderedContainers ()
import JSON
import Org.Walk
import Control.Exception (throw)
import Data.Bitraversable (bimapM)
import System.Exit
import System.IO (openTempFile)
import Data.Binary (Binary)
import Data.HashMap.Strict (lookup, insert)

type LaTeXCache = HashMap (Text, LaTeXOptions) (Text, ByteString)

data MathLaTeXProcess = MathLaTeXProcess
  { preamble :: Text
  , imageInputType :: String
  , imageOutputType :: String
  , imageMIMEType :: Text
  , imageSizeAdjust :: Float
  , latexCompiler :: [Text]
  , imageConverter :: [Text]
  }
  deriving (Eq, Ord, Show, Generic, Binary, Hashable)

instance ToJSON MathLaTeXProcess where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON MathLaTeXProcess where
  parseJSON = genericParseJSON customOptions

data LaTeXOptions = LaTeXOptions
  { defaultLatexProcess :: Text
  , latexProcesses :: Map Text MathLaTeXProcess
  }
  deriving (Eq, Ord, Show, Generic, Binary, Hashable)

instance ToJSON LaTeXOptions where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON LaTeXOptions where
  parseJSON = genericParseJSON customOptions

getPreamble :: OrgDocument -> Text
getPreamble d = T.unlines . mapMaybe justText $ lookupKeyword "latex_header" d
  where
    justText (ValueKeyword _ t) = Just t
    justText _ = Nothing

preambilizeKaTeX :: forall m. (MonadUnliftIO m, MonadLogger m) => Place -> OrgDocument -> m OrgElement
preambilizeKaTeX plc doc = preambleBlock
  where
    hide :: KeywordValue
    hide = BackendKeyword [("style", "display: none")]

    mathOp = "\\newcommand{\\DeclareMathOperator}[2]{\\newcommand{#1}{\\operatorname{#2}}}"

    preambleBlock :: m OrgElement
    preambleBlock = do
      preamble' <- filteredPreamble
      pure $ GreaterBlock (M.singleton "attr_html" hide) (Special "hide") [ExportBlock "html" preamble']

    filteredPreamble :: m Text
    filteredPreamble = do
      lines' <- unlines . (mathOp :) . filter filt . concat
                <$> mapM includeInput (lines $ getPreamble doc)
      return [text|
                <span class="math display">
                \[
                $lines'
                \]
                </span>
             |]

    includeInput :: Text -> m [Text]
    includeInput txt
      | "\\input" `T.isPrefixOf` txt = do
          let path = do
                withNoPrefix <- T.stripPrefix "\\input{" txt
                bare <- T.stripSuffix "}" $ T.stripEnd withNoPrefix
                pure $ toString bare -<.> "tex"
          file <- mapM (readFileBS . (takeDirectory (absolute plc) </>)) path
            `catch` (\(e :: IOException) -> do
                        logErrorNS "Preamble Loading" (show e)
                        pure Nothing)
          return $ maybe [] (lines . decodeUtf8) file
      | otherwise = return [txt]

    filt :: Text -> Bool
    filt txt =  "\\newcommand" `T.isPrefixOf` txt
             || "\\renewcommand" `T.isPrefixOf` txt
             || "\\def" `T.isPrefixOf` txt
             || "\\DeclareMathOperator" `T.isPrefixOf` txt


renderLaTeX ::
  forall m.
  (MonadUnliftIO m, MonadLogger m, HasCallStack) =>
  Place ->
  MathLaTeXProcess ->
  Text ->
  Text ->
  m (Text, ByteString)
renderLaTeX place process preamble' txt = do
  withSystemTempDirectory "tex-conversion" $ \tmpDir -> do
    (texInFp, texInH) <- liftIO $ openTempFile tmpDir "orgtex.tex"
    liftIO $ hPutStr texInH finalText
    hClose texInH
    -- Here I'm reimplementing Org's `org-compile-file`, which mandates that the
    -- process produces exactly the same filepath but with replaced extension.
    let texOutFp = texInFp -<.> imageInputType process
    forM_ (latexCompiler process) \ cmd ->
        callCreateProcess (shell . toString $
          cmd & T.replace "%o" (toText tmpDir)
              & T.replace "%O" (toText texOutFp)
              & T.replace "%f" (toText texInFp)) { cwd = Just dir }
    let iOutFp = texOutFp -<.> imageOutputType process
    forM_ (imageConverter process) \ cmd ->
      callCreateProcess (shell . toString $
        cmd & T.replace "%o" (toText tmpDir)
            & T.replace "%O" (toText iOutFp)
            & T.replace "%f" (toText texOutFp)
            & T.replace "%S" (show $ imageSizeAdjust process)) { cwd = Just dir }
    (imageMIMEType process,) <$> readFileBS iOutFp
  `catch` (\(_ :: IOException) -> do
      logErrorNS "LaTeX Rendering" ("Failed to render LaTeX in file " <> toText (absolute place))
      logErrorNS "LaTeX Rendering" (T.strip $ "LaTeX content:\n" <> finalText)
      pure (imageMIMEType process, ""))
  where
    finalText =
      let pPreamble = preamble process
      in [text|
          $pPreamble
          $preamble'
          \begin{document}
          $txt
          \end{document}
        |]
    dir = takeDirectory (absolute place)
    callCreateProcess :: CreateProcess -> m ()
    callCreateProcess p = do
      (ecode, out, err) <- readCreateProcessWithExitCode p ""
      case ecode of
        ExitSuccess -> pure ()
        ExitFailure _ -> do
          logErrorNS "LaTeX Rendering" ("Error in process " <> show (cmdspec p))
          logErrorNS "LaTeX Rendering" (T.strip $ "STDOUT:\n" <> toText out)
          logErrorNS "LaTeX Rendering" (T.strip $ "STDERR:\n" <> toText err)

-- | Encode Base64 data in link.
makeDataURI :: (Text, ByteString) -> LinkTarget
makeDataURI (mime, d) = URILink "data" (mime <> ";base64," <> encodeBase64 d)

newtype UndefinedLaTeXProcess = UndefinedLaTeXProcess Text
  deriving stock (Show)
  deriving anyclass (Exception)

processLaTeX ::
  forall model n m k.
  (HasType LaTeXCache model) =>
  (HasType LaTeXOptions n) =>
  (MonadLogger m, MonadReader n m) =>
  (MonadUnliftIO k, MonadLogger k) =>
  Place ->
  OrgDocument ->
  m (model -> k (OrgDocument, model))
processLaTeX plc doc = do
  opt <- asks getTyped
  let pname = defaultLatexProcess opt -- TODO read from metadata
  process <-
    case M.lookup pname (latexProcesses opt) of
      Just p -> pure p
      Nothing -> do
        logErrorNS "" $ "LaTeX process \"" <> pname <> "\" is not defined."
        throw (UndefinedLaTeXProcess pname)
  let
    mapping' :: (Walkable OrgElement a, Walkable OrgInline a) => a -> StateT LaTeXCache k a
    mapping' = walkM wBlocks >=> walkM wInlines

    doProcess :: Text -> StateT LaTeXCache k (Text, ByteString)
    doProcess txt = do
      cache <- get
      case lookup (txt, opt) cache of
        Just cached -> pure cached
        Nothing -> do
          result <- lift $ renderLaTeX plc process (getPreamble doc) txt
          modify (insert (txt, opt) result)
          pure result

    wInlines :: OrgInline -> StateT LaTeXCache k OrgInline
    wInlines (ExportSnippet "latex" s) =
      Image . makeDataURI <$> doProcess s
    wInlines x = pure x

    wBlocks :: OrgElement -> StateT LaTeXCache k OrgElement
    wBlocks (ExportBlock "latex" s) =
      Paragraph mempty . one . Image . makeDataURI <$> doProcess s
    wBlocks (LaTeXEnvironment _ name s)
      | name `notElem` leaveToKaTeX =
      Paragraph mempty . one . Image . makeDataURI <$> doProcess s
      where
        leaveToKaTeX = ["equation", "equation*", "align", "align*", "aligned"]
    wBlocks x = pure x

  pure \m -> do
    (x, y) <- flip runStateT (getTyped m) $ do
      (child', sect') <- bimapM mapping' mapping' (documentContent doc)
      pure doc { documentChildren = child', documentSections = sect' }
    pure (x, setTyped y m)
