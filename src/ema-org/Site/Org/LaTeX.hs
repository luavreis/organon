{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Site.Org.LaTeX where

import Control.Exception (throw)
import Control.Monad.Logger (MonadLogger, logErrorNS)
import Data.Bitraversable (bimapM)
import Data.ByteString.Base64 (encodeBase64)
import Data.HashMap.Strict qualified as HM
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO (hPutStr)
import NeatInterpolation
import Optics.Core ((%~))
import Org.Types
import Org.Walk
import Site.Org.Cache
import Site.Org.LaTeX.Types
import System.Exit
import System.FilePath (takeDirectory, (-<.>), (</>))
import System.IO (openTempFile)
import UnliftIO (IOException, MonadUnliftIO, catch, hClose, modifyTVar, pooledMapConcurrentlyN, withSystemTempDirectory)
import UnliftIO.Process
import Org.Exporters.Processing.OrgData (OrgData, keywords)

latexHeader :: OrgData -> Text
latexHeader d = fromMaybe "" $ M.lookup "latex_header" (keywords d)

getKaTeXPreamble ::
  forall m.
  (MonadUnliftIO m, MonadLogger m) =>
  -- | Document's filepath
  FilePath ->
  OrgData ->
  m Text
getKaTeXPreamble fp datum = filteredPreamble
  where
    mathOp = "\\providecommand{\\DeclareMathOperator}[2]{\\providecommand{#1}{\\operatorname{#2}}}"

    filteredPreamble :: m Text
    filteredPreamble =
      unlines . (mathOp :) . mapMaybe filt . concat
        <$> mapM includeInput (lines $ latexHeader datum)

    includeInput :: Text -> m [Text]
    includeInput (T.strip -> txt)
      | "\\input" `T.isPrefixOf` txt = do
          let path = do
                withNoPrefix <- T.stripPrefix "\\input{" txt
                bare <- T.stripSuffix "}" $ T.stripEnd withNoPrefix
                pure $ toString bare -<.> "tex"
          file <-
            mapM (readFileBS . (takeDirectory fp </>)) path
              `catch` ( \(e :: IOException) -> do
                          logErrorNS "Preamble Loading" (show e)
                          pure Nothing
                      )
          return $ maybe [] (lines . decodeUtf8) file
      | otherwise = return [txt]

    filt :: Text -> Maybe Text
    filt t =
      if
          | Just txt <- "\\newcommand" `T.stripPrefix` t ->
              Just $ "\\providecommand" <> txt
          | "\\providecommand" `T.isPrefixOf` t
              || "\\renewcommand" `T.isPrefixOf` t
              || "\\def" `T.isPrefixOf` t
              || "\\DeclareMathOperator" `T.isPrefixOf` t ->
              Just t
          | otherwise -> Nothing

processKaTeX ::
  (MonadUnliftIO m, MonadLogger m) =>
  -- | Document's filepath
  FilePath ->
  (OrgDocument, OrgData) ->
  m (OrgDocument, OrgData)
processKaTeX fp (doc, datum) = do
  preambl <- getKaTeXPreamble fp datum
  pure (doc & #documentProperties %~ M.insert "katex_preamble" preambl, datum)

renderLaTeX ::
  forall m.
  (MonadUnliftIO m, MonadLogger m, HasCallStack) =>
  -- | Document's file path.
  FilePath ->
  MathLaTeXProcess ->
  Text ->
  Text ->
  m (Text, ByteString)
renderLaTeX path process preamble' txt =
  do
    withSystemTempDirectory "tex-conversion" $ \tmpDir -> do
      (texInFp, texInH) <- liftIO $ openTempFile tmpDir "orgtex.tex"
      liftIO $ hPutStr texInH finalText
      hClose texInH
      -- Here I'm reimplementing Org's `org-compile-file`, which mandates that the
      -- process produces exactly the same filepath but with replaced extension.
      let texOutFp = texInFp -<.> imageInputType process
      forM_ (latexCompiler process) \cmd ->
        callCreateProcess
          ( shell . toString $
              cmd & T.replace "%o" (toText tmpDir)
                & T.replace "%O" (toText texOutFp)
                & T.replace "%f" (toText texInFp)
          )
            { cwd = Just dir
            }
      let iOutFp = texOutFp -<.> imageOutputType process
      forM_ (imageConverter process) \cmd ->
        callCreateProcess
          ( shell . toString $
              cmd & T.replace "%o" (toText tmpDir)
                & T.replace "%O" (toText iOutFp)
                & T.replace "%f" (toText texOutFp)
                & T.replace "%S" (show $ imageSizeAdjust process)
          )
            { cwd = Just dir
            }
      (imageMIMEType process,) <$> readFileBS iOutFp
    `catch` ( \(_ :: IOException) -> do
                logErrorNS "LaTeX Rendering" ("Failed to render LaTeX in file " <> toText path)
                logErrorNS "LaTeX Rendering" (T.strip $ "LaTeX content:\n" <> finalText)
                pure (imageMIMEType process, "")
            )
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
    dir = takeDirectory path
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
  forall m.
  (MonadUnliftIO m, MonadLogger m) =>
  -- | Document's file path.
  LaTeXOptions ->
  TVar Cache ->
  FilePath ->
  (OrgDocument, OrgData) ->
  m (OrgDocument, OrgData)
processLaTeX opt cVar path (doc, datum) = do
  let pname = defaultLatexProcess opt -- TODO read from metadata
  process <-
    case M.lookup pname (latexProcesses opt) of
      Just p -> pure p
      Nothing -> do
        logErrorNS "LaTeX process" $ "LaTeX process \"" <> pname <> "\" is not defined."
        throw (UndefinedLaTeXProcess pname)
  let doProcess :: Text -> m (Text, ByteString)
      doProcess txt = do
        cache <- readTVarIO cVar
        let lcache = latexCache cache
            lHeader = latexHeader datum
            ckey = (lHeader, txt, path, opt)
        case HM.lookup ckey lcache of
          Just cached -> pure cached
          Nothing -> do
            result <- renderLaTeX path process lHeader txt
            atomically $ modifyTVar cVar (#latexCache %~ HM.insert ckey result)
            pure result

      wContent :: OrgContent -> m OrgContent
      wContent = bimapM (pooledMapConcurrentlyN 8 wAll) (pooledMapConcurrentlyN 4 wSection)

      wSection :: OrgSection -> m OrgSection
      wSection = mapSectionContentM wContent

      wAll :: OrgElement -> m OrgElement
      wAll = buildMultiW \f list ->
        list .> wBlock f .> wInline f

      wInline :: WalkM m -> OrgObject -> m OrgObject
      wInline _ (ExportSnippet "latex" s) =
        Image . makeDataURI <$> doProcess s
      wInline f x = f x

      wBlock :: WalkM m -> OrgElement -> m OrgElement
      wBlock _ (ExportBlock "latex" s) =
        Paragraph mempty . one . Image . makeDataURI <$> doProcess s
      wBlock _ (LaTeXEnvironment _ name s)
        | name `notElem` leaveToKaTeX =
            Paragraph mempty . one . Image . makeDataURI <$> doProcess s
        where
          leaveToKaTeX = ["equation", "equation*", "align", "align*", "aligned"]
      wBlock f x = f x

  (, datum) <$> mapContentM wContent doc
