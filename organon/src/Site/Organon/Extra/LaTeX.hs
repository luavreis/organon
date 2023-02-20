{-# LANGUAGE RankNTypes #-}

module Site.Organon.Extra.LaTeX where

import Control.Monad.Logger (MonadLogger, logErrorNS, logInfoNS)
import Data.Aeson (Result (..), fromJSON)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Base64 (encodeBase64)
import Data.Map qualified as M
import Data.Map qualified as Map
import Data.Text qualified as T
import Ondim.Extra (lookupAttr)
import Ondim.Targets.HTML (HtmlNode)
import Optics.Core ((%~))
import Org.Exporters.Processing.OrgData (OrgData, keywords)
import Org.Types
import Site.Org.Render (Expansion, Ondim, bindingText, callText, liftChildren, liftRenderT, throwCustom, (##))
import Site.Organon.Extra.LaTeX.Render (renderLaTeX)
import Site.Organon.Extra.LaTeX.Types
import Site.Organon.Model (Model (..))
import System.FilePath (takeDirectory, (-<.>), (</>))
import UnliftIO (IOException, MonadUnliftIO, catch, modifyTVar)

latexHeader :: OrgData -> Text
latexHeader d =
  case M.lookup "latex_header" (keywords d) of
    Just (ValueKeyword t) -> t
    _ -> ""

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
          | "\\providecommand"
              `T.isPrefixOf` t
              || "\\renewcommand"
              `T.isPrefixOf` t
              || "\\def"
              `T.isPrefixOf` t
              || "\\DeclareMathOperator"
              `T.isPrefixOf` t ->
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

-- | Encode Base64 data in link.
makeDataURI :: (Text, ByteString) -> Text
makeDataURI (mime, d) = "data:" <> mime <> ";base64," <> encodeBase64 d

specFromModel :: Model -> Ondim LaTeXProcessSpec
specFromModel m =
  maybe (throwCustom err) pure $
    Map.lookup opt.defaultProcess opt.processes
  where
    opt :: LaTeXOptions =
      case fromJSON <$> KM.lookup "latex" m.extraOpts of
        Just (Success x) -> x
        _ -> defLaTeXOptions
    err = "Could not find LaTeX process named '" <> opt.defaultProcess <> "'."

renderLaTeXExp ::
  Model ->
  Expansion HtmlNode
renderLaTeXExp model node = do
  filepath <- toString <$> callText "page:filepath"
  txt <- fromMaybe "" <$> lookupAttr "text" node
  additionalPreamble <- maybe "" ("\n" <>) <$> lookupAttr "preamble" node
  spec' <- specFromModel model
  let spec = spec' {preamble = spec'.preamble <> additionalPreamble}
      ckey = (txt, filepath, spec)
  cache <- readTVarIO cacheVar
  result <-
    case lookupLaTeXCache ckey cache of
      Just result -> pure result
      Nothing -> do
        lift $ logInfoNS "organon:latex" "Cache miss; calling LaTeX instead."
        result <- liftRenderT $ renderLaTeX filepath spec txt
        atomically $ modifyTVar cacheVar $ insertLaTeXCache ckey result
        pure result
  liftChildren node
    `bindingText` do
      "latex:datauri" ## pure $ makeDataURI result
      "latex:mimetype" ## pure $ fst result
  where
    cacheVar = model.cache
