{-# LANGUAGE QuasiQuotes #-}
-- |

module LaTeX where
import Place
import Org.Types
import NeatInterpolation
import qualified Data.Map as M
import qualified Data.Text as T
import System.FilePath ((-<.>), (</>), takeDirectory)
import UnliftIO (IOException, catch, MonadUnliftIO)
import Control.Monad.Logger (logErrorNS, MonadLogger)


getPreamble :: OrgDocument -> Text
getPreamble d = T.unlines . mapMaybe justText $ lookupKeyword "latex_header" d
  where
    justText (ValueKeyword _ t) = Just t
    justText _ = Nothing

preambilizeKaTeX :: forall m. (MonadUnliftIO m, MonadLogger m) => Place -> OrgDocument -> m OrgElement
preambilizeKaTeX plc doc = preamble
  where
    hide :: KeywordValue
    hide = BackendKeyword [("style", "display: none")]

    mathOp = "\\newcommand{\\DeclareMathOperator}[2]{\\newcommand{#1}{\\operatorname{#2}}}"

    preamble :: m OrgElement
    preamble = do
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
          file <- mapM (readFileText . (takeDirectory (absolute plc) </>)) path
            `catch` (\(e :: IOException) -> do
                        logErrorNS "Preamble Loading" (show e)
                        pure Nothing)
          return $ maybe [] lines file
      | otherwise = return [txt]

    filt :: Text -> Bool
    filt txt =  "\\newcommand" `T.isPrefixOf` txt
             || "\\renewcommand" `T.isPrefixOf` txt
             || "\\def" `T.isPrefixOf` txt
             || "\\DeclareMathOperator" `T.isPrefixOf` txt
