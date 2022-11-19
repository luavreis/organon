module Site.Org.Meta where

import Data.Map.Syntax (runMapSyntax)
import Data.Text qualified as T
import Ondim.Extra (lookupAttr, prefixed)
import Ondim.Targets.HTML (HtmlNode, HtmlTag)
import Org.Exporters.HTML (HtmlBackend)
import Relude.Extra.Map (toPairs)
import Site.Org.Meta.Types
import Site.Org.Render.Types

immediately :: forall t. OndimNode HtmlTag t => MapSyntax Text (Expansion t) -> MapSyntax Text (Expansion t)
immediately x = fromRight x $ runMapSyntax (\_ _ -> Nothing) (\k v m -> m *> (k ## withoutExpansion @t k . v)) x

arrayTextMap :: Monad m => Text -> [MetaValue] -> MapSyntax Text (m Text)
arrayTextMap name arr = prefixed name do
  ":type" ## pure "array"
  ":size" ## pure $ show $ length arr

arrayExpMap ::
  HtmlBackend RenderT ->
  Text ->
  [MetaValue] ->
  MapSyntax Text (Expansion HtmlNode)
arrayExpMap bk name arr = immediately $ prefixed name do
  ":list" ## listArray bk arr

bindArray ::
  HtmlBackend RenderT ->
  Text ->
  [MetaValue] ->
  Ondim a ->
  Ondim a
bindArray bk name arr env =
  filteringExpansions @HtmlNode (not . T.isPrefixOf name) $
    env
      `bindingText` arrayTextMap name arr
      `binding` arrayExpMap bk name arr

listArray ::
  HtmlBackend RenderT ->
  [MetaValue] ->
  Expansion HtmlNode
listArray bk arr node = do
  alias <- fromMaybe "item" <$> lookupAttr "as" node
  join <$> forM (toList arr) \el ->
    liftChildren node
      & case el of
        MetaMap o -> bindObject bk alias o
        MetaList o -> bindArray bk alias o
        MetaObjects o ->
          (`bindingText` do alias <> ":type" ## pure "objects")
            . (`binding` do alias ## const $ expandOrgObjects bk o)

listMetaMap :: HtmlBackend RenderT -> MetaMap -> Expansion HtmlNode
listMetaMap bk meta node = do
  alias <- fromMaybe "item" <$> lookupAttr "as" node
  join <$> forM (toPairs meta) \(k, el) -> do
    let alias' = alias <> ":value"
    liftChildren node
      `bindingText` do alias <> ":key" ## pure k
      & case el of
        MetaMap o -> bindObject bk alias' o
        MetaList o -> bindArray bk alias' o
        MetaObjects o ->
          (`bindingText` do alias' <> ":type" ## pure "objects")
            . (`binding` do alias' ## const $ expandOrgObjects bk o)

objectTextMap :: Monad m => Text -> MetaMap -> MapSyntaxM Text (m Text) ()
objectTextMap name obj = prefixed name do
  ":type" ## pure "object"
  ":size" ## pure $ show $ length obj

objectExpMap ::
  HtmlBackend RenderT ->
  Text ->
  MetaMap ->
  MapSyntaxM Text (Expansion HtmlNode) ()
objectExpMap bk name obj = prefixed name do
  ":open" ## \node ->
    filteringExpansions @HtmlNode (not . T.isPrefixOf name) $ do
      pfx <- lookupAttr "prefix" node
      openMetaMap bk pfx obj $ liftChildren node
  ":list" ## listMetaMap bk obj

bindObject ::
  HtmlBackend RenderT ->
  Text ->
  MetaMap ->
  Ondim a ->
  Ondim a
bindObject bk name obj env =
  filteringExpansions @HtmlNode (not . T.isPrefixOf name) $
    env
      `bindingText` objectTextMap name obj
      `binding` objectExpMap bk name obj

openMetaMap ::
  HtmlBackend RenderT ->
  Maybe Text ->
  MetaMap ->
  Ondim a ->
  Ondim a
openMetaMap bk (maybe "" (<> ":") -> pfx) obj node =
  node
    `bindingText` prefixed pfx do
      forM_ (toPairs obj) \(k, v) -> do
        case v of
          MetaList a -> arrayTextMap k a
          MetaMap o -> objectTextMap k o
          MetaObjects _ -> do
            k <> ":type" ## pure "objects"
    `binding` (immediately . prefixed pfx) do
      forM_ (toPairs obj) \(k, v) -> do
        case v of
          MetaList a -> arrayExpMap bk k a
          MetaMap o -> objectExpMap bk k o
          MetaObjects o -> do
            k ## const $ expandOrgObjects bk o
