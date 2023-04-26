{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module Site.Org.Render (
  module Site.Org.Render,
  module Site.Org.Render.Types,
  module Site.Org.Render.Backend,
  module Org.Exporters.Common,
)
where

import Control.Monad.Except (liftEither)
import Control.Monad.Trans.Either
import Data.Bitraversable (bimapM)
import Data.IxSet.Typed qualified as Ix
import Data.List qualified as L
import Data.Time
import Ema
import Ondim.Extra
import Ondim.Targets.HTML (HtmlNode (..), fromNodeList, toNodeList)
import Optics.Core
import Org.Exporters.Common hiding (Expansion, Filter, Ondim, OndimMS)
import Org.Exporters.HTML (evalOndim, render')
import Relude.Unsafe (fromJust)
import Site.Org.Meta
import Site.Org.Model
import Site.Org.Render.Backend
import Site.Org.Render.Types
import Site.Org.Route
import Text.XmlHtml qualified as X

createPortal :: Ondim [HtmlNode] -> Ondim (Either [HtmlNode] [HtmlNode])
createPortal nodes = do
  oSt <- getOndimMS
  eSt <- get
  x <- lift $ lift $ runRenderT $ runStateT (evalOndimTWith oSt nodes) eSt
  put $ either snd snd x
  bimapM (pure . fst) (liftEither . fst) x

returnEarly :: [HtmlNode] -> Ondim a
returnEarly nodes = do
  eSt <- get
  lift $ lift $ RenderT $ left (nodes, eSt)

bindPage ::
  RPrism ->
  Pages ->
  OrgEntry ->
  Ondim a ->
  Ondim a
bindPage rp pgs page node =
  openMetaMap bk (Just "meta") page.meta $
    bindDocument bk page.orgData page.document node
      `binding` prefixed "doc:" do
        "tags" ## \inner ->
          join <$> forM page.tags \tag ->
            liftChildren @HtmlNode inner
              `bindingText` do
                "tag" ## pure tag
        -- TODO: after Ondim is upated, those two will be textual expansions.
        forM_ page.ctime \t -> "ctime" ## timeExp t
        forM_ page.mtime \t -> "mtime" ## timeExp t
      `bindingText` prefixed "page:" do
        "route" ## pure $ router $ Route_Page page.identifier
        "routeRaw" ## pure $ toText $ review rp $ Route_Page page.identifier
        "filepath" ## pure $ toText $ toRawPath page.identifier.path
        for_ page.identifier.orgId \i -> "id" ## pure $ i.idText
  where
    bk = backend pgs rp
    router = routeUrl rp

evalOutput :: RenderM m => OndimMS -> Ondim a -> m (Either OndimException a)
evalOutput ostate content = do
  let RenderT out = evalOndim ostate initialExporterState content
  collapse <$> runEitherT out
  where
    collapse = fromRight (error "")

renderPost :: Identifier -> Prism' FilePath Route -> Model -> OndimOutput
renderPost identifier rp m =
  PageOutput page.layout \ly -> do
    lifted <- bindPage rp m.pages page do
      liftNodes (fromNodeList $ X.docContent ly)
    return $ AssetGenerated Html $ render' $ ly {X.docContent = toNodeList lifted}
  where
    page = fromJust $ Ix.getOne (m.pages Ix.@= identifier)
