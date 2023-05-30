{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module Site.Org.Render (
  module Site.Org.Render,
  module Site.Org.Render.Types,
  module Site.Org.Render.Backend,
)
where

import Control.Monad.Except (tryError)
import Control.Monad.Trans.Either
import Data.IxSet.Typed qualified as Ix
import Data.Map qualified as Map
import Ema
import Ondim.Targets.HTML (HtmlNode (..), fromNodeList, toNodeList)
import Optics.Core
import Org.Exporters.HTML (evalOndim, render')
import Org.Exporters.Processing (initialOrgData)
import Org.Types (OrgDocument (..))
import Relude.Unsafe (fromJust)
import Site.Org.Meta
import Site.Org.Model
import Site.Org.Render.Backend
import Site.Org.Render.Types
import Site.Org.Route
import Text.XmlHtml qualified as X

createPortal :: Ondim [HtmlNode] -> Ondim (Either [HtmlNode] [HtmlNode])
createPortal = tryError

returnEarly :: [HtmlNode] -> Ondim a
returnEarly nodes = lift $ lift $ RenderT $ left nodes

bindPage ::
  RPrism ->
  Pages ->
  OrgEntry ->
  Ondim a ->
  Ondim a
bindPage rp pgs page node =
  bindDocument bk page.orgData page.document node
    `binding` do
      "page" #. do
        "meta" #. metaMapExp bk page.meta
        "route" #@ router $ Route_Page page.identifier
        "routeRaw" #@ toText $ review rp $ Route_Page page.identifier
        "filepath" #@ toText $ toFilePath page.identifier.path
        for_ page.identifier.orgId \i -> "id" #@ i.idText
  where
    bk = backend pgs rp
    router = routeUrl rp

evalOutput :: RenderM m => OndimState -> Ondim a -> m (Either OndimException a)
evalOutput ostate content = do
  let RenderT out = evalOndim ostate initialOrgData content
  collapse <$> runEitherT out
  where
    collapse = fromRight (error "")

renderPost :: Identifier -> Prism' FilePath Route -> Model -> OndimOutput
renderPost identifier rp m =
  PageOutput layout \ly fp -> do
    lifted <-
      bindPage rp m.pages page $
        withSite (FileDefinition fp) $
          liftNodes (fromNodeList $ X.docContent ly)
    return $ AssetGenerated Html $ render' $ ly {X.docContent = toNodeList lifted}
  where
    page = fromJust $ Ix.getOne (m.pages Ix.@= identifier)
    layout = fromMaybe "org-page" $ Map.lookup "layout" page.document.documentProperties
