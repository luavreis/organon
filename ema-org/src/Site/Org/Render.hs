{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module Site.Org.Render (
  module Site.Org.Render,
  module Site.Org.Render.Types,
  module Site.Org.Render.Backend,
)
where

import Control.Monad.Except (tryError)
import Control.Monad.Trans.Either (left, runEitherT)
import Data.IxSet.Typed qualified as Ix
import Data.Map qualified as Map
import Ema (Asset (AssetGenerated), Format (Html), routeUrl)
import Ondim.Targets.HTML (HtmlNode (..), fromNodeList, toNodeList)
import Optics.Core (Prism', review)
import Org.Exporters.HTML (render')
import Org.Types (OrgDocument (..))
import Relude.Unsafe (fromJust)
import Site.Org.Meta (metaMapExp)
import Site.Org.Model (
  Identifier (orgId, path),
  Model (pages),
  OrgEntry (..),
  OrgID (idText),
  Pages,
  toFilePath, lookupOrgLocation, linksTo,
 )
import Site.Org.Render.Backend
import Site.Org.Render.Types
import Site.Org.Route (Route (Route_Page))
import Text.XmlHtml qualified as X
import Ondim.Extra.Expansions (listExp)

createPortal :: Ondim [HtmlNode] -> Ondim (Either [HtmlNode] [HtmlNode])
createPortal = tryError

returnEarly :: [HtmlNode] -> Ondim a
returnEarly nodes = lift $ RenderT $ left nodes

pageExp ::
  RPrism ->
  Pages ->
  OrgEntry ->
  ExpansionMap
pageExp rp pgs page = do
  documentExp bk page.orgData page.document
  "doc:parse:objs" #* parserExpObjs bk page.orgData
  "doc:parse:elms" #* parserExpElms bk page.orgData
  "page" #. do
    "meta" #. metaMapExp bk page.orgData page.meta
    "route" #@ router $ Route_Page page.identifier
    "routeRaw" #@ toText $ review rp $ Route_Page page.identifier
    "filepath" #@ toText $ toFilePath page.identifier.path
    "links-to" #. listExp (namespace . pageExp rp pgs) linksTo
    for_ page.identifier.orgId \i -> "id" #@ i.idText
  where
    linksTo = mapMaybe (lookupOrgLocation pgs) $ Map.keys page.linksTo
    bk = backend pgs rp
    router = routeUrl rp

evalOutput :: RenderM m => OndimState -> Ondim a -> m (Either OndimException a)
evalOutput ostate content = do
  let RenderT out = evalOndimTWith ostate content
  collapse <$> runEitherT out
  where
    collapse = fromRight (error "")

renderPost :: Identifier -> Prism' FilePath Route -> Model -> OndimOutput
renderPost identifier rp m =
  PageOutput layout \ly -> do
    lifted <-
      liftNodes (fromNodeList $ X.docContent ly)
        `binding` pageExp rp m.pages page
    return $
      AssetGenerated Html $
        render' ly {X.docContent = toNodeList lifted}
  where
    page = fromJust $ Ix.getOne (m.pages Ix.@= identifier)
    layout =
      fromMaybe "org-page" $
        Map.lookup "layout" page.document.documentProperties
