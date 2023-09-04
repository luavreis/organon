{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module Site.Org.Render (
  module Site.Org.Render,
  module Site.Org.Render.Types,
  module Site.Org.Render.Backend,
)
where

import Control.Monad.Trans.Either (runEitherT)
import Data.IxSet.Typed qualified as Ix
import Data.Map qualified as Map
import Ema (Asset (AssetGenerated), Format (Html), routeUrl)
import Ondim.Extra.Expansions (listExp)
import Optics.Core (Prism', review)
import Org.Types (OrgDocument (..))
import Relude.Unsafe (fromJust)
import Site.Org.Meta (metaMapExp)
import Site.Org.Model (
  Identifier (orgId, path),
  Model (pages),
  OrgEntry (..),
  OrgID (idText),
  Pages,
  linksTo,
  lookupOrgLocation,
  toFilePath,
 )
import Site.Org.Render.Backend
import Site.Org.Render.Types
import Site.Org.Route (Route (Route_Page))
import Ondim.Targets.HTML (HtmlDocument)

pageExp ::
  RPrism ->
  Pages ->
  OrgEntry ->
  ExpansionMap
pageExp rp pgs page = do
  documentExp bk page.orgData page.document
  "parse:objs" #* parserExpObjs bk page.orgData
  "parse:elms" #* parserExpElms bk page.orgData
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

renderPost :: Identifier -> Prism' FilePath Route -> Model -> Ondim (Asset LByteString)
renderPost identifier rp m = do
  lPage :: [HtmlDocument] <-
    callTemplate layout
      `binding` do
        "page" #. pageExp rp m.pages page
  AssetGenerated Html <$> renderNodeOrError lPage
  where
    page = fromJust $ Ix.getOne (m.pages Ix.@= identifier)
    layout =
      fromMaybe "org-page.html" $
        Map.lookup "layout" page.document.documentProperties
