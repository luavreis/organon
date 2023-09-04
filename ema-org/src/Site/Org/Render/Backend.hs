{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Site.Org.Render.Backend (backend) where

import Data.IxSet.Typed qualified as Ix
import Data.Text qualified as T
import Ema.Route.Url (routeUrl)
import Org.Exporters.HTML (defBackend)
import Org.Exporters.Processing (OrgData)
import Org.Types (LinkTarget (..), OrgObject (..))
import Relude.Unsafe (fromJust)
import Site.Org.Model (
  LevelIx (LevelIx),
  OrgEntry (identifier),
  OrgID (OrgID),
  OrgPath,
  Pages,
  StaticFile (StaticFile),
 )
import Site.Org.Render.Types (
  ExpansionMap,
  ExportBackend (customObject),
  HtmlBackend,
  RPrism,
  objectExp,
 )
import Site.Org.Route (Route (Route_Page, Route_Static))
import Org.Exporters.Processing.InternalLinks (sectionTitleToAnchor)

backend :: Pages -> RPrism -> HtmlBackend
backend p rp =
  defBackend
    { customObject = customObjectPipeline p rp
    }

customObjectPipeline ::
  Pages ->
  RPrism ->
  HtmlBackend ->
  OrgData ->
  OrgObject ->
  Maybe ExpansionMap
customObjectPipeline m rp bk odata x =
  asum $
    flap
      [ customLink m rp bk odata
      ]
      x

customLink ::
  Pages ->
  RPrism ->
  HtmlBackend ->
  OrgData ->
  OrgObject ->
  Maybe ExpansionMap
customLink m rp bk odata = \case
  Link tgt descr ->
    objectExp bk odata <$> (Link <$> resolveTarget m rp tgt ?? descr)
  _ -> Nothing

resolveTarget :: Pages -> RPrism -> LinkTarget -> Maybe LinkTarget
resolveTarget m rp = \case
  (URILink "id" id')
    | (id_, anchor) <- breakInternalRef id'
    , Just page <- Ix.getOne (m Ix.@= OrgID id_) ->
        Just . UnresolvedLink $
          route (Route_Page page.identifier) <> anchor
  (URILink uri (maybeAddHash -> anchor))
    | Just rawOPath <- T.stripPrefix "source:" uri ->
        Just . UnresolvedLink $
          let ix_ :: OrgPath = fromJust $ readMaybe (toString rawOPath)
              orgRoute = do
                ident <- (.identifier) <$> Ix.getOne (m Ix.@= LevelIx 0 Ix.@= ix_)
                pure $ Route_Page ident
              staticRoute = Route_Static (StaticFile ix_)
           in route (fromMaybe staticRoute orgRoute) <> anchor
  _ -> Nothing
  where
    -- TODO: move this to org-hs
    maybeAddHash x =
      fromMaybe x do
        x' <- T.stripPrefix "::" x
        return $ "#" <> maybe x' sectionTitleToAnchor (T.stripPrefix "*" x')
    breakInternalRef = second maybeAddHash . T.breakOn "::"
    route = routeUrl rp
