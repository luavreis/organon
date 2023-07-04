{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Site.Org.Render.Backend (backend) where

import Data.IxSet.Typed qualified as Ix
import Data.Text qualified as T
import Ema.Route.Url (routeUrl)
import Org.Exporters.HTML (defHtmlBackend)
import Org.Exporters.Processing (OrgData)
import Org.Types (LinkTarget (..), OrgElement (..), OrgObject (..))
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
  ExportBackend (customElement, customObject),
  HtmlBackend,
  RPrism,
  elementExp,
  objectExp,
 )
import Site.Org.Route (Route (Route_Page, Route_Static))
import Text.Slugify (slugify)

backend :: Pages -> RPrism -> HtmlBackend
backend p rp =
  let def = defHtmlBackend
   in def
        { customElement = customElementPipeline p rp
        , customObject = customObjectPipeline p rp
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

customElementPipeline ::
  Pages ->
  RPrism ->
  HtmlBackend ->
  OrgData ->
  OrgElement ->
  Maybe ExpansionMap
customElementPipeline m rp bk odata x =
  asum $
    flap
      [ customFigure m rp bk odata
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

customFigure ::
  Pages ->
  RPrism ->
  HtmlBackend ->
  OrgData ->
  OrgElement ->
  Maybe ExpansionMap
customFigure m rp bk odata = \case
  Paragraph aff [Link tgt []] ->
    elementExp bk odata
      <$> (Paragraph aff . one <$> (Link <$> resolveTarget m rp tgt ?? []))
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
        return $
          "#"
            <> case T.stripPrefix "*" x' of
              Just x'' -> "h-" <> slugify x''
              Nothing -> x'
    breakInternalRef = second maybeAddHash . T.breakOn "::"
    route = routeUrl rp
