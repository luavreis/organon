module Site.Org.Render where

import Data.IxSet.Typed qualified as Ix
import Data.Map.Syntax ((##))
import Data.Text qualified as T
import Ema
import Ondim
import Ondim.Extra
import Ondim.HTML (HtmlNode (..))
import Optics.Core
import Org.Exporters.Common
import Org.Exporters.HTML (HTag, HtmlBackend, defHtmlBackend, render)
import Org.Exporters.Highlighting.EngraveFaces (engraveFacesHtml)
import Org.Types
import Org.Walk (walk)
import Relude.Unsafe (fromJust)
import Site.Org.Model
import Text.XmlHtml qualified as X

type OS = OndimMS (HTag IO)

type Layouts = Map Text X.Document

data OndimOutput
  = OAsset (OS -> IO (Asset LByteString))
  | OPage Text (OS -> X.Document -> IO (Either OndimException LByteString))

backend :: HtmlBackend IO
backend = defHtmlBackend {srcPretty = engraveFacesHtml}

renderSettings :: ExporterSettings
renderSettings =
  defaultExporterSettings
    { headlineLevelShift = 1,
      orgExportHeadlineLevels = 8
    }

resolveLink :: Pages -> (Route -> Text) -> OrgObject -> OrgObject
resolveLink m route = \case
  (Link t c) -> Link (resolveTarget t) c
  (Image t) -> Image (resolveTarget t)
  x -> x
  where
    resolveTarget = \case
      (URILink "id" id_) ->
        fromMaybe (error "TODO") do
          ident <- _identifier <$> Ix.getOne (m Ix.@= OrgID id_)
          pure $ UnresolvedLink $ route (Route_Page ident)
      (URILink uri (toString -> path))
        | Just source <- T.stripPrefix "source:" uri ->
            let ix_ = OrgPath source path
                orgRoute = do
                  ident <- _identifier <$> Ix.getOne (m Ix.@= LevelIx 0 Ix.@= ix_)
                  pure $ Route_Page ident
                finalRoute = fromMaybe (Route_Static $ StaticFileIx ix_) orgRoute
             in UnresolvedLink $ route finalRoute
      x -> x

resolveLinksInDoc :: Pages -> (Route -> Text) -> OrgDocument -> OrgDocument
resolveLinksInDoc m route = walk (resolveLink m route)

renderPost :: Identifier -> Prism' FilePath Route -> Model -> OndimOutput
renderPost identifier rp m =
  OPage "roam-post" \st ly ->
    render renderSettings st $
      liftDocument backend (_document page) ly
        `binding` do
          "roam:tags" ## \inner ->
            join <$> forM (_tags page) \tag ->
              children @HtmlNode inner `bindingText` do
                "roam:tag" ## pure tag
          "roam:backlinks" ## \inner ->
            ifElse (not (null backlinks)) inner
              `bindingText` do
                "backlinks:number" ## pure $ show (length backlinks)
              `binding` do
                "backlinks:entries" ## \inner' ->
                  join <$> forM (toList backlinks) \(blPage, blData) ->
                    children @HtmlNode inner'
                      `bindingText` do
                        "backlink:route" ## pure $ router (Route_Page $ _identifier blPage)
                      `binding` do
                        "backlink:title" ## const $ expandOrgObjects backend (documentTitle $ _document blPage)
                        "backlink:excerpt" ## const $
                          clearAttrs
                            <$> expandOrgElement backend (_blExcerpt blData)
  where
    page =
      Ix.getOne (_mPages m Ix.@= identifier)
        & fromJust
        & #_document %~ resolveLinksInDoc (_mPages m) router

    router = routeUrl rp

    clearAttr :: HtmlNode -> HtmlNode
    clearAttr el@Element {} = el {elementAttrs = []}
    clearAttr x = x

    clearAttrs :: [HtmlNode] -> [HtmlNode]
    clearAttrs = map clearAttr

    backlinks = lookupBacklinks (_mPages m) identifier
