module Site.Roam.Render where

import Common
import Data.Aeson (encode)
import Data.Map (assocs, (!))
import Data.Map.Syntax ((##))
import Ema
import Ondim
import Ondim.Extra
import Ondim.HTML (HtmlNode (..))
import Optics.Core
import Org.Exporters.Common
import Org.Exporters.HTML (render)
import Org.Types
import Org.Walk
import OrgAttach
import Relude.Extra (lookup)
import Render
import Site.Roam.Graph (buildRoamGraph)
import Site.Roam.Model

resolveLink :: (Route -> Text) -> RoamID -> OrgObject -> OrgObject
resolveLink route _ (Link (URILink "id" rid) content) =
  Link (UnresolvedLink $ route (Route_Post $ RoamID rid)) content
resolveLink _ _ x = x

resolveLinksInDoc :: (Route -> Text) -> OrgDocument -> OrgDocument
resolveLinksInDoc route = walkOrgInContext resolve
  where
    resolve _ p = maybe id (resolveLink route) (RoamID <$> lookup "id" p)

renderPost :: RoamID -> Prism' FilePath Route -> Model -> OndimOutput
renderPost rid rp m =
  OPage "roam-post" \st ly ->
    render renderSettings st $
      liftDocument backend doc' ly
        `binding` do
          "roam:tags" ## \inner ->
            join <$> forM (view docTag post) \tag ->
              children @HtmlNode inner `bindingText` do
                "roam:tag" ## pure tag
          "roam:backlinks" ## \inner ->
            ifElse (not (null backlinks)) inner
              `bindingText` do
                "backlinks:number" ## pure $ show (length backlinks)
              `binding` do
                "backlinks:entries" ## \inner' ->
                  join <$> forM (toList backlinks) \bl ->
                    children @HtmlNode inner'
                      `bindingText` do
                        "backlink:route" ## pure $ router (Route_Post $ backlinkID bl)
                      `binding` do
                        "backlink:title" ## const $ expandOrgObjects backend (backlinkTitle bl)
                        "backlink:excerpt" ## const $
                          clearAttrs
                            <$> expandOrgElements backend (postProcessBl (backlinkID bl) $ backlinkExcerpt bl)
  where
    router = routeUrl rp

    doc' = resolveAttachLinks router $ resolveLinksInDoc router post

    postProcessBl blId = walk (resolveLink router blId)

    clearAttr :: HtmlNode -> HtmlNode
    clearAttr el@Element {} = el {elementAttrs = []}
    clearAttr x = x

    clearAttrs :: [HtmlNode] -> [HtmlNode]
    clearAttrs = map clearAttr

    Post post _parent = posts m ! rid
    backlinks = fromMaybe mempty $ lookup rid (database m)

renderIndex :: Prism' FilePath Route -> Model -> OndimOutput
renderIndex rp m =
  OPage "roam-index" \st ly ->
    render renderSettings st $
      liftSubstructures ly
      `binding` do
        "roam:posts" ## \inner ->
          join <$> forM (assocs $ posts m) \(rid, post) ->
            children @HtmlNode inner
              `binding` do
                "post:title" ## const $ expandOrgObjects backend (documentTitle (doc post))
              `bindingText` do
                "post:link" ## pure (routeUrl rp $ Route_Post rid)

renderGraph :: Model -> OS -> IO (Asset LByteString)
renderGraph m = fmap (AssetGenerated Other . encode) . buildRoamGraph m
