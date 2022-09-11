module Site.Roam.Render where

import Data.Aeson (encode)
import Data.Map.Syntax ((##))
import Ema
import Ondim
import Ondim.Extra
import Ondim.HTML (HtmlNode (..))
import Optics.Core
import Org.Exporters.Common
import Org.Exporters.HTML (render)
import Org.Types
import Render
import Site.Roam.Graph (buildRoamGraph)
import Site.Roam.Model
import Relude.Unsafe (fromJust)
import Data.IxSet.Typed qualified as Ix

-- resolveLink :: (Route -> Text) -> RoamID -> OrgObject -> OrgObject
-- resolveLink route _ (Link (URILink "id" rid) content) =
--   Link (UnresolvedLink $ route (Route_Post $ RoamID rid)) content
-- resolveLink _ _ x = x

-- resolveLinksInDoc :: (Route -> Text) -> OrgDocument -> OrgDocument
-- resolveLinksInDoc route = walkOrgInContext resolve
--   where
--     resolve _ p = maybe id (resolveLink route) (RoamID <$> lookup "id" p)

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
                  join <$> forM (toList backlinks) \(blPage, excerpt) ->
                    children @HtmlNode inner'
                      `bindingText` do
                        "backlink:route" ## pure $ router (Route_Page $ _identifier blPage)
                      `binding` do
                        "backlink:title" ## const $ expandOrgObjects backend (documentTitle $ _document blPage)
                        "backlink:excerpt" ## const $
                          clearAttrs
                            <$> expandOrgElements backend excerpt
  where
    page = fromJust $ Ix.getOne $ (m ^. pages) Ix.@= identifier

    router = routeUrl rp

    clearAttr :: HtmlNode -> HtmlNode
    clearAttr el@Element {} = el {elementAttrs = []}
    clearAttr x = x

    clearAttrs :: [HtmlNode] -> [HtmlNode]
    clearAttrs = map clearAttr

    backlinks =
      catMaybes $
        _linksTo page <&> \bl -> do
          blPage <- lookupBacklink bl (m ^. pages)
          return (blPage, _blExcerpt bl)

renderGraph :: Model -> OS -> IO (Asset LByteString)
renderGraph m = fmap (AssetGenerated Other . encode) . buildRoamGraph (m ^. pages)
