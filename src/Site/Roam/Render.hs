-- |

module Site.Roam.Render where
import Render
import Ema
import Site.Roam.Model
import Relude.Extra (lookup)
import Data.Map (assocs, (!))
import Data.Map.Syntax ((##))
import Heist.Interpreted (callTemplate, runChildrenWith, textSplice)
import Org.Exporters.Heist (Spliceable (..), documentSplices, walkNodes, Exporter)
import Ema.Route.Encoder (RouteEncoder)
import Text.XmlHtml qualified as X
import Org.Types
import Org.Walk
import Heist (HeistState)
import Site.Roam.Graph (buildRoamGraph)
import Data.Aeson (encode)

resolveLink :: (Route -> Text) -> OrgInline -> OrgInline
resolveLink route (Link (URILink "id" rid) content) =
  Link (URILink "http" $ route (Route_Post $ RoamID rid)) content
resolveLink _ x = x

renderPost :: RoamID -> RouteEncoder Model Route -> Model -> HeistState Exporter -> Asset LByteString
renderPost rid enc m = renderAsset $
  callTemplate "RoamPost" do
    "Tags" ## join <$> forM (tags post) \tag ->
      runChildrenWith do
        "Tag" ## textSplice tag
    documentSplices (postProcess $ doc post)
    "Backlinks" ##
      ifElseSpliceWith (not (null backlinks)) do
        "NumberOfBacklinks" ## textSplice (show $ length backlinks)
        "BacklinkEntries" ## join <$> forM (toList backlinks) \ bl ->
          runChildrenWith do
            "BacklinkTitle" ## toSplice $ backlinkTitle bl
            "BacklinkRoute" ## textSplice $ router (Route_Post $ backlinkID bl)
            "BacklinkExcerpt" ## clearAttrs <$> toSplice (postProcess $ backlinkExcerpt bl)
  where
    router = routeUrl enc m

    postProcess :: Walkable OrgInline a => a -> a
    postProcess = walk (resolveLink router)

    clearAttr :: X.Node -> X.Node
    clearAttr (X.Element n a c) = X.Element n (filter ((/= "id") . fst) a) c
    clearAttr x = x

    clearAttrs :: [X.Node] -> [X.Node]
    clearAttrs = walkNodes clearAttr

    post = posts m ! rid
    backlinks = fromMaybe mempty $ lookup rid (database m)

renderIndex :: RouteEncoder Model Route -> Model -> HeistState Exporter -> Asset LByteString
renderIndex enc m = renderAsset $
  callTemplate "RoamIndex" do
    "TitleText" ## textSplice "Olá!"
    "Index" ## join <$> forM (assocs $ posts m) \ (rid, post) ->
      runChildrenWith do
        "PostTitle" ## toSplice (documentTitle (doc post))
        "PostLink" ## textSplice (routeUrl enc m $ Route_Post rid)

renderGraph :: Model -> HeistState Exporter -> Asset LByteString
renderGraph m = AssetGenerated Other . encode . buildRoamGraph m
