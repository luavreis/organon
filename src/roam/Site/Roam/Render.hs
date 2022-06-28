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
import Text.XmlHtml qualified as X
import Org.Types
import Org.Walk
import Heist (HeistState)
import Site.Roam.Graph (buildRoamGraph)
import Data.Aeson (encode)
import Optics.Core
import Common
import OrgAttach

resolveLink :: (Route -> Text) -> RoamID -> OrgInline -> OrgInline
resolveLink route _ (Link (URILink "id" rid) content) =
  Link (URILink "http" $ route (Route_Post $ RoamID rid)) content
resolveLink _ _ x = x

resolveLinksInDoc :: (Route -> Text) -> OrgDocument -> OrgDocument
resolveLinksInDoc route = walkOrgInContext resolve
  where resolve _ p = maybe id (resolveLink route) (RoamID <$> lookup "id" p)

renderPost :: RoamID -> Prism' FilePath Route -> Model -> HeistState Exporter -> Asset LByteString
renderPost rid rp m = renderAsset $
  callTemplate "RoamPost" do
    "Tags" ## join <$> forM (view docTag post) \tag ->
      runChildrenWith do
        "Tag" ## textSplice tag
    documentSplices (resolveAttachLinks router $ resolveLinksInDoc router post)
    "Backlinks" ##
      ifElseSpliceWith (not (null backlinks)) do
        "NumberOfBacklinks" ## textSplice (show $ length backlinks)
        "BacklinkEntries" ## join <$> forM (toList backlinks) \ bl ->
          runChildrenWith do
            "BacklinkTitle" ## toSplice $ backlinkTitle bl
            "BacklinkRoute" ## textSplice $ router (Route_Post $ backlinkID bl)
            "BacklinkExcerpt" ## clearAttrs <$> toSplice (postProcessBl (backlinkID bl) $ backlinkExcerpt bl)
  where
    router = routeUrl rp

    postProcessBl blId = walk (resolveLink router blId)

    clearAttr :: X.Node -> X.Node
    clearAttr (X.Element n a c) = X.Element n (filter ((`notElem` ["id", "href"]) . fst) a) c
    clearAttr x = x

    clearAttrs :: [X.Node] -> [X.Node]
    clearAttrs = walkNodes clearAttr

    Post post _parent = posts m ! rid
    backlinks = fromMaybe mempty $ lookup rid (database m)

renderIndex :: Prism' FilePath Route -> Model -> HeistState Exporter -> Asset LByteString
renderIndex rp m = renderAsset $
  callTemplate "RoamIndex" do
    "TitleText" ## textSplice "Olá!"
    "Index" ## join <$> forM (assocs $ posts m) \ (rid, post) ->
      runChildrenWith do
        "PostTitle" ## toSplice (documentTitle (doc post))
        "PostLink" ## textSplice (routeUrl rp $ Route_Post rid)

renderGraph :: Model -> HeistState Exporter -> Asset LByteString
renderGraph m = AssetGenerated Other . encode . buildRoamGraph m
