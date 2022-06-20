-- |

module Site.Roam.Render where
import Render
import Ema
import Site.Roam.Model
import Site.Roam.Route
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
import System.FilePath ((</>))
import Optics.Core
import Site.Roam.Common

resolveLink :: (Route -> Text) -> RoamID -> OrgInline -> OrgInline
resolveLink route _ (Link (URILink "id" rid) content) =
  Link (URILink "http" $ route (RoutePost' $ RoamID rid)) content
resolveLink route rid (Link (URILink "attachment" path) content) =
  Link (URILink "http" $ route (RouteAttach' (AttachPath rid path))) content
resolveLink route rid (Image (URILink "attachment" path)) =
  Image (URILink "http" $ route (RouteAttach' (AttachPath rid path)))
resolveLink _ _ x = x

resolveLinksInDoc :: (Route -> Text) -> OrgDocument -> OrgDocument
resolveLinksInDoc route = walkOrgInContext resolve
  where resolve _ p = maybe id (resolveLink route) (RoamID <$> lookup "id" p)

renderPost :: RoamID -> RouteEncoder Model Route -> Model -> HeistState Exporter -> Asset LByteString
renderPost rid enc m = renderAsset $
  callTemplate "RoamPost" do
    "Tags" ## join <$> forM (view docTag post) \tag ->
      runChildrenWith do
        "Tag" ## textSplice tag
    documentSplices (resolveLinksInDoc router post)
    "Backlinks" ##
      ifElseSpliceWith (not (null backlinks)) do
        "NumberOfBacklinks" ## textSplice (show $ length backlinks)
        "BacklinkEntries" ## join <$> forM (toList backlinks) \ bl ->
          runChildrenWith do
            "BacklinkTitle" ## toSplice $ backlinkTitle bl
            "BacklinkRoute" ## textSplice $ router (RoutePost' $ backlinkID bl)
            "BacklinkExcerpt" ## clearAttrs <$> toSplice (postProcessBl (backlinkID bl) $ backlinkExcerpt bl)
  where
    router = routeUrl enc m

    postProcessBl blId = walk (resolveLink router blId)

    clearAttr :: X.Node -> X.Node
    clearAttr (X.Element n a c) = X.Element n (filter ((`notElem` ["id", "href"]) . fst) a) c
    clearAttr x = x

    clearAttrs :: [X.Node] -> [X.Node]
    clearAttrs = walkNodes clearAttr

    Post post = posts m ! rid
    backlinks = fromMaybe mempty $ lookup rid (database m)

renderIndex :: RouteEncoder Model Route -> Model -> HeistState Exporter -> Asset LByteString
renderIndex enc m = renderAsset $
  callTemplate "RoamIndex" do
    "TitleText" ## textSplice "Olá!"
    "Index" ## join <$> forM (assocs $ posts m) \ (rid, post) ->
      runChildrenWith do
        "PostTitle" ## toSplice (documentTitle (doc post))
        "PostLink" ## textSplice (routeUrl enc m $ RoutePost' rid)

renderGraph :: Model -> HeistState Exporter -> Asset LByteString
renderGraph m = AssetGenerated Other . encode . buildRoamGraph m

renderAttachment :: AttachPath -> Model -> HeistState Exporter -> Asset LByteString
renderAttachment (AttachPath rid path) m = const $
  case lookup rid (attachDirs m) of
    Just dir -> AssetStatic (dir </> toString path)
    Nothing  -> error "This should not happen. Unknown org-attach dir."
