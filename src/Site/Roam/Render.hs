-- |

module Site.Roam.Render where
import Render
import Ema
import Site.Roam.Model
import Relude.Extra (lookup)
import Data.Map (assocs, (!))
import Data.Map.Syntax ((##))
import Heist.Interpreted (callTemplate, runChildrenWith, textSplice, Splice)
import Org.Exporters.Heist (Spliceable (..), documentSplices, walkNodes, Exporter)
import Ema.Route.Encoder (RouteEncoder)
import Text.XmlHtml qualified as X
import Org.Types
import Org.Walk

resolveLink :: (RoamRoute -> Text) -> OrgInline -> OrgInline
resolveLink route (Link (URILink "id" rid) content) =
  Link (URILink "http" $ route (RoamRoute_Post $ RoamID rid)) content
resolveLink _ x = x

renderPost :: RoamID -> RouteEncoder Model RoamRoute -> Model -> Splice Exporter
renderPost rid enc m =
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
            "BacklinkRoute" ## textSplice $ router (RoamRoute_Post $ backlinkID bl)
            "BacklinkExcerpt" ## clearAttrs <$> toSplice (postProcess $ backlinkExcerpt bl)
  where
    router = routeUrl enc m

    postProcess :: Walkable OrgInline a => a -> a
    postProcess = walk (resolveLink router)

    clearAttr :: X.Node -> X.Node
    clearAttr (X.Element n _ c) = X.Element n [] c
    clearAttr x = x

    clearAttrs :: [X.Node] -> [X.Node]
    clearAttrs = walkNodes clearAttr

    post = posts m ! rid
    backlinks = fromMaybe mempty $ lookup rid (database m)

renderIndex :: RouteEncoder Model RoamRoute -> Model -> Splice Exporter
renderIndex enc m =
  callTemplate "RoamIndex" $
    "Index" ## join <$> forM (assocs $ posts m) \ (rid, _post) ->
      runChildrenWith do
        "PostLink" ## textSplice (routeUrl enc m $ RoamRoute_Post rid)

-- buildRoamGraph :: Model -> Graph
-- buildRoamGraph m = Graph nodes links
--   where
--     pageToNode (uuid, RoamPost page _tags) =
--       Node uuid (renderHtml' m (documentTitle page))
--     nodes = map pageToNode (toPairs $ roamPosts m)
--     backlinksToLinks (target, backlinks)
--       -- Ensure target exists
--       | target `member` roamPosts m =
--           map (\ bl -> Link (backlinkID bl) target) $
--           toList backlinks
--       | otherwise = []
--     links = backlinksToLinks =<< toPairs (roamDatabase m)
