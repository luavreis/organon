-- |

module Site.Roam.Graph where
import Site.Roam.Model
import Data.Aeson
import Render (renderSettings, OS)
import Org.Types (documentTitle)
import Relude.Extra (toPairs, member)
import Org.Exporters.Common
import Org.Exporters.HTML (renderFragment)

data Node = Node { nodeId :: RoamID, nodeName :: Text } deriving Generic
data Link = Link { linkSource :: RoamID, linkTarget :: RoamID } deriving Generic
data Graph = Graph [Node] [Link] deriving Generic

instance ToJSON Node where
  toEncoding (Node i name) =
    pairs ( "id" .= i <> "name" .= name )

instance ToJSON Link where
  toEncoding (Link source target) =
    pairs ( "source" .= source <> "target" .= target )

instance ToJSON Graph where
  toEncoding (Graph nodes links) =
    pairs ( "nodes" .= nodes <> "links" .= links )

buildRoamGraph :: Model -> OS -> Graph
buildRoamGraph m st = Graph nodes links
  where
    render = decodeUtf8 . fromRight "" . renderFragment renderSettings st
    pageToNode (uuid, Post page _parent) =
      let e = expandOrgObjects $ documentTitle page
      in Node uuid (render e)
    nodes = map pageToNode (toPairs $ posts m)
    backlinksToLinks (target, backlinks)
      -- Ensure target exists
      | target `member` posts m =
          map (\ bl -> Link (backlinkID bl) target) $
          toList backlinks
      | otherwise = []
    links = backlinksToLinks =<< toPairs (database m)
