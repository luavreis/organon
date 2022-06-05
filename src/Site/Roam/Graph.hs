-- |

module Site.Roam.Graph where
import Site.Roam.Model
import Data.Aeson
import Heist (HeistState)
import Org.Exporters.Heist (Exporter, toSplice, renderSplice)
import Render (renderSettings)
import Org.Types (documentTitle)
import Relude.Extra (toPairs, member)

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

buildRoamGraph :: Model -> HeistState Exporter -> Graph
buildRoamGraph m hs = Graph nodes links
  where
    render = decodeUtf8 . renderSplice hs renderSettings . toSplice
    pageToNode (uuid, Post page) =
      Node uuid (render (documentTitle page))
    nodes = map pageToNode (toPairs $ posts m)
    backlinksToLinks (target, backlinks)
      -- Ensure target exists
      | target `member` posts m =
          map (\ bl -> Link (backlinkID bl) target) $
          toList backlinks
      | otherwise = []
    links = backlinksToLinks =<< toPairs (database m)
