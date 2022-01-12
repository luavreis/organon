{-# LANGUAGE DeriveGeneric #-}

-- |

module RoamGraph where
import Data.Aeson
import Models
import Relude.Extra

data Node = Node { nodeId :: UUID, nodeName :: Text } deriving Generic
data Link = Link { linkSource :: UUID, linkTarget :: UUID } deriving Generic
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

buildRoamGraph :: Model -> Graph
buildRoamGraph m = Graph nodes links
  where
    pageToNode ~(uuid, page) = Node uuid (postTitle page)

    nodes = map pageToNode (toPairs $ roamPosts m)

    backlinksToLinks ~(target, backlinks) =
      map (\ bl -> Link (backlinkUUID bl) target) $ toList backlinks

    links = backlinksToLinks =<< toPairs (roamDatabase m)
