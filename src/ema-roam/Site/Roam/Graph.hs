module Site.Roam.Graph where

import Data.Aeson
import Org.Exporters.Common
import Org.Exporters.HTML (renderFragment)
import Org.Types (documentTitle)
import Render (OS, backend, renderSettings)
import Site.Roam.Model

data Node = Node {nodeId :: Identifier, nodeName :: Text} deriving (Generic)

data Link = Link {linkSource :: Identifier, linkTarget :: Identifier} deriving (Generic)

data Graph = Graph [Node] [Link] deriving (Generic)

instance ToJSON Node where
  toEncoding (Node i name) =
    pairs ("id" .= i <> "name" .= name)

instance ToJSON Link where
  toEncoding (Link source target) =
    pairs ("source" .= source <> "target" .= target)

instance ToJSON Graph where
  toEncoding (Graph nodes links) =
    pairs ("nodes" .= nodes <> "links" .= links)

buildRoamGraph :: Pages -> OS -> IO Graph
buildRoamGraph m st = Graph <$> nodes ?? links
  where
    render =
      fmap (decodeUtf8 . fromRight (error "TODO: better error hadling in buildRoamGraph"))
        . renderFragment renderSettings st

    pageToNode page =
      let title =
            render $
              expandOrgObjects backend $
                documentTitle (_document page)
       in Node (_identifier page) <$> title
    nodes = mapM pageToNode (toList m)

    links =
      toList m >>= \source ->
        catMaybes $
          _linksTo source <&> \backlink ->
            Link (_identifier source)
              <$> _identifier
              <$> lookupBacklink backlink m
