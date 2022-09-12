module Site.Org.Graph where

import Data.Aeson
import Data.Map (keys)
import Ema (Asset (..), Format (..))
import Optics.Operators ((^.))
import Org.Exporters.Common
import Org.Exporters.HTML (renderFragment)
import Org.Types (documentTitle)
import Site.Org.Model
import Site.Org.Render (OS, backend, renderSettings)

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
          keys (_linksTo source) <&> \backlink ->
            Link (_identifier source)
              <$> _identifier
              <$> lookupBacklink backlink m

renderGraph :: Model -> OS -> IO (Asset LByteString)
renderGraph m = fmap (AssetGenerated Other . encode) . buildRoamGraph (m ^. pages)
