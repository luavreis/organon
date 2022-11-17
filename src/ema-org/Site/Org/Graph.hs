module Site.Org.Graph where

import Data.Aeson
import Data.Map (keys)
import Ema (Asset (..), Format (..), routeUrl)
import Optics.Core
import Org.Exporters.Common
import Org.Exporters.HTML (renderFragment)
import Org.Exporters.Processing.OrgData (OrgData (parsedTitle))
import Site.Org.Model
import Site.Org.Render (M, OS, backend)

data Node = Node {nodeId :: Text, nodeName :: Text} deriving (Generic)

data Link = Link {linkSource :: Text, linkTarget :: Text} deriving (Generic)

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

buildRoamGraph :: Prism' FilePath Route -> Pages -> OS -> M Graph
buildRoamGraph rp m st = Graph <$> nodes ?? links
  where
    route = routeUrl rp . Route_Page

    render s =
      fmap (decodeUtf8 . fromRight (error "TODO: better error hadling in buildRoamGraph"))
        . renderFragment s st

    pageToNode page =
      let title =
            render (_orgData page) $
              expandOrgObjects backend $
                parsedTitle (_orgData page)
       in Node (route $ _identifier page) <$> title
    nodes = mapM pageToNode (toList m)

    links =
      toList m >>= \source ->
        catMaybes $
          keys (_linksTo source) <&> \backlink -> do
            page <- lookupOrgLocation m backlink
            return $ Link (route $ _identifier source) (route $ _identifier page)

renderGraph :: Prism' FilePath Route -> Model -> OS -> M (Asset LByteString)
renderGraph rp m = fmap (AssetGenerated Other . encode) . buildRoamGraph rp (m ^. pages)
