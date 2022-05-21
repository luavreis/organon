{-# LANGUAGE DeriveGeneric #-}

-- |

module RoamGraph where
import Data.Aeson
-- import Models

-- data Node = Node { nodeId :: RoamID, nodeName :: Text } deriving Generic
-- data Link = Link { linkSource :: RoamID, linkTarget :: RoamID } deriving Generic
-- data Graph = Graph [Node] [Link] deriving Generic

-- instance ToJSON Node where
--   toEncoding (Node i name) =
--     pairs ( "id" .= i <> "name" .= name )

-- instance ToJSON Link where
--   toEncoding (Link source target) =
--     pairs ( "source" .= source <> "target" .= target )

-- instance ToJSON Graph where
--   toEncoding (Graph nodes links) =
--     pairs ( "nodes" .= nodes <> "links" .= links )
