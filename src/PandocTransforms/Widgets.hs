-- |

module PandocTransforms.Widgets where
import PandocTransforms.Utilities
import Text.Pandoc.Builder as B
import Models

type Args = Text

type Widget = Args -> Model -> [Block]

findWidget :: Text -> Maybe Widget
findWidget "recent-posts" = Just recentPosts
findWidget _ = Nothing

recentPosts :: Widget
recentPosts _ _ = B.toList $
  bulletList []
