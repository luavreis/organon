-- |

module PandocTransforms.Roam where
import PandocTransforms.Utilities hiding (Writer)
import Text.Pandoc.Shared
import qualified Data.Text as T
import Control.Monad.Trans.Writer
import Data.UUID.Types as UUID (UUID, fromText)

processRoam :: [Block] -> ([Block], [(UUID, Block)])
processRoam blocks = error ""
  where
    processBlock :: Block -> Writer [(UUID, Block)] Block
    processBlock block =
      mapWriter mapper (walkM processLink block)
      where
        mapper :: (Block, [UUID]) -> (Block, [(UUID, Block)])
        mapper ~(b, xs) = (b, map (, block) xs)

    processLink :: Inline -> Writer [UUID] Inline
    processLink (Link attr alt (url, title)) =
      let same = pure $ Link attr alt (url, title)
      in if isURI url
      then case T.breakOn ":" url of
        ("id", id) -> case UUID.fromText id of
          Just uuid -> do
            tell [uuid]
            return $ Link attr alt ("/zettelkasten/" <> id, title)
          Nothing -> same
        otherwise -> same
      else same
    processLink x = pure x
