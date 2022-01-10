-- |

module PandocTransforms.Links where
import PandocTransforms.Utilities
import System.FilePath
import qualified Data.Text as T

fixLink' :: FilePath -> Text -> Text
fixLink' root p =
  if isAbsolute (toString p) || isURI p
  then p
  else toText (normalise $ root </> toString p)

fixLink :: FilePath -> Text -> Text
fixLink root p =
  let p' = if liftT takeExtension p `elem` strippedFmts
           then liftT dropExtension p
           else p
  in case T.breakOn ":" p' of
    ("file", c) -> fixLink' root (T.tail c)
    (c, "") -> fixLink' root c
    _ -> p'
  where
    strippedFmts = [".org", ".md"]
    liftT f = toText . f . toString

fixLinks :: FilePath -> Inline -> Inline
fixLinks fp (Image f i (u,t)) = Image f i (fixLink' fp u, t)
fixLinks fp (Link a i (u,t)) = Link a i (fixLink fp u, t)
fixLinks _ x = x
