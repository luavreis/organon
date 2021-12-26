-- |

module PandocTransforms.Org where
import PandocTransforms.Utilities
import Text.Regex.TDFA

extractOrgVar :: Block -> [(Text,Text)]
extractOrgVar (RawBlock "org" s) =
  let (_,_,_,m) = s =~ ("^#\\+([[:alpha:]]+): *\"?([^\"]+)\"?$" :: Text) :: (Text,Text,Text,[Text])
  in case m of
    [a,b] -> [(a,b)]
    _ -> []
extractOrgVar _ = []

extractOrgVars :: [Block] -> [(Text,Text)]
extractOrgVars = query extractOrgVar

setOrgVars :: Pandoc -> Pandoc
setOrgVars (Pandoc meta blocks) =
  let vars = extractOrgVars blocks
      meta' = foldr (uncurry setMeta) meta vars
  in Pandoc meta' blocks
