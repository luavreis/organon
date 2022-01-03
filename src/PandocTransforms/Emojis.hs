-- |

module PandocTransforms.Emojis where
import PandocTransforms.Utilities
import Text.Emoji
import qualified Data.Text as T

-- I am not sure how to implement this. The problem is that I would have to
-- implement some sort of crazy algorithm to make combining emojis work, as
-- they span multiple characters... so for now all emoji must be separated
-- by whitepace!
convertEmojis :: Inline -> Inline
convertEmojis (Str s) = case aliasesFromEmoji s of
  Nothing -> Str s
  Just [] -> Str s
  Just (a : _) ->
    RawInline (Format "html") ("<i class=\"twa twa-"
                               <> fixAlias a
                               <> "\"></i>")
  where
    fixAlias a = T.map (\case '_' -> '-'; x -> x) a
convertEmojis x = x
