-- |

module PandocTransforms.Emojis where
import PandocTransforms.Utilities
import Text.Emoji

-- I am not sure how to implement this. The problem is that I would have to
-- implement some sort of crazy algorithm to make combining emojis work, as
-- they span multiple characters... so for now all emoji must be separated
-- by whitepace!
convertEmojis :: Inline -> Inline
convertEmojis i@(Str s) =
  if isJust (aliasesFromEmoji s)
  then Span attr [i]
  else i
  where attr = ("",["emoji"],[])
convertEmojis x = x
