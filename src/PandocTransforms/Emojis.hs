-- |

module PandocTransforms.Emojis where
import PandocTransforms.Utilities
import Text.Emoji

convertEmojis :: Inline -> Inline
convertEmojis i@(Str s) =
  if isJust (aliasesFromEmoji s)
  then Span attr [i]
  else i
  where attr = ("",["emoji"],[])
convertEmojis x = x
