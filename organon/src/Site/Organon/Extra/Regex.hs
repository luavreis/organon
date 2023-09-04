module Site.Organon.Extra.Regex where

import Ondim.Extra.Expansions (ifElse, lookupAttr', listExp)
import Site.Org.Render.Types
import Text.Regex.PCRE
import Text.Regex.PCRE.Text ()

regexExp :: GlobalExpansion
regexExp node = do
  text <- lookupAttr' "text" node
  regex <- lookupAttr' "regex" node
  case text =~~ regex of
    Just (getAllTextMatches -> matches) ->
      ifElse True node
        `binding` do
          "match.result" #. listExp textData matches
    Nothing -> ifElse False node
