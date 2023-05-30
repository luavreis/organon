module Site.Organon.Extra.Regex where

import Ondim.Extra (ifElse, lookupAttr')
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
          "match" #. forM_ (zip [0 ..] matches) \(i :: Int, m) -> do
            show i #@ m
    Nothing -> ifElse False node
