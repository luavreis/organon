module Site.Organon.Extra.Regex where

import Ondim.Extra (ifElse, lookupAttr', prefixed)
import Ondim.Targets.HTML (HtmlNode)
import Site.Org.Render.Types
import Text.Regex.PCRE
import Text.Regex.PCRE.Text ()

regexExp :: Expansion HtmlNode
regexExp node = do
  text <- lookupAttr' "text" node
  regex <- lookupAttr' "regex" node
  case text =~~ regex of
    Just (getAllTextMatches -> matches) ->
      ifElse True node
        `bindingText` prefixed "regex:match:" do
          forM_ (zip [0 ..] matches) \(i :: Int, m) -> do
            show i ## pure m
    Nothing -> ifElse False node
