module Site.Org.Parsing.Options where

import Optics.Core ((%~))
import Org.Parser (OrgOptions (..), defaultOrgOptions)

parsingOptions :: OrgOptions
parsingOptions =
  def
    & #orgElementParsedKeywords %~ (++ ["transclude", "excerpt"])
    & #orgElementAffiliatedKeywords %~ (++ ["meta"])
  where
    def = defaultOrgOptions
