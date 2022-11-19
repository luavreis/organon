module Site.Org.Parsing.Options where

import Org.Parser (OrgOptions (..), defaultOrgOptions)

parsingOptions :: OrgOptions
parsingOptions =
  def
    { orgElementParsedKeywords =
        orgElementParsedKeywords def ++ ["transclude", "excerpt"],
      orgElementAffiliatedKeywords =
        orgElementAffiliatedKeywords def ++ ["meta"]
    }
  where
    def = defaultOrgOptions
