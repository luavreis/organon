{-# LANGUAGE OverloadedLists #-}

module Site.Org.Parsing.Options where

import Optics.Core ((%~))
import Org.Parser (OrgOptions (..), defaultOrgOptions)

parsingOptions :: OrgOptions
parsingOptions =
  def
    & #orgElementAffiliatedKeywords %~ (<> ["meta"])
  where
    def = defaultOrgOptions
