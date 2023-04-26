module Site.Org.Meta where

import Ondim.Extra.Expansions
import Site.Org.Meta.Types
import Site.Org.Render.Types
import Site.Org.Utils.MonoidalMap (MonoidalMap (getMap))

metaExp :: HtmlBackend -> MetaValue -> SomeExpansion
metaExp bk = \case
  MetaMap m -> namespace $ metaMapExp bk m
  MetaList l -> namespace $ metaListExp bk l
  MetaObjects o -> someExpansion $ const $ expandOrgObjects bk o

metaListExp :: HtmlBackend -> [MetaValue] -> ExpansionMap
metaListExp = listExp . metaExp

metaMapExp :: HtmlBackend -> MetaMap -> ExpansionMap
metaMapExp bk = mapExp (metaExp bk) . (.getMap)
