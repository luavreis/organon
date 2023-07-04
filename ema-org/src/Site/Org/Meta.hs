module Site.Org.Meta where

import Ondim.Extra.Expansions (listExp, mapExp)
import Org.Exporters.Processing (OrgData)
import Site.Org.Meta.Types (MetaMap, MetaValue (..))
import Site.Org.Render.Types (
  ExpansionMap,
  HtmlBackend,
  SomeExpansion,
  namespace,
  objectsExp,
 )
import Site.Org.Utils.MonoidalMap (MonoidalMap (getMap))

metaExp :: HtmlBackend -> OrgData -> MetaValue -> SomeExpansion
metaExp bk odata = \case
  MetaMap m -> namespace $ metaMapExp bk odata m
  MetaList l -> namespace $ metaListExp bk odata l
  MetaObjects o -> namespace $ objectsExp bk odata o

metaListExp :: HtmlBackend -> OrgData -> [MetaValue] -> ExpansionMap
metaListExp = (listExp .) . metaExp

metaMapExp :: HtmlBackend -> OrgData -> MetaMap -> ExpansionMap
metaMapExp bk odata = mapExp (metaExp bk odata) . (.getMap)
