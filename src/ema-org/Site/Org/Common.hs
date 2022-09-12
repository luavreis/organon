module Site.Org.Common where

import Data.Bitraversable (bimapM)
import Data.Text qualified as T
import Optics.Core
import Org.Types
import Org.Walk (walk)

docTag :: Lens' OrgDocument Tags
docTag = lens getTags setTags
  where
    getTags = foldMap sepTags . lookupKeyword "filetags"
    setTags doc tags =
      doc
        { documentKeywords =
            [("filetags", ValueKeyword "" (":" <> T.intercalate ":" tags <> ":")) | not (null tags)]
              ++ filter (("filetags" /=) . fst) (documentKeywords doc)
        }
    sepTags (ValueKeyword _ t) =
      t & T.dropAround (':' ==)
        & T.split (':' ==)
    sepTags _ = []

walkOrgInContextM ::
  forall m.
  (Monad m) =>
  (Tags -> Properties -> [OrgElement] -> m [OrgElement]) ->
  OrgDocument ->
  m OrgDocument
walkOrgInContextM f doc =
  mapContentM
    ( bimapM
        (f docTags docProps)
        (mapM (doSection docTags docProps))
    )
    doc
  where
    docTags = view docTag doc
    docProps = documentProperties doc
    doSection :: Tags -> Properties -> OrgSection -> m OrgSection
    doSection tags properties section = do
      let inhTags = sectionTags section <> tags
          inhProps = sectionProperties section <> properties
      mapSectionContentM
        ( bimapM
            (f inhTags inhProps)
            (mapM (doSection inhTags inhProps))
        )
        section

walkOrgInContext ::
  (Tags -> Properties -> [OrgElement] -> [OrgElement]) ->
  OrgDocument ->
  OrgDocument
walkOrgInContext f = runIdentity . walkOrgInContextM (\t p -> Identity . f t p)

queryOrgInContext ::
  forall m.
  (Monoid m) =>
  (Tags -> Properties -> OrgSection -> m) ->
  OrgDocument ->
  m
queryOrgInContext f doc =
  foldMap (doSection docTags docProps) (documentSections doc)
  where
    docTags = view docTag doc
    docProps = documentProperties doc
    doSection :: Tags -> Properties -> OrgSection -> m
    doSection tags properties section = do
      let inhTags = sectionTags section <> tags
          inhProps = sectionProperties section <> properties
          subs = foldMap (doSection inhTags inhProps) (sectionSubsections section)
      f tags properties section <> subs

shift :: Int -> OrgSection -> OrgSection
shift i sec@OrgSection {sectionLevel = level}
  | level + i > 0 = sec {sectionLevel = level + i}
  | otherwise = sec {sectionLevel = 1}

isolateSection :: OrgSection -> OrgDocument -> OrgDocument
isolateSection section doc =
  let section' = walk (shift (-(sectionLevel section) + 1)) section
      title = sectionTitle section
   in doc
        { documentKeywords =
            ("title", ParsedKeyword [] title) :
            filter (("title" /=) . fst) (documentKeywords doc),
          documentProperties = sectionProperties section',
          documentChildren = sectionChildren section',
          documentSections = sectionSubsections section'
        }
        & set docTag (sectionTags section)
