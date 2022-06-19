-- |

module Site.Roam.Common where
import Org.Types
import Org.Walk (walk, Walkable (walkM), walkM)
import qualified Data.Text as T
import Optics.Core
import Data.Bitraversable (bimapM)

docTag :: Lens' OrgDocument Tags
docTag = lens getTags setTags
  where
    getTags = foldMap sepTags . lookupKeyword "filetags"
    setTags doc tags =
      doc { documentKeywords =
            [("filetags", ValueKeyword "" (":" <> T.intercalate ":" tags <> ":")) | not (null tags)]
            ++ filter (("filetags" /=) . fst) (documentKeywords doc)
          }
    sepTags (ValueKeyword _ t) =
      t & T.dropAround (':' ==)
        & T.split (':' ==)
    sepTags _ = []

walkOrgInContextM :: forall m a.
  (Monad m, Walkable a OrgSection, Walkable a OrgElement) =>
  (Tags -> Properties -> a -> m a) -> OrgDocument -> m OrgDocument
walkOrgInContextM f doc =
  mapContentM (bimapM
               (walkM (f docTags docProps))
               (mapM (doSection docTags docProps))) doc
  where
    docTags = view docTag doc
    docProps = documentProperties doc
    doSection :: Tags -> Properties -> OrgSection -> m OrgSection
    doSection tags properties section = do
      let inhTags = sectionTags section <> tags
          inhProps = sectionProperties section <> properties
      mapSectionContentM (bimapM
                          (walkM (f inhTags inhProps))
                          (mapM (doSection inhTags inhProps))) section

walkOrgInContext :: forall a.
  (Walkable a OrgSection, Walkable a OrgElement) =>
  (Tags -> Properties -> a -> a) -> OrgDocument -> OrgDocument
walkOrgInContext f = runIdentity . walkOrgInContextM (\t p -> Identity . f t p)

queryOrgInContext :: forall m.
  (Monoid m) =>
  (Tags -> Properties -> OrgSection -> m) -> OrgDocument -> m
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
      f inhTags inhProps section <> subs

shift :: Int -> OrgSection -> OrgSection
shift i sec@OrgSection { sectionLevel = level }
  | level + i > 0  = sec { sectionLevel = level + i }
  | otherwise      = sec { sectionLevel = 1 }

isolateSection :: OrgSection -> OrgDocument -> OrgDocument
isolateSection section doc =
  let section' = walk (shift (-(sectionLevel section) + 1)) section
      title = sectionTitle section
  in doc { documentKeywords =
              ("title", ParsedKeyword [] title) :
              filter (("title" /=) . fst) (documentKeywords doc)
          , documentProperties = sectionProperties section' <> documentProperties doc
          , documentChildren = sectionChildren section'
          , documentSections = sectionSubsections section'
          }
     & set docTag (sectionTags section)
