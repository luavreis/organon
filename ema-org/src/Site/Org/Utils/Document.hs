module Site.Org.Utils.Document where

import Data.Bitraversable (bimapM)
import Org.Exporters.Processing.OrgData (OrgData (filetags))
import Org.Types
import Org.Walk (walk)

walkOrgInContextM ::
  forall m.
  (Monad m) =>
  (Tags -> Properties -> [OrgElement] -> m [OrgElement]) ->
  (OrgDocument, OrgData) ->
  m OrgDocument
walkOrgInContextM f (doc, datum) =
  mapContentM
    ( bimapM
        (f docTags docProps)
        (mapM (doSection docTags docProps))
    )
    doc
  where
    docTags = filetags datum
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
  (OrgDocument, OrgData) ->
  OrgDocument
walkOrgInContext f = runIdentity . walkOrgInContextM (\t p -> Identity . f t p)

queryOrgInContext ::
  forall m.
  (Monoid m) =>
  (Tags -> Properties -> OrgSection -> m) ->
  (OrgDocument, OrgData) ->
  m
queryOrgInContext f (doc, datum) =
  foldMap (doSection docTags docProps) (documentSections doc)
  where
    docTags = filetags datum
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

isolateSection :: OrgSection -> OrgDocument
isolateSection section =
  OrgDocument
    { documentProperties = sectionProperties section
    , documentChildren = sectionChildren section
    , documentSections =
        sectionSubsections $
          walk (shift (- sectionLevel section)) section
    }
