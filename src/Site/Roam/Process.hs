-- |

module Site.Roam.Process where
import Org.Types
import Site.Roam.Model ( Backlink(Backlink), RoamID(..), Post(Post), Model, insertPost )
import Place ( Place(..) )
import Common ( docTag, isolateSection, queryOrgInContext )
import Org.Walk ( Walkable(query) )
import System.FilePath (takeDirectory, splitDirectories)
import LaTeX (preambilizeKaTeX)
import UnliftIO (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import Optics.Core ( over )

processRoam ::
  forall m.
  (MonadUnliftIO m, MonadLogger m)
  => HasCallStack
  => OrgDocument
  -> Place
  -> m (Endo Model)
processRoam post place = do
  let fp = relative place
  preamble <- preambilizeKaTeX place post

  let relDir = takeDirectory (relative place)
      fpTags =
        if relDir == "."
          then []
          else toText <$> splitDirectories (takeDirectory $ relative place)
      postWithTags = over docTag (fpTags <>) post

      addToModel :: OrgDocument -> RoamID -> Ap m (Endo Model)
      addToModel doc' rid = do
        let bklinks = processLinks doc'
            doc'' = mapContent (first (preamble :)) doc'
            postEndo = insertPost fp rid (Post doc'') $
                       map (second (Backlink rid (documentTitle doc'')))
                       bklinks
        pure postEndo

      processSectionsWithId :: Tags -> Properties -> OrgSection -> (Ap m) (Endo Model)
      processSectionsWithId tags props section =
        case lookupSectionProperty "id" section of
          Just (RoamID -> uuid) -> do
            let inhSection = section { sectionTags = tags, sectionProperties = props }
            addToModel (isolateSection inhSection post) uuid
          Nothing -> pure mempty

  let endo = queryOrgInContext processSectionsWithId postWithTags

  getAp $
    case lookupProperty "id" postWithTags of
      Just uid -> endo <> addToModel postWithTags (RoamID uid)
      _ -> endo

  where
    -- Walks over the document but only maps the "zero-level" elements (no nesting)
    processLinks :: OrgDocument -> [(RoamID, [OrgElement])]
    processLinks doc' = f (documentChildren doc') <> query mapSections doc'
      where
        f = foldMap processBlock
        mapSections OrgSection{ sectionChildren = c } = f c

    processBlock :: OrgElement -> [(RoamID, [OrgElement])]
    processBlock (PlainList attrs ltype items) =
      flip evalState 0 $
        flip foldMapM items \(ListItem bul con box tag els) -> do
          maybe (modify (+ 1)) put con
          con' <- gets Just
          pure . zip (query processLink els) . repeat . one $
            PlainList attrs ltype [ListItem bul con' box tag els]
    processBlock blk = zip (query processLink blk) (repeat [blk])

    processLink :: OrgInline -> [RoamID]
    processLink (Link (URILink "id" uid) _) = [RoamID uid]
    processLink _ = []

