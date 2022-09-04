-- |

module Site.Roam.Process where
import Org.Types
import Site.Roam.Model ( Backlink(Backlink), RoamID(..), Post(Post), Model, insertPost )
import Place ( Place(..) )
import Common ( docTag, isolateSection, queryOrgInContext )
import Org.Walk ( Walkable(query) )
import System.FilePath (takeDirectory, splitDirectories)
import LaTeX (getKaTeXPreamble)
import UnliftIO (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import Site.Roam.Options
import Data.List (intersect)
import Optics.Core
import Relude.Extra (lookup)

processRoam ::
  forall m _c.
  (MonadUnliftIO m, MonadLogger m, MonadReader (Options, _c) m) =>
  HasCallStack =>
  OrgDocument ->
  Place ->
  m (Model -> Model)
processRoam post place = do
  let fp = relative place
  preamble <- getKaTeXPreamble place post
  opts <- asks fst

  let relDir = takeDirectory (relative place)
      fpTags =
        if relDir == "."
          then []
          else toText <$> splitDirectories (takeDirectory $ relative place)
      postWithTags = over docTag (fpTags <>) post

      addToModel :: OrgDocument -> Maybe RoamID -> Ap m (Endo Model)
      addToModel docToAdd parentId =
        case lookupProperty "id" docToAdd of
          Just (RoamID -> rid)
            | parentId /= Just rid,
              ex <- lookupProperty "roam_exclude" docToAdd,
              ex == Nothing || ex == Just "nil" ->
              let tags = view docTag docToAdd
              in if null (privateTags opts `intersect` tags) &&
                    (null (publicTags opts) || not (null (publicTags opts `intersect` tags)))
                then do
                  let bklinks = processLinks docToAdd
                      docWithPreamble = mapContent (first (preamble :)) docToAdd
                      postEndo = insertPost fp rid (Post docWithPreamble parentId) $
                                map (second (Backlink rid (documentTitle docWithPreamble)))
                                bklinks
                  pure postEndo
                else pure (Endo id)
          _ -> pure mempty

      processSectionsWithId :: Tags -> Properties -> OrgSection -> (Ap m) (Endo Model)
      processSectionsWithId tags props section =
        let inhSection = section & #sectionTags %~ (<> tags)
                                 & #sectionProperties %~ (<> props)
        in addToModel (isolateSection inhSection post) (RoamID <$> lookup "id" props)

  let endo = queryOrgInContext processSectionsWithId postWithTags

  fmap appEndo $ getAp $ endo <> addToModel postWithTags Nothing

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
    processBlock (GreaterBlock _ _ els) = query processBlock els
    processBlock blk = zip (query processLink blk) (repeat [blk])

    processLink :: OrgObject -> [RoamID]
    processLink (Link (URILink "id" uid) _) = [RoamID uid]
    processLink _ = []

