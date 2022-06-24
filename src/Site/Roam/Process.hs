-- |

module Site.Roam.Process
  ( Place (..)
  , absolute
  , processRoam
  , tell
  ) where
import Site.Roam.Model
import Place
import Control.Monad.Trans.Writer.Strict
import Org.Types
import Org.Walk
import System.FilePath (takeDirectory, splitDirectories)
import Site.Roam.OrgAttach (processAttachments)
import Site.Roam.Options as O
import LaTeX (preambilizeKaTeX, processLaTeX)
import Org.Parser
import UnliftIO (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import Site.Roam.Common
import Optics.Core

processRoam ::
  forall m.
  (MonadUnliftIO m, MonadLogger m, MonadReader Options m)
  => HasCallStack
  => FilePath
  -> m (Endo Model)
processRoam fp = do
  source <- asks O.mount
  let place = Place fp source
  post <- parseOrgIO defaultOrgOptions (absolute place)
          >>= processLaTeX place
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
        attachEndo <- Ap $ processAttachments rid doc''
        let postEndo = insertPost fp rid (Post doc'') $
                       map (second (Backlink rid (documentTitle doc'')))
                       bklinks
        pure $ attachEndo <> postEndo

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

