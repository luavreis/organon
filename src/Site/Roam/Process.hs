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
import LaTeX (preambilizeKaTeX)
import Org.Parser
import UnliftIO (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import Site.Roam.Common
import Optics.Core

processRoam ::
  forall m.
  (MonadUnliftIO m, MonadLogger m, MonadReader Options m)
  => FilePath
  -> m (Endo Model)
processRoam fp = execWriterT $ do
  source <- asks O.mount
  let place = Place fp source

  post <- parseOrgIO defaultOrgOptions (absolute place)
  preamble <- lift $ preambilizeKaTeX place post

  let relDir = takeDirectory (relative place)
      fpTags =
        if relDir == "."
          then []
          else toText <$> splitDirectories (takeDirectory $ relative place)
      postWithTags = over docTag (fpTags <>) post

      addToModel :: OrgDocument -> RoamID -> WriterT (Endo Model) m ()
      addToModel doc' rid = do
        let bklinks = processLinks doc'
            doc'' = mapContent (first (preamble :)) doc'
        tell =<< lift (processAttachments rid doc'')
        tell $ insertPost rid (Post doc'') $
          map
            (\ ~(ruuid, excrpt) -> (ruuid, Backlink rid (documentTitle doc'') excrpt))
            bklinks

      processSectionsWithId :: Tags -> Properties -> OrgSection -> Ap (WriterT (Endo Model) m) [RoamID]
      processSectionsWithId tags props section = Ap $
        case lookupSectionProperty "id" section of
          Just (RoamID -> uuid) -> do
            let inhSection = section { sectionTags = tags, sectionProperties = props }
            addToModel (isolateSection inhSection post) uuid
            pure [uuid]
          Nothing -> pure []

  uuids <- getAp $ queryOrgInContext processSectionsWithId postWithTags

  case lookupProperty "id" postWithTags of
    Just (RoamID -> uuid) -> do
      addToModel postWithTags uuid
      tell $ filterIds fp (uuid : uuids)
    _ -> tell $ filterIds fp uuids

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

