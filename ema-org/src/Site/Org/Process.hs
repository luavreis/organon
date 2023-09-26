{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Site.Org.Process (loadOrgFile) where

import Control.Monad.Logger (MonadLogger, logDebugN, logWarnNS)
import Control.Monad.Trans.RWS.CPS
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NES
import Data.Text qualified as T
import Data.Time (UTCTime)
import Optics.Core
import Org.Exporters.Processing
import Org.Types
import Org.Walk
import Relude.Extra (lookup)
import Site.Org.Model
import Site.Org.Options
import Site.Org.Utils.Document (isolateSection)
import Site.Org.Utils.MonoidalMap
import System.FilePath (addTrailingPathSeparator, isAbsolute, isExtensionOf, takeDirectory, (</>))
import UnliftIO (MonadUnliftIO)
import UnliftIO.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, getHomeDirectory, getModificationTime, makeAbsolute)
import Prelude hiding (ask, asks, get, gets, local, modify)

class DocLike a where
  getLevel :: a -> Int
  getTags :: a -> [Tag]
  getProps :: a -> Properties
  getTitle :: a -> Maybe [OrgObject]
  toDoc :: a -> OrgDocument

instance DocLike OrgDocument where
  getLevel _ = 0
  getTags _ = []
  getProps = documentProperties
  getTitle _ = Nothing
  toDoc = id

instance DocLike OrgSection where
  getLevel = sectionLevel
  getTags = sectionTags
  getProps = sectionProperties
  getTitle = Just . sectionTitle
  toDoc = isolateSection

type Backlinks = MonoidalMap UnresolvedLocation (NES.NESet (Maybe InternalRef))

-- | Entries, backlinks and files found
type Output = ([OrgEntry], Backlinks, Set (OrgPath, UTCTime))

data ProcessEnv = ProcessEnv
  { -- Constant
    path :: OrgPath
  , opts :: Options
  , srcOpts :: Source
  , -- Inherited
    parent :: Maybe Identifier
  , inhData :: OrgData
  , inhProps :: Properties
  , attachDir :: Maybe FilePath
  }
  deriving (Generic)

newtype ProcessSt = ProcessSt
  { anchorCounter :: Int
  }
  deriving (Generic)

type ProcessM m =
  RWST
    ProcessEnv
    Output
    ProcessSt -- Portal counter to assign unique anchor
    m

loadOrgFile ::
  forall m.
  (MonadUnliftIO m, MonadLogger m) =>
  Options ->
  Source ->
  -- | Source filepath
  OrgPath ->
  OrgDocument ->
  m [OrgEntry]
loadOrgFile opts srcOpts path doc0 = do
  let odata0 =
        initialOrgData
          { exporterSettings = opts.exporterSettings
          , parserOptions = opts.parserSettings
          }
      (prunedDoc, datum) = continuePipeline odata0 do
        gatherSettings doc0
        pure <$> withCurrentData (pruneDoc doc0)
  let (docWithResolvedLinks, inhData) = continuePipeline datum do
        getCompose $ resolveLinks prunedDoc
  (e, _, _) <- snd <$> execRWST (walkProcess docWithResolvedLinks) (ProcessEnv {..}) (ProcessSt {..})
  pure e
  where
    anchorCounter = 0
    parent = Nothing
    attachDir = Nothing
    inhProps = mempty

walkProcess :: (MonadUnliftIO m, MonadLogger m) => WalkM (ProcessM m)
walkProcess = buildMultiW \f l ->
  l
    .> processLink f
    .> processElement f
    .> processEntry @OrgSection f
    .> processEntry @OrgDocument f

tellEntry :: (Monad m) => OrgEntry -> ProcessM m ()
tellEntry x = tell ([x], mempty, mempty)

tellBacklink :: (Monad m) => UnresolvedLocation -> ProcessM m ()
tellBacklink x = tell (mempty, MonoidalMap $ Map.singleton x (NES.singleton Nothing), mempty)

tellBacklinks :: (Monad m) => Backlinks -> ProcessM m ()
tellBacklinks x = tell (mempty, x, mempty)

tellFile :: (Monad m) => OrgPath -> UTCTime -> ProcessM m ()
tellFile x y = tell (mempty, mempty, Set.singleton (x, y))

listenBacklinks :: (Monad m) => ProcessM m a -> ProcessM m (a, Backlinks)
listenBacklinks = censor (\(x, _, z) -> (x, mempty, z)) . listens (\(_, b, _) -> b)

-- processLink first writes backlinks with null internal references to the
-- writer state. Then, if processElement finds any of those backlinks with
-- null references in its children, it replaces those null references with a
-- new anchor and creates a div around the block to receive that anchor.

-- This should be tought of as a stack of pending references, that don't
-- have a block yet. Then, the closest block in the upward tree picks up
-- those null references and replace them with a reference to itself.

processElement :: Monad m => WalkM (ProcessM m) -> OrgElement -> ProcessM m OrgElement
processElement f elm@(OrgElement _ (Keyword _ _)) = f elm -- pass-through
processElement f elm = do
  (el'@(OrgElement _ eld), blks) <- listenBacklinks $ f elm
  if any (Nothing `NES.member`) blks
    then do
      modify (#anchorCounter %~ (+ 1))
      n <- gets (.anchorCounter)
      let anchor = "bltarget" <> show n
          -- Here, we keep only the outermost backlinks.
          -- That is, if there is a backlink just below this element
          -- (so it does not have an anchor yet), we throw away all
          -- the backlinks to the same file and keep just that one.
          newBlks =
            blks <&> \s ->
              if Nothing `NES.member` s
                then NES.singleton (Just (Anchor anchor))
                else s
      tellBacklinks newBlks
      return $ OrgElement (Map.singleton "name" $ ValueKeyword anchor) eld
    else tellBacklinks blks $> el'

-- | Process links to other Org files and register the backlinks.
processLink :: (MonadUnliftIO m, MonadLogger m) => WalkM (ProcessM m) -> OrgObject -> ProcessM m OrgObject
processLink f = \case
  (Link t c) -> Link <$> processTarget t <*> mapM f c
  l -> f l

processTarget :: (MonadUnliftIO m, MonadLogger m) => LinkTarget -> ProcessM m LinkTarget
processTarget t = do
  env <- ask
  case t of
    -- ID link to another Org file
    (URILink "id" (T.breakOn "::" -> (uid, _))) -> do
      tellBacklink (Right (OrgID uid))
      return t
    (URILink "attachment" att)
      | Just aDir <- env.attachDir ->
          fromMaybe t <$> findTarget (toText $ aDir </> toString att)
    (URILink protocol fp)
      | protocol `elem` env.opts.fileProtocols ->
          fromMaybe t <$> findTarget fp
    _ -> return t
  where
    findTarget (T.breakOn "::" -> (fp, anchor)) =
      findLinkSource (toString fp) >>= \case
        Just opath -> do
          if "org" `isExtensionOf` opath.relpath
            then do
              tellBacklink (Left opath)
            else do
              lift $ logDebugN $ "Adding file " <> prettyOrgPath opath
              getModificationTime (toFilePath opath) >>= tellFile opath
              tellBacklink (Left opath)
          let uri = "source:" <> show opath
          return $ Just $ URILink uri anchor
        Nothing -> return Nothing

processEntry :: (DocLike a, MonadLogger m, MonadUnliftIO m, MultiWalk MWTag a) => WalkM (ProcessM m) -> a -> ProcessM m a
processEntry f s = do
  env <- ask

  let tags = getTags s <> env.inhData.filetags
      entryId = lookup "id" (getProps s)
      level = getLevel s
      noExportTags = env.inhData.exporterSettings.orgExportExcludeTags
      exclude =
        let p = lookup "roam_exclude" (getProps s)
         in any (/= "nil") p || or [i == j | j <- tags, i <- noExportTags]

      isEntry = (level == 0 || isJust entryId) && not exclude

      identifier = Identifier env.path (OrgID <$> entryId)
      newData =
        env.inhData
          & #filetags .~ tags
          & #keywords %~ maybe id (Map.insert "title" . ParsedKeyword) (getTitle s)

      -- For org-attach
      updatedEntryId =
        if entryId /= lookup "id" env.inhProps
          then entryId
          else Nothing

  newAttachDir <- foldMapM getAttachDir updatedEntryId

  lift $ whenJust newAttachDir $ logDebugN . ("Using attach dir: " <>) . show

  let childEnv =
        env
          & bool id (#parent ?~ identifier) isEntry
          & #inhData .~ newData
          & #inhProps %~ (<> getProps s)
          & maybe id (#attachDir ?~) newAttachDir

  local (const childEnv) do
    (subProcessedDoc, (_, subBacklinks, subFiles)) <- listen (f s)

    let doc = toDoc subProcessedDoc & #documentProperties .~ childEnv.inhProps

    when isEntry do
      tellEntry $
        OrgEntry
          { identifier = identifier
          , document = doc
          , orgData = newData
          , level = level
          , parent = env.parent
          , staticFiles = subFiles
          , linksTo = coerce subBacklinks
          }

    return subProcessedDoc

findLinkSource :: (MonadIO m, MonadLogger m) => FilePath -> ProcessM m (Maybe OrgPath)
findLinkSource rawLinkFp = do
  env <- ask
  -- HACK: improvised tilde expansion
  userDir <- getHomeDirectory
  let tildeExpandedLinkFp = toString $ T.replace "~/" (toText $ addTrailingPathSeparator userDir) (toText rawLinkFp)
  trueFp <-
    canonicalizePath
      if isAbsolute tildeExpandedLinkFp
        then tildeExpandedLinkFp
        else takeDirectory (toFilePath env.path) </> tildeExpandedLinkFp
  exists <- doesFileExist trueFp
  if exists
    then do
      op <- findSource env.opts.mount trueFp
      whenNothing_ op $
        lift $
          logWarnNS "findSource" $
            "File " <> show trueFp <> " linked from " <> prettyOrgPath env.path <> " exists but does not belong to a declared source."
      return op
    else do
      lift $
        logWarnNS "findSource" $
          "File " <> show trueFp <> " linked from " <> prettyOrgPath env.path <> " does not exist."
      return Nothing

getAttachDir :: MonadIO m => Text -> ProcessM m (Maybe FilePath)
getAttachDir key = do
  env <- ask
  let rid = toString key
      orgAttachIdTSFolderFormat = take 6 rid </> drop 6 rid
      orgAttachIdUUIDFolderFormat = take 2 rid </> drop 2 rid

  possibleDirs <-
    mapM makeAbsolute $
      (takeDirectory (toFilePath env.path) </>) . (env.srcOpts.orgAttachDir </>)
        <$> [ orgAttachIdTSFolderFormat
            , orgAttachIdUUIDFolderFormat
            , rid
            ]

  findM doesDirectoryExist possibleDirs
  where
    findM p = foldr (\x -> ifM (p x) (pure $ Just x)) (pure Nothing)
