{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Site.Org.Process (loadOrgFile) where

import Control.Monad.Logger (MonadLogger, logDebugN, logWarnNS)
import Control.Monad.Trans.RWS.CPS
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NES
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Optics.Core
import Org.Exporters.Processing
import Org.Types
import Org.Walk
import Relude.Extra (lookup)
import Relude.Unsafe (fromJust)
import Site.Org.Meta.Types (MetaMap, elementToMetaMap)
import Site.Org.Model
import Site.Org.Options (Options (..), Source (..))
import Site.Org.PreProcess (DocLike (..), walkPreProcess)
import Site.Org.PreProcess qualified as PP
import Site.Org.Utils.MonoidalMap
import System.FilePath (dropExtension, isExtensionOf, splitDirectories, takeDirectory)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Directory (getModificationTime)
import Prelude hiding (ask, asks, get, gets, local, modify)

type Backlinks = MonoidalMap UnresolvedLocation (NES.NESet (Maybe InternalRef))

-- | Entries, backlinks and files found
type Output = ([OrgEntry], Backlinks, Set (OrgPath, UTCTime))

data ProcessEnv = ProcessEnv
  { -- Constant
    path :: OrgPath
  , -- Inherited
    parent :: Maybe Identifier
  , inhData :: OrgData
  , inhProps :: Properties
  }
  deriving (Generic)

data ProcessSt = ProcessSt
  { anchorCounter :: Int
  , meta :: MetaMap
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
  -- | Source filepath
  OrgPath ->
  OrgDocument ->
  m [OrgEntry]
loadOrgFile opts path doc0 = do
  let odata0 =
        initialOrgData
          { exporterSettings = opts.exporterSettings
          , parserOptions = opts.parserSettings
          }
      (doc, datum) = continuePipeline odata0 do
        gatherSettings doc0
        pure <$> withCurrentData (pruneDoc doc0)
  doc' <- runReaderT (walkPreProcess doc) (PP.PreProcessEnv {..})
  let (doc'', datum') = continuePipeline datum do
        getCompose $ resolveLinks doc'
      inhData = datum' & #filetags %~ (<> fpTags)
  (e, _, _) <- snd <$> execRWST (walkProcess doc'') (ProcessEnv {..}) (ProcessSt {..})
  pure e
  where
    meta = mempty
    anchorCounter = 0
    sources = opts.mount
    parent = Nothing
    attachDir = Nothing
    srcDir = path.source.dir
    relDir = takeDirectory path.relpath
    fpTags =
      if relDir == "."
        then []
        else toText <$> splitDirectories relDir
    inhProps = mempty

walkProcess :: (MonadUnliftIO m, MonadLogger m) => WalkM (ProcessM m)
walkProcess = buildMultiW \f l ->
  l
    .> processLink
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

parseTS :: (MonadLogger m) => Text -> ProcessM m (Maybe UTCTime)
parseTS (T.takeWhile (/= ' ') -> t) = do
  let parsed :: Maybe UTCTime = parseTimeM True defaultTimeLocale "%Y%m%d%H%M%S" (toString t)
  case parsed of
    Nothing -> warnCouldNotParse $> Nothing
    time -> pure time
  where
    warnCouldNotParse = do
      p <- asks (.path)
      lift $ logWarnNS "Loading" $ "Could not parse timestamp " <> t <> " in " <> prettyOrgPath p

-- processLink first writes backlinks with null internal references to the
-- writer state. Then, if processBlock finds any of those backlinks with
-- null references in its children, it replaces those null references with a
-- new anchor and creates a div around the block to receive that anchor.

-- This should be tought of as a stack of pending references, that don't
-- have a block yet. Then, the closest block in the upward tree picks up
-- those null references and replace them with a reference to itself.

processElement :: Monad m => WalkM (ProcessM m) -> OrgElement -> ProcessM m OrgElement
processElement f elm =
  case elementToMetaMap elm of
    Just m -> do
      blks <- snd <$> listenBacklinks (f elm)
      let blks' = NES.map (<|> Just MetaProperty) <$> blks
      tellBacklinks blks'
      modify (#meta %~ (m <>))
      return $ GreaterBlock mempty (Special "meta") []
    Nothing -> do
      (elm', blks) <- listenBacklinks $ f elm
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
          return $ GreaterBlock (Map.singleton "portal-target" $ ValueKeyword anchor) (Special "portal") [elm']
        else tellBacklinks blks $> elm'

-- | Process links to other Org files and register the backlinks.
processLink :: (MonadUnliftIO m, MonadLogger m) => OrgObject -> ProcessM m OrgObject
processLink l = do
  case l of
    (Link t _) -> processTarget t
    _ -> pure ()
  return l

processTarget :: (MonadUnliftIO m, MonadLogger m) => LinkTarget -> ProcessM m ()
processTarget t = do
  case t of
    -- ID link to another Org file
    (URILink "id" (T.breakOn "::" -> (uid, _))) ->
      tellBacklink (Right (OrgID uid))
    -- File link to another file.
    (URILink uri _)
      | Just sAlias <- T.stripPrefix "source:" uri -> do
          let opath = fromJust $ readMaybe (toString sAlias)
          if "org" `isExtensionOf` opath.relpath
            then do
              tellBacklink (Left $ #relpath %~ dropExtension $ opath)
            else do
              lift $ logDebugN $ "Adding file " <> prettyOrgPath opath
              getModificationTime (toRawPath opath) >>= tellFile opath
              tellBacklink (Left opath)
    _ -> pure ()

processEntry :: (DocLike a, MonadLogger m, MonadUnliftIO m, MultiWalk MWTag a) => WalkM (ProcessM m) -> a -> ProcessM m a
processEntry f s = do
  env <- ask

  let tags = getTags s <> filetags env.inhData
      sectionId = lookup "id" (getProps s)
      level = getLevel s
      notExclud =
        let p = lookup "roam_exclude" (getProps s)
         in isNothing p || p == Just "nil"

      isEntry = (level == 0 || isJust sectionId) && notExclud

      identifier = Identifier env.path (OrgID <$> sectionId)
      newData =
        env.inhData
          & #filetags .~ tags
          & #parsedTitle %~ maybe id const (getTitle s)

  let childEnv _ =
        env
          & #parent %~ bool id (const $ Just identifier) isEntry
          & #inhData .~ newData
          & #inhProps %~ (<> getProps s)

  oldMeta <- gets (.meta)

  (s', (_, subBacklinks, subFiles)) <- listen (local childEnv $ f s)

  newMeta <- gets (.meta)

  when isEntry do
    let doc = toDoc s' & (#documentProperties %~ (<> env.inhProps))
        layout =
          fromMaybe "org-page" $
            Map.lookup "layout" doc.documentProperties

    ctime <- join <$> forM (Map.lookup "ctime" doc.documentProperties) parseTS
    mtime <- join <$> forM (Map.lookup "mtime" doc.documentProperties) parseTS

    tellEntry $
      OrgEntry
        { identifier = identifier
        , ctime = ctime
        , mtime = mtime
        , tags = tags
        , document = doc
        , orgData = newData
        , meta = newMeta
        , level = level
        , layout = layout
        , parent = env.parent
        , staticFiles = subFiles
        , linksTo = coerce subBacklinks
        }

  modify (#meta .~ oldMeta)

  return s'
