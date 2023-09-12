{-# LANGUAGE RankNTypes #-}

module Site.Org.PreProcess (DocLike (..), walkPreProcess, PreProcessEnv (..)) where

import Control.Monad.Logger (MonadLogger, logDebugN, logWarnNS)
import Data.Text qualified as T
import Optics.Core
import Org.Types
import Org.Walk
import Relude.Extra (lookup)
import Site.Org.Model
import Site.Org.Options (Options (..), Source (..))
import Site.Org.Utils.Document (isolateSection)
import System.FilePath (addTrailingPathSeparator, isAbsolute, takeDirectory, (</>))
import UnliftIO (MonadUnliftIO)
import UnliftIO.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, getHomeDirectory, makeAbsolute)

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

data PreProcessEnv = PreProcessEnv
  { -- Constant
    path :: OrgPath
  , opts :: Options
  , srcOpts :: Source
  , -- Inherited
    attachDir :: Maybe FilePath
  , inhProps :: Properties
  }
  deriving (Generic)

type PreProcessM m = ReaderT PreProcessEnv m

walkPreProcess :: (MonadUnliftIO m, MonadLogger m) => WalkM (PreProcessM m)
walkPreProcess = buildMultiW \f l ->
  l
    .> processLink f
    .> processEntry @OrgSection f
    .> processEntry @OrgDocument f

-- | Resolve links to other files
processLink :: (MonadUnliftIO m, MonadLogger m) => WalkM (PreProcessM m) -> OrgObject -> PreProcessM m OrgObject
processLink f = \case
  (Link t c) -> Link <$> processTarget t <*> mapM f c
  l -> f l

processTarget :: forall m. (MonadUnliftIO m, MonadLogger m) => LinkTarget -> PreProcessM m LinkTarget
processTarget t = do
  env <- ask
  case t of
    -- Link to another file.
    l@(URILink protocol fp)
      | protocol `elem` env.opts.fileProtocols ->
          fromMaybe l <$> findTarget fp
    l@(URILink "attachment" att)
      | Just aDir <- env.attachDir ->
          fromMaybe l <$> findTarget (toText $ aDir </> toString att)
    l -> pure l
  where
    findTarget :: Text -> PreProcessM m (Maybe LinkTarget)
    findTarget (T.breakOn "::" -> (fp, anchor)) = do
      src <- findLinkSource $ toString fp
      return do
        oP <- src
        let uri = "source:" <> show oP
        return $ URILink uri anchor

processEntry :: (DocLike a, MonadLogger m, MonadUnliftIO m, MultiWalk MWTag a) => WalkM (PreProcessM m) -> a -> PreProcessM m a
processEntry f s = do
  env <- ask

  let sectionId = lookup "id" (getProps s)

      updateId = case sectionId of
        Just x | sectionId /= lookup "id" env.inhProps -> Just x
        _ -> Nothing

  newAttachDir <- join <$> forM updateId getAttachDir
  lift $ whenJust newAttachDir $ logDebugN . ("New attach dir: " <>) . show

  let childEnv _ =
        env
          & #inhProps %~ (<> getProps s)
          & #attachDir %~ maybe id (const . pure) newAttachDir

  local childEnv $ f s

findLinkSource :: (MonadIO m, MonadLogger m) => FilePath -> PreProcessM m (Maybe OrgPath)
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

getAttachDir :: MonadIO m => Text -> PreProcessM m (Maybe FilePath)
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
