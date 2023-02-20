{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}
module Site.Organon.Dynamic where

import Control.Monad.Logger (LogLevel (LevelDebug), MonadLogger, logErrorNS, logInfoNS, logWithoutLoc)
import Data.Aeson (decodeStrict)
import Data.Binary (encodeFile)
import Data.ByteString (hGetLine)
import Data.Map (singleton)
import Data.Map qualified as Map
import Ema.Dynamic (Dynamic (..))
import GHC.IO.Handle.FD (withFileBlocking)
import Ondim.Targets.HTML.Load (loadTemplatesDynamic)
import Org.Exporters.HTML (htmlTemplateDir)
import Site.Org.Model (OrgID (..), findSource, prettyOrgPath)
import Site.Org.Options (Options (..))
import Site.Org.Render.Types (Layouts, OndimMS)
import Site.Organon.Cache (Cache, loadCache)
import Site.Organon.Config (Config (..))
import Site.Organon.Model (TargetLocation (TargetLocation))
import System.FilePath (dropExtension, takeBaseName, (</>))
import System.IO.Error (IOError)
import System.Posix
import System.UnionMount qualified as UM
import Text.XmlHtml qualified as X
import UnliftIO (MonadUnliftIO, catch, finally)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory (getTemporaryDirectory, removeFile)

layoutDynamic :: (MonadUnliftIO m, MonadLogger m) => FilePath -> m (Dynamic m Layouts)
layoutDynamic dir = do
  Dynamic
    <$> UM.mount dir [((), "**/*.html")] [] mempty \() fp _fa ->
      X.parseHTML fp <$> readFileBS (dir </> fp) >>= \case
        Left e -> logErrorNS "Template Loading" (toText e) >> liftIO (fail e)
        Right tpl -> do
          let name = fromString $ takeBaseName fp
          pure (singleton name tpl <>)

ondimDynamic :: (MonadUnliftIO m, MonadLogger m) => FilePath -> m (Dynamic m OndimMS)
ondimDynamic dir = do
  ddir <- liftIO htmlTemplateDir
  Dynamic <$> loadTemplatesDynamic [dir, ddir]

cacheDynamic :: (MonadUnliftIO m, MonadLogger m) => FilePath -> m (Dynamic m (TVar Cache))
cacheDynamic file = do
  cVar <- loadCache file
  let close _ =
        forever (threadDelay maxBound) `finally` do
          logInfoNS "Organon" "Writing cache to disk."
          cache <- readTVarIO cVar
          liftIO $ encodeFile file cache
  pure $ Dynamic (cVar, close)

lastPathDynamic :: forall m. (MonadUnliftIO m, MonadLogger m) => Config -> Dynamic m (Maybe TargetLocation)
lastPathDynamic cfg = Dynamic (Nothing, watch)
  where
    watch :: (Maybe TargetLocation -> m ()) -> m ()
    watch f = do
      tmpDir <- getTemporaryDirectory
      let fifoFp = tmpDir </> "organon.fifo"
          loop = do
            input <- liftIO do
              whenM (fileExist fifoFp) $
                unlessM (isNamedPipe <$> getFileStatus fifoFp) $
                  removeFile fifoFp
              unlessM (fileExist fifoFp) $
                createNamedPipe fifoFp ownerModes
              withFileBlocking fifoFp ReadMode hGetLine
                `catch` (\(_ :: IOError) -> pure "")
            log LevelDebug $ "Received signal " <> show input
            f =<< runMaybeT do
              obj :: Map Text (Maybe Text) <- hoistMaybe $ decodeStrict input
              let anchor = join $ Map.lookup "anchor" obj
              case join $ Map.lookup "id" obj of
                Just oId -> return $ TargetLocation (Right (OrgID oId)) anchor
                Nothing -> do
                  fp <- hoistMaybe $ join $ Map.lookup "file" obj
                  let fp' = dropExtension (toString fp)
                  source <- hoistMaybe =<< findSource sources fp'
                  lift $ log LevelDebug $ "Signal matches " <> prettyOrgPath source
                  return $ TargetLocation (Left source) anchor
            loop
      loop `finally` removeFile fifoFp
    sources = cfg.orgFiles.mount
    log :: LogLevel -> Text -> m ()
    log = logWithoutLoc "Organon Socket"
