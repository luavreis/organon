{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}
module Site.Organon.Dynamic (
  layoutDynamic,
  ondimDynamic,
  cacheDynamic,
  lastPathDynamic,
)
where

import Control.Monad.Logger (LogLevel (LevelDebug), MonadLogger, logErrorNS, logWithoutLoc)
import Data.Aeson (decodeStrict)
import Data.ByteString (hGetLine)
import Data.Map (singleton)
import Data.Map qualified as Map
import Ema.Dynamic (Dynamic (..))
import GHC.IO.Handle.FD (withFileBlocking)
import Ondim.Targets.HTML.Load (loadTemplatesDynamic)
import Org.Exporters.HTML (htmlTemplateDir)
import Site.Org.Model
import Site.Org.Options
import Site.Org.Render.Types
import Site.Organon.Cache
import Site.Organon.Config
import Site.Organon.Model
import System.FilePath (dropExtension, takeBaseName, (</>))
import System.IO.Error (IOError)
import System.Posix
import System.UnionMount qualified as UM
import Text.XmlHtml qualified as X
import UnliftIO (MonadUnliftIO, catch, finally)
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
