{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}
module Site.Organon.Dynamic (
  layoutDynamic,
  ondimDynamic,
  cacheDynamic,
  wsConnDynamic,
)
where

import Control.Monad.Logger
import Data.Map (singleton)
import Ema.Dynamic (Dynamic (..))
import Network.WebSockets qualified as WS
import Ondim.Targets.HTML.Load (loadTemplatesDynamic)
import Site.Org.Render.Types (Layouts, OndimState, templateDir)
import Site.Organon.Cache (cacheDynamic)
import System.FilePath (takeBaseName, (</>))
import System.UnionMount qualified as UM
import Text.XmlHtml qualified as X
import UnliftIO (MonadUnliftIO, TChan, newBroadcastTChanIO, writeTChan)

layoutDynamic :: (MonadUnliftIO m, MonadLoggerIO m) => FilePath -> m (Dynamic m Layouts)
layoutDynamic dir = do
  Dynamic <$> UM.mount dir [((), "**/*.html")] [] mempty \() fp _fa ->
    X.parseHTML fp <$> readFileBS (dir </> fp) >>= \case
      Left e -> logErrorNS "Template Loading" (toText e) >> liftIO (fail e)
      Right tpl -> do
        let name = fromString $ takeBaseName fp
        pure (singleton name (tpl, fp) <>)

ondimDynamic :: (MonadUnliftIO m, MonadLoggerIO m) => FilePath -> m (Dynamic m OndimState)
ondimDynamic dir = do
  ddir <- liftIO templateDir
  Dynamic <$> loadTemplatesDynamic [dir, ddir]

wsConnDynamic :: forall m. (MonadUnliftIO m, MonadLoggerIO m) => m (Dynamic m (TChan ByteString))
wsConnDynamic = do
  value <- newBroadcastTChanIO
  let manage :: m ()
      manage = do
        liftIO $ WS.runServer "127.0.0.1" 9160 \pendingConn -> do
          conn :: WS.Connection <- WS.acceptRequest pendingConn
          WS.withPingThread conn 30 pass $
            forever do
              msg <- WS.receiveData conn
              atomically $ writeTChan value msg
  return $ Dynamic (value, const manage)
