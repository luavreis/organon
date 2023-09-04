{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}
module Site.Organon.Dynamic (
  ondimDynamic,
  cacheDynamic,
  wsConnDynamic,
)
where

import Control.Monad.Logger
import Ema.Dynamic (Dynamic (..))
import Network.WebSockets qualified as WS
import Ondim.Extra.Loading (loadTemplatesDynamic, LoadConfig (..))
import Site.Org.Render.Types (OndimState)
import Ondim.Targets.HTML (loadHtml)
import Site.Organon.Cache (cacheDynamic)
import UnliftIO (MonadUnliftIO, TChan, newBroadcastTChanIO, writeTChan)
import Org.Exporters.Data.Templates (templatesEmbed)

ondimDynamic :: (MonadUnliftIO m, MonadLoggerIO m) => FilePath -> m (Dynamic m OndimState)
ondimDynamic dir = do
  Dynamic <$> loadTemplatesDynamic [load] [dir]
  where
    load = loadHtml {initialState = templatesEmbed [loadHtml]}

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
