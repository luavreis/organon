{-# LANGUAGE AllowAmbiguousTypes #-}

module Site.Organon.Server (runOrganon) where

import Control.Monad.Logger
import Control.Monad.Logger.Extras (runLoggerLoggingT)
import Data.Aeson (decodeStrict)
import Data.Dependent.Sum (DSum (..))
import Data.Map qualified as Map
import Data.Text qualified as T
import Ema
import Ema.CLI qualified as CLI
import Ema.Server (EmaServerOptions (..), EmaWsHandler)
import Network.WebSockets qualified as WS
import Site.Org.Model
import Site.Org.Options
import Site.Org.Route qualified as OR
import Site.Organon.Model
import Site.Organon.Route (Route (..))
import System.Info qualified as Info
import UnliftIO (conc, runConc)
import UnliftIO.Process (callCommand)
import UnliftIO.STM (dupTChan, readTChan)
import Org.Exporters.Processing.InternalLinks (sectionTitleToAnchor)

runOrganon ::
  SiteArg Route ->
  IO ()
runOrganon input = do
  cli <- CLI.cliAction
  result <- snd <$> runSiteWithServerOpts @Route emaServerOptions cli input
  case result of
    CLI.Run _ :=> Identity () ->
      flip runLoggerLoggingT (CLI.getLogger cli) $
        CLI.crash "ema" "Live server unexpectedly stopped"
    CLI.Generate _ :=> Identity _ ->
      pass

emaServerOptions :: EmaServerOptions Route
emaServerOptions = EmaServerOptions "" customEmaWs

customEmaWs :: EmaWsHandler Route
customEmaWs conn model =
  runConc $
    asum
      [ conc customMessage
      , conc followRedirect
      ]
  where
    customMessage = do
      msg <- liftIO $ WS.receiveData conn
      log LevelDebug $ "<~~ " <> show msg
      if
          | Just fp <- T.stripPrefix "#!open:" msg -> do
              log LevelInfo $ "Opening file " <> fp
              case Info.os of
                "darwin" -> callCommand $ "open " ++ toString fp
                "mingw32" -> callCommand $ "start " ++ toString fp
                "linux" -> callCommand $ "xdg-open " ++ toString fp
                _ -> log LevelError "Opening files in this OS is not supported."
              customMessage
          | otherwise -> pure msg

    followRedirect = do
      c <- atomically $ dupTChan model.wsNextMsg
      input <- atomically $ readTChan c
      log LevelDebug $ "Received signal " <> show input
      _ <- runMaybeT do
        obj :: Map Text (Maybe Text) <- hoistMaybe $ decodeStrict input
        let anchor = join $ Map.lookup "anchor" obj
        loc <- case join $ Map.lookup "id" obj of
          Just oId -> return (Right (OrgID oId))
          Nothing -> do
            fp <- hoistMaybe $ join $ Map.lookup "file" obj
            let fp' = toString fp
            source <- hoistMaybe =<< findSource model.org.options.mount fp'
            lift $ log LevelDebug $ "Signal matches " <> prettyOrgPath source
            return (Left source)
        entry <- hoistMaybe $ lookupOrgLocation model.org.pages loc
        let pagePath = routeUrl rp (RouteContent $ OR.Route_Page entry.identifier)
            anchor' = maybe "" sectionTitleToAnchor anchor
        liftIO $ WS.sendTextData conn $ "SWITCH " <> pagePath <> "#" <> anchor'
      followRedirect
    rp = fromPrism_ $ routePrism @Route model
    log = logWithoutLoc "Organon WS"
