{-# LANGUAGE AllowAmbiguousTypes #-}

module Site.Organon.Server (runOrganon) where

import Control.Monad.Logger
import Data.Aeson (decodeStrict)
import Data.Map qualified as Map
import Ema
import Ema.CLI qualified as CLI
import Ema.Server (EmaServerOptions (..), EmaWsHandler)
import Network.WebSockets qualified as WS
import Site.Org.Model
import Site.Org.Options
import Site.Org.Route qualified as OR
import Site.Organon.Model
import Site.Organon.Route (Route (..))
import UnliftIO (conc, runConc)
import UnliftIO.STM (dupTChan, readTChan)
import Org.Exporters.Processing.InternalLinks (sectionTitleToAnchor)
import Control.Monad.Logger.Extras (runLoggerLoggingT)

runOrganon ::
  SiteArg Route ->
  IO ()
runOrganon input = do
  cli <- CLI.cliAction
  result <- snd <$> runSiteWithServerOpts @Route emaServerOptions cli input
  case result of
    RunResult () ->
      flip runLoggerLoggingT (CLI.getLogger cli) $
        CLI.crash "ema" "Live server unexpectedly stopped"
    GenerateResult _ -> pass

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
      pure msg

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
