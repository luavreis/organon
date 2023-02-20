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
import Site.Organon.Model
import Site.Organon.Route (Route)
import System.FilePath (dropExtension)
import System.Info qualified as Info
import Text.Slugify (slugify)
import UnliftIO (conc, runConc)
import UnliftIO.Process (callCommand)

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
      input <- liftIO $ model.wsNextMsg
      log LevelDebug $ "Received signal " <> show input
      _ <- runMaybeT do
        obj :: Map Text (Maybe Text) <- hoistMaybe $ decodeStrict input
        let anchor = join $ Map.lookup "anchor" obj
        loc <- case join $ Map.lookup "id" obj of
          Just oId -> return (Right (OrgID oId))
          Nothing -> do
            fp <- hoistMaybe $ join $ Map.lookup "file" obj
            let fp' = dropExtension (toString fp)
            source <- hoistMaybe =<< findSource model.org.options.mount fp'
            lift $ log LevelDebug $ "Signal matches " <> prettyOrgPath source
            return (Left source)
        entry <- hoistMaybe $ lookupOrgLocation model.org.pages loc
        let pagePath = routeUrl (fromPrism_ $ routePrism model.org) entry.identifier
            anchor' = maybe "" slugify anchor
        liftIO $ WS.sendTextData conn $ "SWITCH " <> pagePath <> "#" <> anchor'
      followRedirect

    log = logWithoutLoc "Organon WS"
