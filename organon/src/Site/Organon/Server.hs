{-# LANGUAGE AllowAmbiguousTypes #-}

module Site.Organon.Server (runOrganon) where

import Control.Monad.Logger
import Control.Monad.Logger.Extras (runLoggerLoggingT)
import Data.ByteString (hPut)
import Data.Dependent.Sum (DSum (..))
import Data.Text qualified as T
import Ema
import Ema.CLI qualified as CLI
import Ema.Server (EmaServerOptions (..), EmaWsRenderer, decodeUrlRoute, defaultEmaWsRenderer)
import GHC.IO.Handle.FD (withFileBlocking)
import Network.WebSockets qualified as WS
import Site.Org.Model
import Site.Organon.Model
import Site.Organon.Route (Route)
import System.FilePath ((</>))
import System.Info qualified as Info
import Text.Slugify (slugify)
import UnliftIO.Directory (getTemporaryDirectory)
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

customEmaWs :: EmaWsRenderer Route
customEmaWs conn model path =
  case decodeUrlRoute @Route model path of
    Right Nothing
      | path == "#!redirected" -> do
          tmpDir <- getTemporaryDirectory
          let fifoFp = tmpDir </> "organon.fifo"
          liftIO $ withFileBlocking fifoFp WriteMode (`hPut` mempty)
          return Nothing
      | Just fp <- T.stripPrefix "#!open:" path -> do
          log LevelInfo $ "Opening file " <> fp
          case Info.os of
            "darwin" -> callCommand $ "open " ++ toString fp
            "mingw32" -> callCommand $ "start " ++ toString fp
            "linux" -> callCommand $ "xdg-open " ++ toString fp
            _ -> log LevelError "Opening files in this OS is not supported."
          return Nothing
    Right (Just _)
      | Just targetLoc <- model.targetLocation
      , Just entry <- lookupOrgLocation model.org.pages targetLoc.locationPage -> do
          let pagePath = routeUrl (fromPrism_ $ routePrism model.org) entry.identifier
              anchor = maybe "" (("#" <>) . slugify) targetLoc.locationAnchor
          liftIO $ WS.sendTextData conn $ "REDIRECT " <> pagePath <> anchor
          return Nothing
    -- defaultEmaWsRenderer @Route conn s pagePath
    _ -> defaultEmaWsRenderer @Route conn model path
  where
    log = logWithoutLoc "Organon WS"
