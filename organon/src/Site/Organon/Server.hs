{-# LANGUAGE AllowAmbiguousTypes #-}

module Site.Organon.Server where

import Control.Monad.Logger
import Control.Monad.Logger.Extras (runLoggerLoggingT)
import Data.Dependent.Sum (DSum (..))
import Data.Text qualified as T
import Ema
import Ema.CLI qualified as CLI
import Ema.Server (EmaServerOptions (..), EmaWsRenderer, decodeUrlRoute, defaultEmaWsRenderer)
import Network.WebSockets qualified as WS
import Optics.Core
import Site.Org.Model hiding (Route)
import Site.Organon.Model
import Site.Organon.Route (Route)
import System.Info qualified as Info
import Text.Slugify (slugify)
import UnliftIO.Process (callCommand)
import UnliftIO.Directory (getTemporaryDirectory)
import System.FilePath ((</>))
import GHC.IO.Handle.FD (withFileBlocking)
import Data.ByteString (hPut)

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
customEmaWs conn s path =
  case decodeUrlRoute @Route s path of
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
      | Just targetLoc <- s ^. #targetLocation
      , Just r' <- _identifier <$> lookupOrgLocation (s ^. #orgM % #_mPages) (targetLoc ^. #locationPage) -> do
          let pagePath = routeUrl (fromPrism_ $ routePrism (s ^. #orgM)) r'
              anchor = maybe "" (("#" <>) . slugify) (targetLoc ^. #locationAnchor)
          liftIO $ WS.sendTextData conn $ "REDIRECT " <> pagePath <> anchor
          return Nothing
    -- defaultEmaWsRenderer @Route conn s path'
    _ -> defaultEmaWsRenderer @Route conn s path
  where
    log = logWithoutLoc "Organon WS"
