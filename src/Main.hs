{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Main where
import Ema hiding (PrefixedRoute)
import Ema.Multi
import Place ()
import Site.Roam
import Site.Static
import Site.Content
import qualified Data.LVar as LVar
import Org.Exporters.Heist
import Heist
import System.FSNotify
import System.FilePath
import Control.Monad.Logger
import Config
import Data.Yaml as Y
import Control.Exception (catch)

data Route
  = Route_Roam RoamRoute
  | Route_Content ContentRoute
  | Route_Static StaticRoute
  deriving stock (Eq, Show, Ord)

instance IsRoute Route where
  type RouteModel Route = Model
  routeEncoder =
    combineRouteEncoder
      routeDecomposition
      decomposeModel
      (routeEncoder @MarkdownRoute)
      (routeEncoder @StaticRoute)
  allRoutes model =
    fmap Route_Markdown (allRoutes @MarkdownRoute $ fst . decomposeModel $ model)
      <> fmap Route_Static (allRoutes @StaticRoute $ snd . decomposeModel $ model)

main :: IO ()
main = do
  cfg <- Y.decodeFileThrow "abacate.yaml"
         `catch` (error . toText . prettyPrintParseException)
  let tpls = templates (sources cfg)
  h0 <- load tpls
  hvar <- LVar.new h0
  withManager \mgr -> do
    stop <- watchDir mgr tpls predicate \_ -> do
      hst <- load tpls
      LVar.set hvar hst
    void $
      runSite
        @(MultiRoute '[Boo, Boo, Boo])
        (I (cfg, hvar) :* I (cfg, hvar) :* I cfg :* Nil)
    stop
  where
    predicate e = eventIsDirectory e || ".tpl" `isExtensionOf` eventPath e
    load :: (MonadIO m) => FilePath -> m (Maybe (HeistState Exporter))
    load source = do
      let config = loadOrgTemplates
                   & runIdentity . hcTemplateLocations (\t -> pure $ loadTemplates source : t)
      hc <- liftIO $ initHeist config
      case hc of
        Left e -> mapM_ (runStdoutLoggingT <$> logErrorNS "Template reloading" . toText) e $> Nothing
        Right hs -> pure $ Just hs
