module Main where
import Ema hiding (PrefixedRoute)
import Site.Roam qualified as R
import Site.Static qualified as S
import Site.Content qualified as C
import Org.Exporters.Heist
import Heist
import Control.Monad.Logger
import Config
import System.UnionMount as UM
import UnliftIO
import qualified Data.Yaml as Y
import Route
import Render (HeistS)
import Ema.Route.GenericClass (subRouteEncoder)

instance EmaSite Route where
  type SiteArg Route = Config
  siteInput act _ask_to_remove_encoder_from_siteinput cfg = do
    dH <- heistDynamic (templates cfg)
    dR <- siteInput @R.RoamRoute act noEnc (zettelkasten cfg)
    dS <- siteInput @S.StaticRoute act noEnc (static cfg)
    dC <- siteInput @C.ContentRoute act noEnc (content cfg)
    pure $ Model <$> dR <*> dS <*> dC <*> dH
    where
      noEnc = error "siteInput should not use encoder"
      heistDynamic :: (MonadUnliftIO m, MonadLogger m) => FilePath -> m (Dynamic m HeistS)
      heistDynamic dir =
        Dynamic <$> UM.mount dir [((), "**/*.tpl")] [] Nothing \_tag _fp _fa -> do
          let config = loadOrgTemplates
                       & hcTemplateLocations (pure . (loadTemplates dir :))
                       & runIdentity
          hc <- liftIO $ initHeist (config :: HeistConfig Exporter)
          case hc of
            Left e -> mapM_ (runStdoutLoggingT <$> logErrorNS "Template reloading" . toText) e
                      $> id
            Right hs -> pure $ const $ Just hs
  siteOutput enc model = \case
    RouteRoam r ->
      let roamM' = (roamM model) { R.heistS = heistS model }
      in siteOutput (subRouteEncoder @"RouteRoam" @"roamM" model enc) roamM' r
    RouteContent r ->
      let contentM' = (contentM model) { C.heistS = heistS model }
      in siteOutput (subRouteEncoder @"RouteContent" @"contentM" model enc) contentM' r
    RouteStatic r -> siteOutput (subRouteEncoder @"RouteStatic" @"staticM" model enc) (staticM model) r

main :: IO ()
main = do
  cfg <- Y.decodeFileThrow "abacate.yaml"
         `catch` (error . toText . Y.prettyPrintParseException)
  void $ runSite @Route cfg
