{-# LANGUAGE AllowAmbiguousTypes #-}

module Render where
import Ema
import Heist
import Heist.Interpreted
import Heist.Splices (ifElseISplice)
import Optics.Core
import Org.Exporters.Heist
import Control.Monad.Logger (MonadLogger, MonadLoggerIO, logErrorNS, runStdoutLoggingT)
import UnliftIO (MonadUnliftIO, race_)
import Ema.Route.Encoder (RouteEncoder, mapRouteEncoder)
import Data.Some
import System.FSNotify
import System.FilePath (isExtensionOf)
import Data.LVar qualified as LVar
import Ema.CLI qualified as CLI
import Generics.SOP qualified as SOP

ifElseSpliceWith :: Monad m => Bool -> Splices (Splice m) -> Splice m
ifElseSpliceWith p splices = localHS (bindSplices splices) $ ifElseISplice p

class IsRoute r => HeistSite r where
  type HSiteArg r :: Type
  type HSiteArg r = ()
  hSiteInput ::
    forall m.
    (MonadIO m, MonadUnliftIO m, MonadLoggerIO m) =>
    Some CLI.Action ->
    RouteEncoder (RouteModel r) r ->
    HSiteArg r ->
    m (Dynamic m (RouteModel r))
  hSiteOutput :: RouteEncoder (RouteModel r) r -> RouteModel r -> r -> Splice Exporter

data WithHeist model = WithHeist { model :: model, hState :: Maybe (HeistState Exporter) }

newtype HeistRoute route = HeistRoute { unHeistRoute :: route }
  deriving stock (Generic)
  deriving newtype (Eq, Show)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

toHeistEncoder :: RouteEncoder m r -> RouteEncoder (WithHeist m) (HeistRoute r)
toHeistEncoder = mapRouteEncoder
                 equality
                 (iso HeistRoute unHeistRoute)
                 model

fromHeistEncoder :: RouteEncoder (WithHeist m) (HeistRoute r) -> RouteEncoder m r
fromHeistEncoder = mapRouteEncoder
                   equality
                   (iso unHeistRoute HeistRoute )
                   (`WithHeist` Nothing)

-- Can this be derived via?
instance IsRoute r => IsRoute (HeistRoute r) where
  type RouteModel (HeistRoute r) = WithHeist (RouteModel r)
  routeEncoder = toHeistEncoder routeEncoder
  allRoutes m = HeistRoute <$> allRoutes (model m)

instance HeistSite r => EmaSite (HeistRoute r) where
  type SiteArg (HeistRoute r) = (LVar.LVar (Maybe (HeistState Exporter)), HSiteArg r)
  siteInput ac enc arg = adapter (fst arg) =<< hSiteInput ac (fromHeistEncoder enc) (snd arg)
    where
      adapter :: forall m model. (MonadUnliftIO m, MonadLogger m)
        => LVar.LVar (Maybe (HeistState Exporter))
        -> Dynamic m model
        -> m (Dynamic m (WithHeist model))
      adapter hvar d0 = do
        h0 <- LVar.get hvar
        pure $ liftA2 WithHeist d0 (Dynamic (h0, hLoop))
        where
          hLoop :: (Maybe (HeistState Exporter) -> m ()) -> m ()
          hLoop send = do
            lid <- LVar.addListener hvar
            forever do
              send =<< LVar.listenNext hvar lid
  siteOutput enc m r =
    case hState m of
      Just st -> AssetGenerated Html $ renderSpliceToDoc st renderSettings splice
      Nothing -> error "Heist exporter state is empty!"
    where
      splice = hSiteOutput (fromHeistEncoder enc) (model m) (unHeistRoute r)
      renderSettings = defaultExporterSettings
        { headlineLevelShift = 1
        }

runHeistSite ::
  forall r.
  (Show r, Eq r, HeistSite r) =>
  HSiteArg r ->
  IO ()
runHeistSite arg = do
  h0 <- load
  hvar <- LVar.new h0
  withManager \mgr -> do
    stop <- watchDir mgr source predicate \_ -> do
      hst <- load
      LVar.set hvar hst
    _ <- runSite @(HeistRoute r) (hvar, arg)
    stop
  where
    source = "demo/templates"
    predicate e = eventIsDirectory e || ".tpl" `isExtensionOf` eventPath e
    load :: (MonadIO m) => m (Maybe (HeistState Exporter))
    load = do
      let config = loadOrgTemplates
                   & runIdentity . hcTemplateLocations (\t -> pure $ loadTemplates source : t)
      hc <- liftIO $ initHeist config
      case hc of
        Left e -> mapM_ (runStdoutLoggingT <$> logErrorNS "Template reloading" . toText) e $> Nothing
        Right hs -> pure $ Just hs
