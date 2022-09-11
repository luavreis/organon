{-# LANGUAGE UndecidableInstances #-}

module Main where

import Cache (Cache, cache0, loadCache)
import Config
import Control.Monad.Logger
import Data.Binary (encodeFile)
import Data.Generics.Sum (AsAny (_As))
import Data.Map (singleton)
import Data.Yaml qualified as Y
import Ema
import Ema.Route.Generic
import Ema.Route.Lib.Extra.StaticRoute (staticRouteUrl)
import Ema.Route.Lib.Extra.StaticRoute qualified as SR
import Generics.SOP qualified as SOP
import Ondim
import Ondim.Extra.Loading.HTML (loadTemplatesDynamic)
import Optics.Core
import Org.Exporters.HTML
import Relude.Extra.Map
import Render
import Site.Roam qualified as O
import System.FilePath (takeBaseName, (</>))
import System.UnionMount as UM
import Text.XmlHtml qualified as X
import UnliftIO (MonadUnliftIO, catch, finally)
import UnliftIO.Directory (setCurrentDirectory)

data Route
  = RouteStatic (SR.StaticRoute "assets")
  | RouteContent O.Route
  deriving (Eq, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            Route
            '[ WithSubRoutes
                 '[ FolderRoute "assets" (SR.StaticRoute "assets"),
                    O.Route
                  ],
               WithModel Model
             ]
        )

data Model = Model
  { roamM :: O.Model,
    staticM :: SR.Model,
    ondimS :: OS,
    layouts :: Layouts
  }
  deriving (Generic)

instance EmaSite Route where
  type SiteArg Route = (Config, TVar Cache)
  siteInput act (cfg, cVar) = do
    dR <- siteInput @O.Route act (zettelkasten cfg, cVar)
    dS <- siteInput @(SR.StaticRoute "assets") act ()
    dO <- ondimDynamic (templates cfg)
    dL <- layoutDynamic (Config.layouts cfg)
    pure $ Model <$> dR <*> dS <*> dO <*> dL
    where
      layoutDynamic :: (MonadUnliftIO m, MonadLogger m) => FilePath -> m (Dynamic m Layouts)
      layoutDynamic dir = do
        Dynamic
          <$> UM.mount dir [((), "**/*.html")] [] mempty \() fp _fa ->
            X.parseHTML fp <$> readFileBS (dir </> fp) >>= \case
              Left e -> logErrorNS "Template Loading" (toText e) >> liftIO (fail e)
              Right tpl -> do
                let name = fromString $ takeBaseName fp
                pure $ (singleton name tpl <>)
      ondimDynamic :: (MonadUnliftIO m, MonadLogger m) => FilePath -> m (Dynamic m OS)
      ondimDynamic dir = do
        ddir <- liftIO htmlTemplateDir
        Dynamic <$> loadTemplatesDynamic [dir, ddir]
  siteOutput rp model route =
    case route of
      RouteContent r -> ondimOutput (rp % _As @O.Route) roamM r
      RouteStatic r -> siteOutput (rp % _As @"RouteStatic") (staticM model) r
    where
      ondimOutput ::
        (MonadLoggerIO m, SiteOutput r ~ OndimOutput, EmaSite r) =>
        Prism' FilePath r ->
        (Model -> RouteModel r) ->
        r ->
        m (SiteOutput Route)
      ondimOutput p s r =
        liftIO . renderWithLayout =<< siteOutput p (s model) r

      renderWithLayout = \case
        OAsset x -> x ostate
        OPage lname doc
          | Just layout <- (model ^. #layouts) !? lname ->
              AssetGenerated Html
                . either (error . show) id
                <$> doc ostate layout
          | otherwise -> error $ "Could not find layout " <> lname
        where
          files = keys $ model ^. #staticM ^. #modelFiles
          encExps =
            files <&> \file ->
              ( "asset:" <> toText file,
                pure $
                  staticRouteUrl
                    (rp % _As @"RouteStatic")
                    (model ^. #staticM)
                    file
              )
          extraTextExps = ("page:url", pure $ routeUrl rp route) : encExps
          ostate :: OS =
            model ^. #ondimS
              & lensVL ondimGState
                %~ (#textExpansions %~ (fromList extraTextExps <>))

main :: IO ()
main = do
  setCurrentDirectory "/home/lucas/dados/projetos/sites/gatil"
  cfg <-
    loadConfigWithDefault "abacate.yaml"
      `catch` (error . toText . Y.prettyPrintParseException)
  cVar <- newTVarIO cache0
  let cFile = cacheFile cfg
  runStdoutLoggingT $ loadCache cFile cVar
  void (runSite @Route (cfg, cVar))
    `finally` do
      cache <- readTVarIO cVar
      liftIO $ encodeFile cFile cache
