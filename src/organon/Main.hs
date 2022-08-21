{-# LANGUAGE UndecidableInstances #-}

module Main where

import Cache (Cache, cache0, loadCache)
import Config
import Control.Monad.Logger
import Data.Binary (encodeFile)
import Data.Generics.Sum (AsAny (_As))
import Data.Yaml qualified as Y
import Ema
import Ema.Route.Generic
import Generics.SOP qualified as SOP
import Ondim (OndimS (expansions))
import Ondim.HTML (fromDocument)
import Optics.Core
import Org.Exporters.HTML
import Relude.Extra.Map
import Site.Org qualified as C
import Site.Roam qualified as R
import Site.Static qualified as S
import System.FilePath (takeBaseName, (</>))
import System.UnionMount as UM
import Text.XmlHtml qualified as X
import UnliftIO (MonadUnliftIO, catch, finally)
import UnliftIO.Directory (setCurrentDirectory)
import Render
import Data.Map (singleton)

data Route
  = RouteRoam R.RoamRoute
  | RouteStatic S.StaticRoute
  | RouteContent C.ContentRoute
  deriving (Eq, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            Route
            '[ WithSubRoutes
                 '[ FolderRoute "zettel" R.RoamRoute,
                    FolderRoute "assets" S.StaticRoute,
                    C.ContentRoute
                  ],
               WithModel Model
             ]
        )

data Model = Model
  { roamM :: R.Model,
    staticM :: S.Model,
    contentM :: C.Model,
    ondimS :: OS,
    layouts :: Layouts
  }
  deriving (Generic)

instance EmaSite Route where
  type SiteArg Route = (Config, TVar Cache)
  siteInput act (cfg, cVar) = do
    dR <- siteInput @R.RoamRoute act (zettelkasten cfg, cVar)
    dS <- siteInput @S.StaticRoute act (static cfg, ())
    dC <- siteInput @C.ContentRoute act (content cfg, cVar)
    dO <- ondimDynamic (templates cfg)
    dL <- layoutDynamic (Config.layouts cfg)
    pure $ Model <$> dR <*> dS <*> dC <*> dO <*> dL
    where
      layoutDynamic :: (MonadUnliftIO m, MonadLogger m) => FilePath -> m (Dynamic m Layouts)
      layoutDynamic dir = do
        Dynamic <$>
          UM.mount dir [((), "**/*.html")] [] mempty \() fp _fa ->
            X.parseHTML fp <$> readFileBS (dir </> fp) >>= \case
              Left e -> logErrorNS "Template Loading" (toText e) >> liftIO (fail e)
              Right tpl -> do
                let name = fromString $ takeBaseName fp
                pure $ (singleton name tpl <>)
      ondimDynamic :: (MonadUnliftIO m, MonadLogger m) => FilePath -> m (Dynamic m OS)
      ondimDynamic dir = do
        s0 <- liftIO $ loadTemplates =<< htmlTemplateDir
        Dynamic <$> UM.mount dir [((), "**/*.tpl")] [] s0 \_tag fp _fa ->
          X.parseHTML fp <$> readFileBS (dir </> fp) >>= \case
            Left e -> logErrorNS "Template Loading" (toText e) >> liftIO (fail e)
            Right tpl -> do
              let name = fromString $ takeBaseName fp
                  expansion = fromDocument tpl
              pure $ \s -> s {expansions = insert name expansion (expansions s)}
  siteOutput rp model =
    fmap (renderWithLayout model) . \case
      RouteRoam r -> siteOutput (rp % _As @"RouteRoam") (roamM model) r
      RouteContent r -> siteOutput (rp % _As @"RouteContent") (contentM model) r
      RouteStatic r -> siteOutput (rp % _As @"RouteStatic") (staticM model) r

main :: IO ()
main = do
  setCurrentDirectory "/home/lucas/dados/projetos/sites/gatil"
  cfg <-
    Y.decodeFileThrow "abacate.yaml"
      `catch` (error . toText . Y.prettyPrintParseException)
  cVar <- newTVarIO cache0
  let cFile = cacheFile cfg
  runStdoutLoggingT $ loadCache cFile cVar
  void (runSite @Route (cfg, cVar))
    `finally` do
      cache <- readTVarIO cVar
      liftIO $ encodeFile cFile cache
