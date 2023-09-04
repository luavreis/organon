{-# LANGUAGE UndecidableInstances #-}

module Site.Organon.Route (Route (..)) where

import Data.Generics.Sum.Any
import Data.List qualified as L
import Data.Map (keys, (!?))
import Ema
import Ema.CLI (isLiveServer)
import Ema.Route.Generic
import Ema.Route.Lib.Extra.StaticRoute qualified as SR
import Generics.SOP qualified as SOP
import Ondim.Extra.Exceptions (prettyException)
import Ondim.Targets.HTML (defaultState)
import Optics.Core
import Site.Org ()
import Site.Org.Render
import Site.Org.Route qualified as Org
import Site.Organon.Config
import Site.Organon.Dynamic
import Site.Organon.Extra.LaTeX
import Site.Organon.Extra.Query
import Site.Organon.Extra.Regex
import Site.Organon.Model

data Route
  = RouteStatic (SR.StaticRoute "assets")
  | RouteContent Org.Route
  deriving (Eq, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            Route
            '[ WithSubRoutes
                '[ FolderRoute "assets" (SR.StaticRoute "assets")
                 , Org.Route
                 ]
             , WithModel Model
             ]
        )

instance EmaSite Route where
  type SiteArg Route = Config
  siteInput act cfg = do
    dR <- siteInput @Org.Route act cfg.orgFiles
    dS <- siteInput @(SR.StaticRoute "assets") act ()
    dO <- ondimDynamic cfg.templates
    dC <- cacheDynamic cfg.cacheFile
    dW <- wsConnDynamic
    return $ Model <$> dR <*> dS <*> dO <*> dC ?? cfg.extraOptions ?? isLiveServer act <*> dW

  siteOutput rp model route =
    case route of
      RouteContent r -> ondimOutput (rp % _As @Org.Route) model.org r
      RouteStatic r -> siteOutput (rp % _As @"RouteStatic") model.static r
    where
      ondimOutput p m r = render =<< siteOutput p m r

      globalExps = do
        when model.liveServer $ "organon:live-server" #@ "" -- TODO get from cli data
        "asset" #. forM_ files \file ->
          toText file #@ SR.staticRouteUrl (rp % _As @"RouteStatic") model.static file
        "query" ## queryExp (rp % _As @Org.Route) model.org
        "organon:latex" ## renderLaTeXExp model
        "utils:sum" #* \node -> do
          nums :: [Int] <- mapMaybe (readMaybe . toString . snd) <$> attributes node
          return $ fromMaybe [] $ fromText ?? show (sum nums)
        "utils:regex" #* regexExp
        "portal" ## portalExp
        where
          files = keys model.static.modelFiles

      render :: RenderM m => Ondim (Asset LByteString) -> m (Asset LByteString)
      render res =
        either handleErrors return
          =<< evalOutput ostate (res `binding` globalExps)
        where
          handleErrors e = do
            let msg = prettyException e
            logErrorN msg
            return $ AssetGenerated Html $ "<pre>" <> encodeUtf8 msg <> "</pre>"
          ostate = model.ondim <> defaultState
