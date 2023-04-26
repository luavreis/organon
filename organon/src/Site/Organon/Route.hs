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
import Ondim.Extra (getAttrs, lookupAttr', prefixed)
import Ondim.Targets.HTML (HtmlNode, HtmlTag)
import Optics.Core
import Site.Org ()
import Site.Org.Model qualified
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
    dL <- layoutDynamic cfg.layouts
    dC <- cacheDynamic cfg.cacheFile
    dW <- wsConnDynamic
    return $ Model <$> dR <*> dS <*> dO <*> dL <*> dC ?? cfg.extraOptions ?? isLiveServer act <*> dW

  siteOutput rp model route =
    case route of
      RouteContent r -> ondimOutput (rp % _As @Org.Route) model.org r
      RouteStatic r -> siteOutput (rp % _As @"RouteStatic") model.static r
    where
      ondimOutput ::
        (RenderM m, SiteOutput r ~ OndimOutput, EmaSite r) =>
        Prism' FilePath r ->
        RouteModel r ->
        r ->
        m (SiteOutput Route)
      ondimOutput p m r = renderWithLayout =<< siteOutput p m r

      renderWithLayout :: RenderM m => OndimOutput -> m (Asset LByteString)
      renderWithLayout =
        handleErrors . evalOutput ostate . \case
          AssetOutput x -> x
          PageOutput lname doc
            | Just layout <- model.layouts !? lname ->
                doc layout
                  `bindingText` do
                    when model.liveServer $ "organon:live-server" ## pure ""
                    prefixed "asset:" $ forM_ files \file ->
                      toText file ## pure $ SR.staticRouteUrl (rp % _As @"RouteStatic") model.static file
                  `binding` do
                    "query" ## queryExp (rp % _As @Org.Route) model.org
                    "organon:latex" ## renderLaTeXExp model
                    "utils:regex" ## regexExp
                    "portal" ## portalExp
                    "o:parse" ## parseObjectsExp (backend model.org.pages (rp % _As @Org.Route))
            | otherwise -> throwCustom $ "Could not find layout " <> lname
        where
          handleErrors = fmap (either (error . show) id)
          files = keys model.static.modelFiles
          ostate = model.ondim

targetFilter :: Text -> Filter HtmlNode
targetFilter p thunk = do
  nodes <- thunk
  forM nodes \node -> do
    let id_ = L.lookup "id" $ getAttrs @HtmlTag node
    if Just p == id_
      then returnEarly (one node)
      else return node

portalExp :: Expansion HtmlNode
portalExp node = do
  target <- lookupAttr' "target" node
  either id id
    <$> createPortal (liftChildren node)
    `bindingFilters` do
      "target" ## targetFilter target
