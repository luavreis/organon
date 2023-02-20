{-# LANGUAGE UndecidableInstances #-}

module Site.Organon.Route where

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
import Site.Org.Model qualified as O
import Site.Org.Render
import Site.Organon.Config qualified as Config
import Site.Organon.Dynamic
import Site.Organon.Extra.LaTeX (renderLaTeXExp)
import Site.Organon.Extra.Query (queryExp)
import Site.Organon.Model (Model (..))
import Site.Organon.Extra.Regex (regexExp)

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

instance EmaSite Route where
  type SiteArg Route = Config.Config
  siteInput act cfg = do
    dR <- siteInput @O.Route act (Config.orgFiles cfg)
    dS <- siteInput @(SR.StaticRoute "assets") act ()
    dO <- ondimDynamic (Config.templates cfg)
    dL <- layoutDynamic (Config.layouts cfg)
    dC <- cacheDynamic (Config.cacheFile cfg)
    let dP = lastPathDynamic cfg
    return $ Model <$> dR <*> dS <*> dO <*> dL <*> dC ?? Config.extraOptions cfg ?? isLiveServer act <*> dP

  siteOutput rp model route =
    case route of
      RouteContent r -> ondimOutput (rp % _As @O.Route) orgM r
      RouteStatic r -> siteOutput (rp % _As @"RouteStatic") (staticM model) r
    where
      ondimOutput ::
        (RenderM m, SiteOutput r ~ OndimOutput, EmaSite r) =>
        Prism' FilePath r ->
        (Model -> RouteModel r) ->
        r ->
        m (SiteOutput Route)
      ondimOutput p s r =
        renderWithLayout =<< siteOutput p (s model) r

      renderWithLayout :: RenderM m => OndimOutput -> m (Asset LByteString)
      renderWithLayout =
        handleErrors . evalOutput ostate . \case
          AssetOutput x -> x
          PageOutput lname doc
            | Just layout <- (model ^. #layouts) !? lname ->
                doc layout
                  `bindingText` do
                    when (liveServer model) $ "organon:live-server" ## pure ""
                    prefixed "asset:" $ forM_ files \file ->
                      toText file ## pure $ SR.staticRouteUrl (rp % _As @"RouteStatic") (model ^. #staticM) file
                  `binding` do
                    "query" ## queryExp (rp % _As @O.Route) (orgM model)
                    "organon:latex" ## renderLaTeXExp model
                    "utils:regex" ## regexExp
                    "unwrap" ## unwrapExp
                    "portal" ## portalExp
                    "o:parse" ## parseObjectsExp (backend (orgM model ^. #_mPages) (rp % _As @O.Route))
            | otherwise -> throwCustom $ "Could not find layout " <> lname
        where
          handleErrors = fmap (either (error . show) id)
          files = keys $ model ^. (#staticM % #modelFiles)
          ostate = model ^. #ondimS

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
