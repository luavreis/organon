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
import Ondim.Extra (lookupAttr')
import Ondim.Targets.HTML (HtmlNode (..))
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
            | Just (layout, fp) <- model.layouts !? lname ->
                doc layout fp
                  `binding` do
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
                    "o:parse" ## parseObjectsExp (backend model.org.pages (rp % _As @Org.Route))
                    "org:with-settings" #* withSettingsExp
            | otherwise -> throwTemplateError $ "Could not find layout " <> lname
        where
          handleErrors = fmap (either (error . show) id)
          files = keys model.static.modelFiles
          ostate = model.ondim

targetFilter :: Text -> Filter HtmlNode
targetFilter p _ thunk = do
  nodes <- thunk
  forM nodes \node -> do
    attrs <- attributes node
    let id_ = L.lookup "id" attrs
        attrs_ = filter (("id" /=) . fst) attrs
        newNode = case node of
          Element {} -> node {elementAttrs = attrs_}
          _ -> node
    if Just p == id_
      then returnEarly (one newNode)
      else return newNode

portalExp :: Expansion HtmlNode
portalExp node = do
  target <- lookupAttr' "target" node
  either id id
    <$> createPortal (liftChildren node)
    `bindingFilters` do
      "target" $# targetFilter target
