{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Site.Org (ContentRoute, Model (..), Options (..)) where

import Cache (Cache)
import Data.Map (delete, insert, (!))
import Data.Set qualified as Set (map)
import Ema
import Ema.Route.Generic
import Generics.SOP qualified as SOP
import LaTeX
import Optics.Core
import Org.Parser
import Org.Types
import OrgAttach
import Place
import Render
import Routes qualified as R
import Site.Org.Links
import Site.Org.Options
import System.FilePath (dropExtension, takeDirectory, (</>))
import System.UnionMount (FileAction (..))
import System.UnionMount qualified as UM
import Text.XmlHtml qualified as X
import UnliftIO.Concurrent (threadDelay)
import Org.Exporters.HTML (renderDoc)

data Model = Model
  { mount :: FilePath,
    serveAt :: FilePath,
    docs :: Map DocRoute OrgDocument,
    files :: Set FileRoute',
    attachments :: AttachModel,
    layouts :: Map Text X.Document
  }
  deriving (Generic)

model0 :: Model
model0 = Model "" mempty mempty mempty emptyAttachModel mempty

newtype DocRoute = DocRoute Text
  deriving (Eq, Ord, Show)
  deriving (R.StringRoute) via (R.HtmlRoute Text)
  deriving (IsRoute) via (R.MapRoute DocRoute OrgDocument)
  deriving newtype (IsString)

newtype FileRoute' = FileRoute' FilePath
  deriving (Eq, Ord, Show)
  deriving (R.StringRoute) via (R.FileRoute' String)
  deriving (IsRoute) via (R.SetRoute FileRoute')

data Route
  = Doc DocRoute
  | File FileRoute'
  | Attch AttachRoute
  deriving (Eq, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            Route
            '[ WithSubRoutes '[DocRoute, FileRoute', AttachRoute],
               WithModel Model
             ]
        )

instance EmaSite Route where
  type SiteArg Route = (Options, TVar Cache)
  type SiteOutput Route = OndimOutput
  siteInput _ arg@(opt, _) =
    Dynamic <$> UM.mount source include exclude' model0 handler
    where
      source = opt ^. #mount
      include = [((), "**/*.org")]
      exclude' = exclude opt
      run x = runReaderT x arg
      handler () fp =
        let place = Place fp source
            key = fromString $ dropExtension fp
         in \case
              Refresh _ () -> do
                threadDelay 200
                run do
                  orgdoc <-
                    parseOrgIO defaultOrgOptions (source </> fp)
                      >>= processLaTeX place
                      >>= putKaTeXPreamble place
                  let (orgdoc', files') = processLinks (takeDirectory fp) orgdoc
                  att <- processAttachInDoc orgdoc'
                  pure $
                    over #docs (insert key orgdoc')
                      . over #files (Set.map FileRoute' files' <>)
                      . att
              Delete -> pure $ over #docs (delete key)

  siteOutput enc m =
    pure . \case
      Attch att ->
        OAsset $ pure $ renderAttachment att m
      File (FileRoute' fp) ->
        OAsset $ pure $ AssetStatic (m ^. #mount </> fp)
      Doc key ->
        OPage "content" $ \st doc ->
          renderDoc renderSettings st doc
            (resolveAttachLinks (routeUrl enc) $ (m ^. #docs) ! key)

type ContentRoute = Route
