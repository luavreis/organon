{-# LANGUAGE UndecidableInstances #-}
-- |

module Site.Content (ContentRoute, Model (..), Options (..)) where
import Place
import Prefix
import Ema
import Org.Types
import Ema.Route.Encoder
import Org.Exporters.Heist (Exporter, documentSplices)
import Heist.Interpreted
import Data.Generics.Product.Fields
import Data.Map ((!), delete, insert)
import Data.Set qualified as Set (map)
import Optics.Core
import System.FilePath ((</>), dropExtension, takeDirectory)
import System.UnionMount (FileAction (..))
import Render (HeistS, heistOutput, renderAsset)
import Org.Parser
import LaTeX
import Heist (HeistState)
import Site.Content.Options
import Generics.SOP qualified as SOP
import Ema.Route.GenericClass
import Routes qualified as R
import Site.Content.Links
import UnliftIO.Concurrent (threadDelay)
import OrgAttach (AttachModel, emptyAttachModel, renderAttachment, AttachRoute, resolveAttachLinks, processAttachInDoc)
import Control.Monad.Trans.Writer
import System.UnionMount qualified as UM
import Cache (Cache)

data Model = Model
  { mount :: FilePath
  , serveAt :: FilePath
  , docs :: Map DocRoute OrgDocument
  , files :: Set FileRoute
  , attachments :: AttachModel
  , heistS :: HeistS
  }
  deriving (Generic)

model0 :: Model
model0 = Model "" mempty mempty mempty emptyAttachModel Nothing

newtype DocRoute = DocRoute Text
  deriving (Eq, Ord, Show)
  deriving (R.StringRoute) via (R.HtmlRoute Text)
  deriving (IsRoute) via (R.MapRoute DocRoute OrgDocument)
  deriving newtype (IsString)

newtype FileRoute = FileRoute FilePath
  deriving (Eq, Ord, Show)
  deriving (R.StringRoute) via (R.FileRoute String)
  deriving (IsRoute) via (R.SetRoute FileRoute)

data ModelI = MI {docs :: Map DocRoute OrgDocument, files :: Set FileRoute, attachments :: AttachModel}
  deriving (Generic, SOP.Generic)

data Route
  = Doc DocRoute
  | File FileRoute
  | Attch AttachRoute
  deriving (Eq, Show, Generic, SOP.Generic)
  deriving (IsRoute) via (GenericRoute ModelI Model Route)

instance EmaSite Route where
  type SiteArg Route = (Options, TVar Cache)
  siteInput _ _ arg@(opt,_) = Dynamic <$>
    UM.mount source include exclude' model0 handler
    where
      source = opt ^. #mount
      include = [((), "**/*.org")]
      exclude' = exclude opt
      run :: Monad m => WriterT (Endo Model) (ReaderT (Options, TVar Cache) m) a -> m (Model -> Model)
      run x = appEndo <$> runReaderT (execWriterT x) arg
      handler () fp =
        let place = Place fp source
            key = fromString $ dropExtension fp
        in \case
          Refresh _ () -> do
            threadDelay 100
            run do
              orgdoc <- parseOrgIO defaultOrgOptions (source </> fp)
                        >>= lift . processLaTeX place
                        >>= lift . putKaTeXPreamble place
              let (orgdoc', Set.map FileRoute -> files') = processLinks (takeDirectory fp) orgdoc
              tell $ Endo (over (field @"docs") (insert key orgdoc') . over #files (files' <>))
              tell =<< lift (processAttachInDoc orgdoc')
          Delete -> pure $ over (field @"docs") (delete key)
  siteOutput = heistOutput renderDoc

renderDoc :: Route -> RouteEncoder Model Route -> Model -> HeistState Exporter -> Asset LByteString
renderDoc (Doc key) enc m = renderAsset $
  callTemplate "ContentPage" $ documentSplices (resolveAttachLinks router $ (m ^. #docs) ! key)
  where router = routeUrl enc m
renderDoc (File (FileRoute fp)) _ m = const $ AssetStatic (m ^. #mount </> fp)
renderDoc (Attch att) _ m = renderAttachment att m

type ContentRoute = PrefixedRoute' Route
