{-# LANGUAGE UndecidableInstances #-}
-- |

module Site.Content (ContentRoute, Model (..), Options (..), ModelCache, cache0) where
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
import System.UnionMount.MoreFlexible
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
import Data.Binary (Binary)

data Model = Model
  { mount :: FilePath
  , serveAt :: FilePath
  , docs :: Map DocRoute OrgDocument
  , files :: Set FileRoute
  , attachments :: AttachModel
  , latexCache :: LaTeXCache
  , heistS :: HeistS
  }
  deriving (Generic)

data ModelCache = ModelCache
  { latexCache :: LaTeXCache
  }
  deriving (Generic, Binary)

cache0 :: ModelCache
cache0 = ModelCache mempty

model0 :: Model
model0 = Model "" mempty mempty mempty emptyAttachModel mempty Nothing

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
  type SiteArg Route = Options
  siteInput _ _ opt = Dynamic <$>
    mount_ source include exclude' model0
      (const handler)
    where
      source = mount (opt :: Options)
      include = [((), "**/*.org")]
      exclude' = exclude opt
      run :: Monad m => Model -> WriterT (KreisliEndo m Model) (ReaderT Options m) a -> m Model
      run m x = flip appKreisliEndo m =<< runReaderT (execWriterT x) opt
      handler fp =
        let place = Place fp source
            key = fromString $ dropExtension fp
        in \case
          Refresh _ () -> \m -> do
            threadDelay 100
            orgdoc <- parseOrgIO defaultOrgOptions (source </> fp)
            preamble' <- preambilizeKaTeX place orgdoc
            let orgdoc' = mapContent (first (preamble' :)) orgdoc
                (orgdoc'', Set.map FileRoute -> files') = processLinks (takeDirectory fp) orgdoc'
            run m $ do
              f <- lift (processLaTeX place orgdoc'')
              let texDoc  = fmap fst . f
                  texEndo = KreisliEndo $ fmap snd . f
              doc <- lift (lift (texDoc m))
              tell texEndo
              tell $ kreisliFromEndo (over (field @"docs") (insert key doc) . over (field @"files") (files' <>))
              attachEndo <- lift (processAttachInDoc doc)
              tell $ kreisliFromEndo (appEndo attachEndo)
          Delete -> pure . over (field @"docs") (delete key)
  siteOutput = heistOutput renderDoc

renderDoc :: Route -> RouteEncoder Model Route -> Model -> HeistState Exporter -> Asset LByteString
renderDoc (Doc key) enc m = renderAsset $
  callTemplate "ContentPage" $ documentSplices (resolveAttachLinks router $ docs (m :: Model) ! key)
  where router = routeUrl enc m
renderDoc (File (FileRoute fp)) _ m = const $ AssetStatic (mount (m :: Model) </> fp)
renderDoc (Attch att) _ m = renderAttachment att m

type ContentRoute = PrefixedRoute' Route
