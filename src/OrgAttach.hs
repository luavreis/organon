{-# LANGUAGE UndecidableInstances #-}
-- |

module OrgAttach where
import Org.Types
import Org.Walk
import UnliftIO.Directory (doesDirectoryExist)
import System.FilePath ((</>), splitDirectories)
import Relude.Extra (insert, lookup)
import Data.Set qualified as Set
import Data.Generics.Product
import Data.Generics.Sum
import Routes
import Ema (IsRoute, Asset (AssetStatic))
import Heist (HeistState)
import Org.Exporters.Heist (Exporter)
import Generics.SOP qualified as SOP
import Ema.Route.GenericClass (GenericRoute (..))
import Common (walkOrgInContext, queryOrgInContext)
import Optics.Operators

type OrgID = Text

data AttachOptions = AttachO { mount :: FilePath, orgAttachDir :: FilePath }
  deriving (Generic)

data AttachModel = AttachM { attachments :: Set AttachPath, attachDirs :: Map OrgID FilePath }
  deriving (Show, Generic)

newtype AttachModelI = MI { attachments :: Set AttachPath }
  deriving (Generic)
  deriving anyclass (SOP.Generic)

data AttachPath = AttachPath OrgID Text
  deriving (Eq, Ord, Show)
  deriving (IsRoute) via (SetRoute AttachPath)

instance StringRoute AttachPath where
  strPrism = (\(AttachPath rid txt) -> toString rid </> toString txt,
              \case
                (splitDirectories -> [fromString -> rid, fromString -> txt]) -> Just $ AttachPath rid txt
                _ -> Nothing)

newtype AttachRoute = AttachRoute AttachPath
  deriving (Eq, Show, Generic)
  deriving anyclass (SOP.Generic)
  deriving (IsRoute) via (GenericRoute AttachModelI AttachModel AttachRoute)

emptyAttachModel :: AttachModel
emptyAttachModel = AttachM mempty mempty

getAttachment :: OrgInline -> Set FilePath
getAttachment (Link (URILink "attachment" att) _) = one (toString att)
getAttachment (Image (URILink "attachment" att))  = one (toString att)
getAttachment _ = mempty

processAttachInDoc ::
  forall options model m _c.
  HasType AttachModel model =>
  Subtype AttachOptions options =>
  (MonadIO m, MonadReader (options, _c) m) =>
  OrgDocument ->
  m (Endo model)
processAttachInDoc doc = do
  attDir <- liftA2 (</>) (asks (mount . upcast . fst)) (asks (orgAttachDir . upcast . fst))
  docEndo <-
    case lookupProperty "id" doc of
      Just uid -> processAttachInSection @model attDir uid doc
      Nothing -> pure mempty
  sectionsEndo <-
    getAp $ flip queryOrgInContext doc $ \ _ _ section ->
      case lookupSectionProperty "id" section of
        Just uid -> Ap $ processAttachInSection attDir uid section
        Nothing -> pure mempty
  pure $ sectionsEndo <> docEndo

processAttachInSection ::
  forall model a m.
  Walkable OrgInline a =>
  HasType AttachModel model =>
  MonadIO m =>
  FilePath ->
  OrgID ->
  a ->
  m (Endo model)
processAttachInSection attDir key@(toString -> rid) obj = do

  let possibleDirs = map (attDir </>) [ orgAttachIdTSFolderFormat
                                      , orgAttachIdUUIDFolderFormat
                                      , rid ]

  findM doesDirectoryExist possibleDirs >>= \case
    Just dir ->
      pure $ Endo \m ->
        let m' = getTyped @AttachModel m
            m'' = m' & field' @"attachDirs" %~ insert key dir
                     & field' @"attachments" %~ Set.union atts
        in setTyped m'' m
    Nothing -> pure mempty
  where
    toAttach = AttachPath key . toText
    atts = Set.map toAttach $ query getAttachment obj

    orgAttachIdTSFolderFormat = take 6 rid </> drop 6 rid
    orgAttachIdUUIDFolderFormat = take 2 rid </> drop 2 rid

    findM p = foldr (\x -> ifM (p x) (pure $ Just x)) (pure Nothing)

renderAttachment ::
  HasType AttachModel model =>
  AttachRoute -> model -> HeistState Exporter -> Asset LByteString
renderAttachment (AttachRoute (AttachPath rid path)) m _hs =
  case lookup rid (attachDirs (getTyped m)) of
    Just dir -> AssetStatic (dir </> toString path)
    Nothing  -> error "This should not happen. Unknown org-attach dir."

resolveAttachLinks ::
  forall route.
  AsType AttachRoute route =>
  (route -> Text) -> OrgDocument -> OrgDocument
resolveAttachLinks route = walkOrgInContext resolve
  where
    resolve _ p = maybe id resolveLink (lookup "id" p)

    resolveLink :: OrgID -> OrgInline -> OrgInline
    resolveLink rid (Link (URILink "attachment" path) content) =
      Link (URILink "http" $ route (injectTyped $ AttachRoute (AttachPath rid path))) content
    resolveLink rid (Image (URILink "attachment" path)) =
      Image (URILink "http" $ route (injectTyped $ AttachRoute (AttachPath rid path)))
    resolveLink _ x = x
