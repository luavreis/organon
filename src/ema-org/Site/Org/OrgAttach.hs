{-# LANGUAGE UndecidableInstances #-}
module Site.Org.OrgAttach where

import Site.Org.Common (queryOrgInContext, walkOrgInContext)
import Data.Generics.Product
import Data.Generics.Sum
import Data.Set qualified as Set
import Ema (Asset (AssetStatic), IsRoute)
import Ema.Route.Generic
import Generics.SOP qualified as SOP
import Optics.Operators
import Org.Types
import Org.Walk
import Relude.Extra (insert, lookup)
import Site.Org.Routes
import System.FilePath (splitDirectories, (</>))
import UnliftIO.Directory (doesDirectoryExist)

type OrgID = Text

data AttachOptions = AttachO {mount :: FilePath, orgAttachDir :: FilePath}
  deriving (Generic)

data AttachModel = AttachM {attachments :: Set AttachPath, attachDirs :: Map OrgID FilePath}
  deriving (Show, Generic)

data AttachPath = AttachPath OrgID Text
  deriving (Eq, Ord, Show, Generic)
  deriving (IsRoute) via (SetRoute AttachPath)

instance StringRoute AttachPath where
  strRoutePrism' =
    ( \(AttachPath rid txt) -> toString rid </> toString txt,
      \case
        (splitDirectories -> [fromString -> rid, fromString -> txt]) -> Just $ AttachPath rid txt
        _ -> Nothing
    )

newtype AttachRoute = AttachRoute AttachPath
  deriving stock (Eq, Show, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            AttachRoute
            '[ WithModel AttachModel,
               WithSubRoutes '[AttachPath]
             ]
        )

emptyAttachModel :: AttachModel
emptyAttachModel = AttachM mempty mempty

getAttachment :: OrgObject -> Set FilePath
getAttachment (Link (URILink "attachment" att) _) = one (toString att)
getAttachment (Image (URILink "attachment" att)) = one (toString att)
getAttachment _ = mempty

processAttachInDoc ::
  forall options model m _c.
  HasType AttachModel model =>
  Subtype AttachOptions options =>
  (MonadIO m, MonadReader (options, _c) m) =>
  OrgDocument ->
  m (model -> model)
processAttachInDoc doc = do
  attDir <- liftA2 (</>) (asks (mount . upcast . fst)) (asks (orgAttachDir . upcast . fst))
  docEndo <-
    case lookupProperty "id" doc of
      Just uid -> processAttachInSection @model attDir uid doc
      Nothing -> pure mempty
  sectionsEndo <-
    getAp $
      flip queryOrgInContext doc $ \_ _ section ->
        case lookupSectionProperty "id" section of
          Just uid -> Ap $ processAttachInSection attDir uid section
          Nothing -> pure mempty
  pure . appEndo $ sectionsEndo <> docEndo

processAttachInSection ::
  forall model a m.
  Walkable OrgObject a =>
  HasType AttachModel model =>
  MonadIO m =>
  FilePath ->
  OrgID ->
  a ->
  m (Endo model)
processAttachInSection attDir key@(toString -> rid) obj = do
  let possibleDirs =
        map
          (attDir </>)
          [ orgAttachIdTSFolderFormat,
            orgAttachIdUUIDFolderFormat,
            rid
          ]

  findM doesDirectoryExist possibleDirs >>= \case
    Just dir ->
      pure $ Endo \m ->
        let m' = getTyped @AttachModel m
            m'' =
              m' & field' @"attachDirs" %~ insert key dir
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
  AttachRoute ->
  model ->
  Asset LByteString
renderAttachment (AttachRoute (AttachPath rid path)) m =
  case lookup rid (attachDirs (getTyped m)) of
    Just dir -> AssetStatic (dir </> toString path)
    Nothing -> error "This should not happen. Unknown org-attach dir."

resolveAttachLinks ::
  forall route.
  AsType AttachRoute route =>
  (route -> Text) ->
  OrgDocument ->
  OrgDocument
resolveAttachLinks route = walkOrgInContext resolve
  where
    resolve _ p = maybe id resolveLink (lookup "id" p)

    resolveLink :: OrgID -> OrgObject -> OrgObject
    resolveLink rid (Link (URILink "attachment" path) content) =
      Link (URILink "file" $ route (injectTyped $ AttachRoute (AttachPath rid path))) content
    resolveLink rid (Image (URILink "attachment" path)) =
      Image (URILink "file" $ route (injectTyped $ AttachRoute (AttachPath rid path)))
    resolveLink _ x = x
