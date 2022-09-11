{-# LANGUAGE UndecidableInstances #-}

module Site.Org.Model where

import Data.IxSet.Typed qualified as Ix
import Ema
import Ema.Route.Generic
import Generics.SOP qualified as SOP
import Site.Org.JSON (FromJSON, ToJSON)
import Optics.Core
import Org.Types (OrgDocument, OrgElement)
import Site.Org.OrgAttach (AttachModel, AttachRoute, emptyAttachModel)
import Text.XmlHtml qualified as X
import Ema.Route.Prism (htmlSuffixPrism)
import System.FilePath (dropExtension)

data Route
  = Route_Page Identifier
  | Route_Attach AttachRoute
  | Route_Graph
  deriving (Eq, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            Route
            '[ WithModel Model,
               WithSubRoutes
                 '[ Identifier,
                    FolderRoute "attach" AttachRoute,
                    FileRoute "graph.json"
                  ]
             ]
        )

type Pages = Ix.IxSet PostIxs OrgData

data Model = Model
  { _mPages :: Pages,
    _mFileAssoc :: Map FilePath (Set OrgID),
    _mAttachments :: AttachModel,
    _mLayouts :: Map Text X.Document
  }
  deriving (Generic)

pages :: Lens' Model Pages
pages = #_mPages

data OrgData = OrgData
  { -- | Entry identifier.
    _identifier :: Identifier,
    -- | Entry tags.
    _tags :: [Text],
    -- | Document for entry.
    _document :: OrgDocument,
    -- | Original level of entry, 0 means file-level.
    _level :: Int,
    -- | If entry has a parent, record it here.
    _parent :: Maybe Identifier,
    -- | Org-attach directory for this entry.
    _attachDir :: Maybe FilePath,
    -- | Filepaths of the static files needed by the entry, relative to identifier path.
    _staticFiles :: [FilePath],
    -- | Org files which are linked by this note.
    _linksTo :: [Backlink]
  }
  deriving (Eq, Ord, Show, Typeable, Generic)

data Identifier = Identifier {_idPath :: OrgPath, _idId :: (Maybe OrgID)}
  deriving (Eq, Ord, Show, Typeable, Generic, ToJSON, FromJSON)

instance IsRoute Identifier where
  type RouteModel Identifier = Pages
  routePrism m =
    toPrism_ $ htmlSuffixPrism % prism' to' from'
    where
      to' id_ = either toString toString (idToEither id_)
      from' fp =
        let i = do
              let ix_ :: OrgID = fromString fp
              _identifier <$> Ix.getOne (m Ix.@= ix_)
            f = do
              let ix_ :: OrgPath = fromString fp
              _identifier <$> Ix.getOne (m Ix.@= ix_)
         in i <|> f
  routeUniverse m = _identifier <$> toList m

newtype FileSource = FileSource FilePath
  deriving (Eq, Ord, Show, Typeable, Generic)

newtype ParentID = ParentID OrgID
  deriving (Eq, Ord, Show, Typeable, Generic)

newtype ParentPath = ParentPath OrgPath
  deriving (Eq, Ord, Show, Typeable, Generic)

newtype LevelIx = LevelIx Int
  deriving (Eq, Ord, Show, Typeable, Generic)

newtype TagIx = TagIx Text
  deriving (Eq, Ord, Show, Typeable, Generic)

newtype BacklinkPath = BacklinkPath OrgPath
  deriving (Eq, Ord, Show, Typeable, Generic)

newtype BacklinkID = BacklinkID OrgID
  deriving (Eq, Ord, Show, Typeable, Generic)

-- | For indexing a Org document filepath, without its extension.
newtype OrgPath = OrgPath FilePath
  deriving stock (Eq, Ord, Show, Typeable, Generic)
  deriving newtype (IsString, ToString, ToText, ToJSON, FromJSON)

fromRawPath :: FilePath -> OrgPath
fromRawPath = OrgPath . dropExtension

newtype OrgID = OrgID {getID :: Text}
  deriving stock (Eq, Ord, Show, Typeable)
  deriving newtype (IsString, ToString, ToText, ToJSON, FromJSON)

type PostIxs =
  '[ Identifier,
     OrgPath,
     OrgID,
     ParentPath,
     ParentID,
     LevelIx,
     TagIx,
     BacklinkPath,
     BacklinkID
   ]

instance Ix.Indexable PostIxs OrgData where
  indices =
    Ix.ixList
      (Ix.ixFun $ one . _identifier)
      (Ix.ixFun $ one . _idPath . _identifier)
      (Ix.ixFun $ maybeToList . _idId . _identifier)
      (Ix.ixFun $ maybeToList . fmap (coerce . _idPath) . _parent)
      (Ix.ixFun $ maybeToList . (coerce . _idId =<<) . _parent)
      (Ix.ixFun $ one . coerce . _level)
      (Ix.ixFun $ coerce . _tags)
      (Ix.ixFun $ coerce . mapMaybe (leftToMaybe . _blTarget) . _linksTo)
      (Ix.ixFun $ coerce . mapMaybe (rightToMaybe . _blTarget) . _linksTo)

data Backlink = Backlink
  { _blTarget :: Either OrgPath OrgID,
    _blExcerpt :: [OrgElement]
  }
  deriving (Eq, Ord, Show, Typeable, Generic)

idToEither :: Identifier -> Either OrgPath OrgID
idToEither = \case
  (Identifier _ (Just i)) -> Right i
  (Identifier path _) -> Left path

lookupBacklink :: Backlink -> Pages -> Maybe OrgData
lookupBacklink bl m =
  case _blTarget bl of
    Left path -> Ix.getOne (m Ix.@= BacklinkPath path)
    Right id_ -> Ix.getOne (m Ix.@= BacklinkID id_)

model0 :: Model
model0 = Model mempty mempty emptyAttachModel mempty
