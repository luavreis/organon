{-# LANGUAGE UndecidableInstances #-}

module Site.Org.Model where

import Data.IxSet.Typed qualified as Ix
import Data.Map qualified as M
import Data.Time
import Ema
import Ema.Route.Generic
import Ema.Route.Prism (htmlSuffixPrism)
import Generics.SOP qualified as SOP
import Optics.Core
import Org.Types (OrgDocument, OrgElement)
import Site.Org.JSON (FromJSON, ToJSON)
import System.FilePath (dropExtension, makeRelative, (</>))
import qualified Data.Set as Set

data Route
  = Route_Page Identifier
  | Route_Static StaticFileIx
  | Route_Graph
  deriving (Eq, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            Route
            '[ WithModel Model,
               WithSubRoutes
                 '[ Identifier,
                    StaticFileIx,
                    FileRoute "graph.json"
                  ]
             ]
        )

type Pages = Ix.IxSet PostIxs OrgData

data Model = Model
  { _mPages :: Pages,
    _mSources :: Map Text FilePath
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
    -- | Filepaths of the static files needed by the entry, relative to identifier path.
    _staticFiles :: (Set (FilePath, UTCTime)),
    -- | Org files which are linked by this note.
    _linksTo :: Backlinks
  }
  deriving (Eq, Ord, Show, Typeable, Generic)

data Identifier = Identifier {_idPath :: OrgPath, _idId :: (Maybe OrgID)}
  deriving (Eq, Ord, Show, Typeable, Generic, ToJSON, FromJSON)

instance IsRoute Identifier where
  type RouteModel Identifier = Model
  routePrism (Model m sources) =
    toPrism_ $ htmlSuffixPrism % prism' to' from'
    where
      toFp (OrgPath s fp) = toString s </> fp
      to' id_ = either toFp toString (idToEither id_)
      from' fp =
        let i = do
              let ix_ :: OrgID = fromString fp
              _identifier <$> Ix.getOne (m Ix.@= ix_)
            f = do
              ix_ :: OrgPath <- findUrlSource sources fp
              _identifier <$> Ix.getOne (m Ix.@= LevelIx 0 Ix.@= ix_)
         in i <|> f
  routeUniverse m = _identifier <$> toList (_mPages m)

instance IsRoute StaticFileIx where
  type RouteModel StaticFileIx = Model
  routePrism (Model m sources) =
    toPrism_ $ prism' toFp from'
    where
      toFp (coerce -> OrgPath s fp) = toString s </> fp
      from' fp = do
        ix_ <- StaticFileIx <$> findUrlSource sources fp
        guard $ not $ Ix.null (m Ix.@= ix_)
        return ix_
  routeUniverse m =
    toList $ Set.unions $ pageStaticFiles <$> toList (_mPages m)

pageStaticFiles :: OrgData -> Set StaticFileIx
pageStaticFiles page =
  let src = _opSource $ _idPath $ _identifier page
   in Set.map (StaticFileIx . OrgPath src . fst) $ _staticFiles page

newtype ParentID = ParentID OrgID
  deriving (Eq, Ord, Show, Typeable, Generic)

newtype ParentPath = ParentPath OrgPath
  deriving (Eq, Ord, Show, Typeable, Generic)

newtype StaticFileIx = StaticFileIx OrgPath
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
data OrgPath = OrgPath {_opSource :: Text, _opPath :: FilePath}
  deriving stock (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (ToJSON, FromJSON)

fromRawPath :: Text -> FilePath -> OrgPath
fromRawPath source = OrgPath source . dropExtension

findUrlSource :: Map Text FilePath -> FilePath -> Maybe OrgPath
findUrlSource sources fp =
  asum $
    M.toDescList sources <&> \(srcKey, _) -> do
      let mbRel = makeRelative (toString srcKey) fp
      guard (srcKey == "" || mbRel /= fp)
      return (OrgPath srcKey mbRel)

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
     BacklinkID,
     StaticFileIx
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
      (Ix.ixFun $ coerce . mapMaybe leftToMaybe . M.keys . _linksTo)
      (Ix.ixFun $ coerce . mapMaybe rightToMaybe . M.keys . _linksTo)
      (Ix.ixFun $ toList . pageStaticFiles)

type Backlinks = Map BacklinkKey BacklinkData

type BacklinkKey = Either OrgPath OrgID

data BacklinkData = BacklinkData
  { _blExcerpt :: OrgElement
  }
  deriving (Eq, Ord, Show, Typeable, Generic)

idToEither :: Identifier -> Either OrgPath OrgID
idToEither = \case
  (Identifier _ (Just i)) -> Right i
  (Identifier path _) -> Left path

lookupBacklinks :: Pages -> Identifier -> [(OrgData, BacklinkData)]
lookupBacklinks m (Identifier path id_) =
  let pPages =
        toList (m Ix.@= LevelIx 0 Ix.@= BacklinkPath path)
          <&> \p -> (p, _linksTo p M.! Left path)
      iPages =
        flip (maybe mempty) id_ \i ->
          toList (m Ix.@= BacklinkID i)
            <&> \p -> (p, _linksTo p M.! Right i)
   in pPages ++ iPages

lookupBacklink :: BacklinkKey -> Pages -> Maybe OrgData
lookupBacklink bl m =
  case bl of
    Left path -> Ix.getOne (m Ix.@= LevelIx 0 Ix.@= path)
    Right id_ -> Ix.getOne (m Ix.@= id_)

model0 :: Model
model0 = Model mempty mempty
