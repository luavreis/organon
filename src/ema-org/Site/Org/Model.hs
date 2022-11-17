{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Site.Org.Model where

import Data.IxSet.Typed qualified as Ix
import Data.Map qualified as M
import Data.Set qualified as Set
import Data.Time
import Ema
import Ema.Route.Generic
import Ema.Route.Prism (htmlSuffixPrism)
import Generics.SOP qualified as SOP
import Optics.Core
import Org.Exporters.Processing.OrgData (ExporterSettings, OrgData)
import Org.Types (OrgDocument)
import Site.Org.JSON (FromJSON, ToJSON)
import System.FilePath (dropExtension, makeRelative, (</>))

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

type Pages = Ix.IxSet PostIxs OrgEntry

data Model = Model
  { _mPages :: Pages,
    _mSources :: Map Text FilePath
  }
  deriving (Generic)

pages :: Lens' Model Pages
pages = #_mPages

data OrgEntry = OrgEntry
  { -- | Entry identifier.
    _identifier :: Identifier,
    -- | Creation time
    _ctime :: Maybe UTCTime,
    -- | Modification times
    _mtime :: [UTCTime],
    -- | Entry tags.
    _tags :: [Text],
    -- | Entry layout
    _layout :: Text,
    -- | Document for entry.
    _document :: OrgDocument,
    -- | Export data for entry.
    _orgData :: OrgData,
    -- | Original level of entry, 0 means file-level.
    _level :: Int,
    -- | If entry has a parent, record it here.
    _parent :: Maybe Identifier,
    -- | Filepaths of the static files needed by the entry, relative to identifier path.
    _staticFiles :: Set (FilePath, UTCTime),
    -- -- | Internal locations
    -- _internalLocations :: Set InternalRef,
    -- | Org files which are referenced by this note.
    _linksTo :: Map UnresolvedLocation (NonEmpty (Maybe InternalRef))
  }
  deriving (Eq, Ord, Show, Typeable, Generic)

data Identifier = Identifier {_idPath :: OrgPath, _idId :: Maybe OrgID}
  deriving (Eq, Ord, Show, Typeable, Generic, ToJSON, FromJSON)

instance IsRoute Identifier where
  type RouteModel Identifier = Model
  routePrism (Model m sources) =
    toPrism_ $ htmlSuffixPrism % prism' to' from'
    where
      to' = \case
        Identifier _ (Just id_) -> toString id_
        Identifier (OrgPath s fp) _ -> toString s </> fp
      from' fp =
        let i = do
              let ix_ :: OrgID = fromString fp
              _identifier <$> Ix.getOne (m Ix.@= ix_)
            f = do
              ix_ :: OrgPath <- findUrlSource sources fp
              x <- _identifier <$> Ix.getOne (m Ix.@= LevelIx 0 Ix.@= ix_)
              guard (isNothing $ _idId x)
              pure x
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

pageStaticFiles :: OrgEntry -> Set StaticFileIx
pageStaticFiles page =
  let src = _opSource $ _idPath $ _identifier page
   in Set.map (StaticFileIx . OrgPath src . fst) $ _staticFiles page

newtype ParentID = ParentID OrgID
  deriving (Eq, Ord, Show, Typeable, Generic)

newtype ParentPath = ParentPath OrgPath
  deriving (Eq, Ord, Show, Typeable, Generic)

newtype CTimeIx = CTimeIx UTCTime
  deriving (Eq, Ord, Show, Typeable, Generic)

newtype MTimeIx = MTimeIx UTCTime
  deriving (Eq, Ord, Show, Typeable, Generic)

newtype StaticFileIx = StaticFileIx OrgPath
  deriving (Eq, Ord, Show, Typeable, Generic)

newtype LevelIx = LevelIx Int
  deriving (Eq, Ord, Show, Typeable, Generic)

newtype TagIx = TagIx Text
  deriving (Eq, Ord, Show, Typeable, Generic)

newtype LinksToIx = LinksToIx (Either OrgPath OrgID)
  deriving (Eq, Ord, Show, Typeable, Generic)

-- | For indexing a Org document filepath, without its extension.
data OrgPath = OrgPath {_opSource :: Text, _opPath :: FilePath}
  deriving stock (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (ToJSON, FromJSON)

prettyOrgPath :: OrgPath -> Text
prettyOrgPath path =
  "file '"
    <> toText (_opPath path)
    <> "' from source '"
    <> _opSource path
    <> "'"

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
  deriving stock (Eq, Ord, Show, Typeable, Generic)
  deriving newtype (IsString, ToString, ToText, ToJSON, FromJSON)

type PostIxs =
  '[ Identifier,
     OrgPath,
     OrgID,
     CTimeIx,
     MTimeIx,
     ParentPath,
     ParentID,
     LevelIx,
     TagIx,
     LinksToIx,
     StaticFileIx
   ]

instance Ix.Indexable PostIxs OrgEntry where
  indices =
    Ix.ixList
      (Ix.ixFun $ one . _identifier)
      (Ix.ixFun $ one . _idPath . _identifier)
      (Ix.ixFun $ maybeToList . _idId . _identifier)
      (Ix.ixFun $ maybeToList . coerce . _ctime)
      (Ix.ixFun $ coerce . _mtime)
      (Ix.ixFun $ maybeToList . fmap (coerce . _idPath) . _parent)
      (Ix.ixFun $ maybeToList . ((coerce . _idId) <=< _parent))
      (Ix.ixFun $ one . coerce . _level)
      (Ix.ixFun $ coerce . _tags)
      (Ix.ixFun $ coerce . M.keys . _linksTo)
      (Ix.ixFun $ toList . pageStaticFiles)

-- | This is like Identifier, but matches information present in links, where
-- there is either a path or an ID.
type UnresolvedLocation = Either OrgPath OrgID

data InternalRef
  = Anchor Text
  | LineRange Int Int
  deriving (Eq, Ord, Show, Typeable, Generic)

lookupBacklinks :: Pages -> Identifier -> [(OrgEntry, Maybe InternalRef)]
lookupBacklinks m (Identifier path id_) =
  let pPages =
        toList (m Ix.@= LinksToIx (Left path))
          >>= \p -> (p,) <$> toList (_linksTo p M.! Left path)
      iPages =
        flip (maybe []) id_ \i ->
          toList (m Ix.@= LinksToIx (Right i))
            >>= \p -> (p,) <$> toList (_linksTo p M.! Right i)
   in pPages ++ iPages

lookupOrgLocation :: Pages -> UnresolvedLocation -> Maybe OrgEntry
lookupOrgLocation m loc =
  case loc of
    Left path -> Ix.getOne (m Ix.@= LevelIx 0 Ix.@= path)
    Right id_ -> Ix.getOne (m Ix.@= id_)

model0 :: Model
model0 = Model mempty mempty

{- ORMOLU_DISABLE -}
instance NFData Model
instance NFData Identifier
instance NFData OrgPath
instance NFData OrgID
instance NFData CTimeIx
instance NFData MTimeIx
instance NFData ParentPath
instance NFData ParentID
instance NFData LevelIx
instance NFData TagIx
instance NFData LinksToIx
instance NFData StaticFileIx
instance NFData InternalRef
instance NFData OrgEntry

deriving instance Generic TimeLocale
instance NFData TimeLocale
instance NFData ExporterSettings
instance NFData OrgData
