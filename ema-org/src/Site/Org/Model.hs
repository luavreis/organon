{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Site.Org.Model where

import Data.IxSet.Typed qualified as Ix
import Data.Map qualified as M
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NES
import Data.Time
import Ema
import Ema.Route.Generic
import Ema.Route.Prism (htmlSuffixPrism)
import Generics.SOP qualified as SOP
import Optics.Core
import Org.Exporters.Processing.OrgData (OrgData)
import Org.Types (OrgDocument)
import Site.Org.Meta.Types (MetaMap)
import Site.Org.Options (Options (..), Source (..))
import Site.Org.Utils.JSON (FromJSON, ToJSON)
import System.FilePath (dropExtension, isRelative, makeRelative, normalise, (</>))
import UnliftIO.Directory (canonicalizePath)

data Route
  = Route_Page Identifier
  | Route_Static StaticFileIx
  | Route_Graph
  deriving (Eq, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            Route
            '[ WithModel Model
             , WithSubRoutes
                '[ Identifier
                 , StaticFileIx
                 , FileRoute "graph.json"
                 ]
             ]
        )

type Pages = Ix.IxSet PostIxs OrgEntry

data Model = Model
  { _mPages :: Pages
  , _mOptions :: Options
  }
  deriving (Generic)

pages :: Lens' Model Pages
pages = #_mPages

data OrgEntry = OrgEntry
  { _identifier :: Identifier
  -- ^ Entry identifier.
  , _ctime :: Maybe UTCTime
  -- ^ Creation time
  , _mtime :: Maybe UTCTime
  -- ^ Modification times
  , _tags :: [Text]
  -- ^ Entry tags.
  , _layout :: Text
  -- ^ Entry layout
  , _document :: OrgDocument
  -- ^ Document for entry.
  , _orgData :: OrgData
  -- ^ Export data for entry.
  , _meta :: MetaMap
  -- ^ Vulpea-style metadata
  , _level :: Int
  -- ^ Original level of entry, 0 means file-level.
  , _parent :: Maybe Identifier
  -- ^ If entry has a parent, record it here.
  , _staticFiles :: Set (OrgPath, UTCTime)
  -- ^ Filepaths of the static files needed by the entry, relative to identifier path.
  , _linksTo :: Map UnresolvedLocation (NES.NESet (Maybe InternalRef))
  -- ^ Org files which are referenced by this note.
  }
  deriving (Eq, Ord, Show, Typeable, Generic)

data Identifier = Identifier {_idPath :: OrgPath, _idId :: Maybe OrgID}
  deriving (Eq, Ord, Show, Typeable, Generic, ToJSON, FromJSON)

instance IsRoute Identifier where
  type RouteModel Identifier = Model
  routePrism (Model m o) =
    toPrism_ $ htmlSuffixPrism % prism' to' from'
    where
      to' = \case
        Identifier _ (Just id_) -> toString id_
        Identifier (OrgPath s fp) _ -> toString (serveAt s) </> fp
      from' fp =
        let idRoute = do
              let ix_ :: OrgID = fromString fp
              _identifier <$> Ix.getOne (m Ix.@= ix_)
            fpRoute = do
              ix_ :: OrgPath <- findUrlSource (mount o) fp
              x <- _identifier <$> Ix.getOne (m Ix.@= LevelIx 0 Ix.@= ix_)
              guard (isNothing $ _idId x)
              pure x
         in idRoute <|> fpRoute
  routeUniverse m = _identifier <$> toList (_mPages m)

instance IsRoute StaticFileIx where
  type RouteModel StaticFileIx = Model
  routePrism (Model m o) =
    toPrism_ $ prism' toFp from'
    where
      toFp (coerce -> OrgPath s fp) = toString (serveAt s) </> fp
      from' fp = do
        ix_ <- StaticFileIx <$> findUrlSource (mount o) fp
        guard $ not $ Ix.null (m Ix.@= ix_)
        return ix_
  routeUniverse m =
    toList $ Set.unions $ pageStaticFiles <$> toList (_mPages m)

pageStaticFiles :: OrgEntry -> Set StaticFileIx
pageStaticFiles page = Set.map (StaticFileIx . fst) $ _staticFiles page

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

newtype SourceIx = SourceIx Text
  deriving (Eq, Ord, Show, Typeable, Generic)

-- | For indexing a Org document filepath, without its extension.
data OrgPath = OrgPath {opSource :: Source, opPath :: FilePath}
  deriving stock (Eq, Ord, Show, Read, Typeable, Generic)
  deriving anyclass (ToJSON, FromJSON)

prettyOrgPath :: OrgPath -> Text
prettyOrgPath path =
  "file '"
    <> toText (opPath path)
    <> "' from source '"
    <> alias (opSource path)
    <> "'"

toRawPath :: OrgPath -> FilePath
toRawPath (OrgPath src fp) = dir src </> fp

fromRawPath :: Source -> FilePath -> OrgPath
fromRawPath source = OrgPath source . dropExtension

findUrlSource :: [Source] -> FilePath -> Maybe OrgPath
findUrlSource sources fp =
  asum $
    sortBy descLength sources <&> \source -> do
      let mbRel = makeRelative (toString $ serveAt source) fp
      guard (serveAt source == "" || mbRel /= fp)
      return (OrgPath source mbRel)
  where
    descLength = flip $ comparing serveAt

findSource :: (MonadIO m) => [Source] -> FilePath -> m (Maybe OrgPath)
findSource sources trueFp = do
  asum <$> forM sources \source -> do
    absSrc <- canonicalizePath (dir source)
    let mbRel = normalise $ makeRelative absSrc trueFp
    return $
      if isRelative mbRel
        then Just $ OrgPath source mbRel
        else Nothing

newtype OrgID = OrgID {getID :: Text}
  deriving stock (Eq, Ord, Show, Typeable, Generic)
  deriving newtype (IsString, ToString, ToText, ToJSON, FromJSON)

type PostIxs =
  '[ Identifier
   , OrgPath
   , SourceIx
   , OrgID
   , CTimeIx
   , MTimeIx
   , ParentPath
   , ParentID
   , LevelIx
   , TagIx
   , LinksToIx
   , StaticFileIx
   ]

instance Ix.Indexable PostIxs OrgEntry where
  indices =
    Ix.ixList
      (Ix.ixFun $ one . _identifier)
      (Ix.ixFun $ one . _idPath . _identifier)
      (Ix.ixFun $ one . coerce . serveAt . opSource . _idPath . _identifier)
      (Ix.ixFun $ maybeToList . _idId . _identifier)
      (Ix.ixFun $ maybeToList . coerce . _ctime)
      (Ix.ixFun $ maybeToList . coerce . _mtime)
      (Ix.ixFun $ maybeToList . fmap (coerce . _idPath) . _parent)
      (Ix.ixFun $ maybeToList . ((coerce . _idId) <=< _parent))
      (Ix.ixFun $ one . coerce . _level)
      (Ix.ixFun $ coerce . _tags)
      (Ix.ixFun $ coerce . M.keys . _linksTo)
      (Ix.ixFun $ toList . pageStaticFiles)

{- | This is like Identifier, but matches information present in links, where
 there is either a path or an ID.
-}
type UnresolvedLocation = Either OrgPath OrgID

data InternalRef
  = Anchor Text
  | MetaProperty
  | LineRange Int Int
  deriving (Eq, Ord, Show, Typeable, Generic)

linksToIdentifier :: Identifier -> [LinksToIx]
linksToIdentifier (Identifier path id_) =
  LinksToIx <$> Left path : maybeToList (Right <$> id_)

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

{- ORMOLU_DISABLE -}
instance NFData Model
instance NFData Identifier
instance NFData OrgPath
instance NFData SourceIx
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
instance NFData OrgData
