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
  { pages :: Pages
  , options :: Options
  }
  deriving (Generic)

data OrgEntry = OrgEntry
  { identifier :: Identifier
  -- ^ Entry identifier.
  , ctime :: Maybe UTCTime
  -- ^ Creation time
  , mtime :: Maybe UTCTime
  -- ^ Modification times
  , tags :: [Text]
  -- ^ Entry tags.
  , layout :: Text
  -- ^ Entry layout
  , document :: OrgDocument
  -- ^ Document for entry.
  , orgData :: OrgData
  -- ^ Export data for entry.
  , meta :: MetaMap
  -- ^ Vulpea-style metadata
  , level :: Int
  -- ^ Original level of entry, 0 means file-level.
  , parent :: Maybe Identifier
  -- ^ If entry has a parent, record it here.
  , staticFiles :: Set (OrgPath, UTCTime)
  -- ^ Filepaths of the static files needed by the entry, relative to identifier path.
  , linksTo :: Map UnresolvedLocation (NES.NESet (Maybe InternalRef))
  -- ^ Org files which are referenced by this note.
  }
  deriving (Eq, Ord, Show, Typeable, Generic)

data Identifier = Identifier {path :: OrgPath, orgId :: Maybe OrgID}
  deriving (Eq, Ord, Show, Typeable, Generic, ToJSON, FromJSON)

instance IsRoute Identifier where
  type RouteModel Identifier = Model
  routePrism (Model m o) =
    toPrism_ $ htmlSuffixPrism % prism' to' from'
    where
      to' = \case
        Identifier _ (Just id_) -> toString id_
        Identifier (OrgPath s fp) _ -> toString s.serveAt </> fp
      from' fp =
        let idRoute = do
              let ix_ :: OrgID = fromString fp
              (.identifier) <$> Ix.getOne (m Ix.@= ix_)
            fpRoute = do
              ix_ :: OrgPath <- findUrlSource o.mount fp
              x <- (.identifier) <$> Ix.getOne (m Ix.@= LevelIx 0 Ix.@= ix_)
              guard (isNothing x.orgId)
              pure x
         in idRoute <|> fpRoute
  routeUniverse m = (.identifier) <$> toList m.pages

instance IsRoute StaticFileIx where
  type RouteModel StaticFileIx = Model
  routePrism (Model m o) =
    toPrism_ $ prism' toFp from'
    where
      toFp (coerce -> OrgPath s fp) = toString s.serveAt </> fp
      from' fp = do
        ix_ <- StaticFileIx <$> findUrlSource o.mount fp
        guard $ not $ Ix.null (m Ix.@= ix_)
        return ix_
  routeUniverse m =
    toList $ Set.unions $ pageStaticFiles <$> toList m.pages

pageStaticFiles :: OrgEntry -> Set StaticFileIx
pageStaticFiles page = Set.map (StaticFileIx . fst) page.staticFiles

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
data OrgPath = OrgPath {source :: Source, relpath :: FilePath}
  deriving stock (Eq, Ord, Show, Read, Typeable, Generic)
  deriving anyclass (ToJSON, FromJSON)

prettyOrgPath :: OrgPath -> Text
prettyOrgPath path =
  "file '"
    <> toText path.relpath
    <> "' from source '"
    <> path.source.alias
    <> "'"

toRawPath :: OrgPath -> FilePath
toRawPath (OrgPath src fp) = src.dir </> fp

fromRawPath :: Source -> FilePath -> OrgPath
fromRawPath source = OrgPath source . dropExtension

findUrlSource :: [Source] -> FilePath -> Maybe OrgPath
findUrlSource sources fp =
  asum $
    sortBy descLength sources <&> \source -> do
      let mbRel = makeRelative (toString source.serveAt) fp
      guard (source.serveAt == "" || mbRel /= fp)
      return (OrgPath source mbRel)
  where
    descLength = flip $ comparing (.serveAt)

findSource :: (MonadIO m) => [Source] -> FilePath -> m (Maybe OrgPath)
findSource sources trueFp = do
  asum <$> forM sources \source -> do
    absSrc <- canonicalizePath source.dir
    let mbRel = normalise $ makeRelative absSrc trueFp
    return $
      if isRelative mbRel
        then Just $ OrgPath source mbRel
        else Nothing

newtype OrgID = OrgID {idText :: Text}
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
      (Ix.ixFun $ \x -> [x.identifier])
      (Ix.ixFun $ \x -> [x.identifier.path])
      (Ix.ixFun $ \x -> [coerce x.identifier.path.source.serveAt])
      (Ix.ixFun $ \x -> maybeToList x.identifier.orgId)
      (Ix.ixFun $ \x -> maybeToList $ coerce x.ctime)
      (Ix.ixFun $ \x -> maybeToList $ coerce x.mtime)
      (Ix.ixFun $ \x -> maybeToList $ coerce $ (.path) <$> x.parent)
      (Ix.ixFun $ \x -> maybeToList $ coerce $ (.orgId) =<< x.parent)
      (Ix.ixFun $ \x -> [coerce x.level])
      (Ix.ixFun $ \x -> coerce x.tags)
      (Ix.ixFun $ \x -> coerce $ M.keys x.linksTo)
      (Ix.ixFun $ \x -> toList $ pageStaticFiles x)

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
          >>= \p -> (p,) <$> toList (p.linksTo M.! Left path)
      iPages =
        flip (maybe []) id_ \i ->
          toList (m Ix.@= LinksToIx (Right i))
            >>= \p -> (p,) <$> toList (p.linksTo M.! Right i)
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
