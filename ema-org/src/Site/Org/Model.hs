module Site.Org.Model where

import Data.IxSet.Typed qualified as Ix
import Data.Map qualified as M
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NES
import Data.Time
import Ema
import Ema.Route.Prism (htmlSuffixPrism)
import Optics.Core
import Org.Exporters.Processing.OrgData (OrgData (..))
import Org.Types (OrgDocument)
import Site.Org.Meta.Types (MetaMap)
import Site.Org.Options (Options (..), Source (..))
import Site.Org.Utils.JSON (FromJSON, ToJSON)
import System.FilePath (dropExtension, isRelative, makeRelative, normalise, (</>))
import UnliftIO.Directory (canonicalizePath)

type Pages = Ix.IxSet EntryIxs OrgEntry

data Model = Model
  { pages :: Pages
  , options :: Options
  }
  deriving (Generic)

data OrgEntry = OrgEntry
  { identifier :: Identifier
  -- ^ Entry identifier.
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

newtype PathRouteIx = PathRouteIx String
  deriving (Eq, Ord, Show, Typeable, Generic)

routeFromPath :: OrgPath -> String
routeFromPath path = toString path.source.serveAt </> dropExtension path.relpath

instance IsRoute Identifier where
  type RouteModel Identifier = Model
  routePrism (Model m _) =
    toPrism_ $ htmlSuffixPrism % prism' to' from'
    where
      to' = \case
        Identifier _ (Just orgId) -> toString orgId
        Identifier p _ -> routeFromPath p
      from' fp =
        let idRoute = do
              let ix_ :: OrgID = fromString fp
              (.identifier) <$> Ix.getOne (m Ix.@= ix_)
            fpRoute = do
              let ix_ = PathRouteIx fp
              x <- (.identifier) <$> Ix.getOne (m Ix.@= LevelIx 0 Ix.@= ix_)
              guard (isNothing x.orgId)
              pure x
         in idRoute <|> fpRoute
  routeUniverse m = (.identifier) <$> toList m.pages

newtype StaticFile = StaticFile OrgPath
  deriving (Eq, Ord, Show, Typeable, Generic)

routeFromStaticFile :: OrgPath -> String
routeFromStaticFile path = toString path.source.serveAt </> path.relpath

instance IsRoute StaticFile where
  type RouteModel StaticFile = Model
  routePrism m =
    toPrism_ $ prism' to' from'
    where
      to' = routeFromStaticFile . coerce
      from' fp = find ((fp ==) . routeFromStaticFile . coerce) (routeUniverse m)
  routeUniverse m =
    toList $ Set.unions $ pageStaticFiles <$> toList m.pages

pageStaticFiles :: OrgEntry -> Set StaticFile
pageStaticFiles page = Set.map (StaticFile . fst) page.staticFiles

newtype ParentID = ParentID OrgID
  deriving (Eq, Ord, Show, Typeable, Generic)

newtype ParentPath = ParentPath OrgPath
  deriving (Eq, Ord, Show, Typeable, Generic)

newtype LevelIx = LevelIx Int
  deriving (Eq, Ord, Show, Typeable, Generic)

newtype TagIx = TagIx Text
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

toFilePath :: OrgPath -> FilePath
toFilePath (OrgPath src fp) = src.dir </> fp

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

type EntryIxs =
  '[ Identifier
   , OrgPath
   , PathRouteIx
   , SourceIx
   , OrgID
   , ParentPath
   , ParentID
   , LevelIx
   , TagIx
   , LinksToIx
   ]

instance Ix.Indexable EntryIxs OrgEntry where
  indices =
    Ix.ixList
      (Ix.ixFun $ \x -> [x.identifier])
      (Ix.ixFun $ \x -> [x.identifier.path])
      (Ix.ixFun $ \x -> [coerce $ routeFromPath x.identifier.path])
      (Ix.ixFun $ \x -> [coerce x.identifier.path.source.serveAt])
      (Ix.ixFun $ \x -> maybeToList x.identifier.orgId)
      (Ix.ixFun $ \x -> maybeToList $ coerce $ (.path) <$> x.parent)
      (Ix.ixFun $ \x -> maybeToList $ coerce $ (.orgId) =<< x.parent)
      (Ix.ixFun $ \x -> [coerce x.level])
      (Ix.ixFun $ \x -> coerce x.orgData.filetags)
      (Ix.ixFun $ \x -> coerce $ M.keys x.linksTo)

{- | This is like Identifier, but matches information present in links, where
 there is either a path or an ID.
-}
type UnresolvedLocation = Either OrgPath OrgID

newtype LinksToIx = LinksToIx UnresolvedLocation
  deriving (Eq, Ord, Show, Typeable, Generic)

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
instance NFData ParentPath
instance NFData PathRouteIx
instance NFData ParentID
instance NFData LevelIx
instance NFData TagIx
instance NFData LinksToIx
instance NFData InternalRef
instance NFData OrgEntry
