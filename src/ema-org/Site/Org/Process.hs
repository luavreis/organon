module Site.Org.Process where

import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Writer.CPS
import Data.List (intersect)
import Data.Map ((!))
import Optics.Core
import Org.Types
import Org.Walk (Walkable (walkM), walkListItemM)
import Relude.Extra (lookup, toPairs)
import Site.Org.Common (docTag, isolateSection, queryOrgInContext)
import Site.Org.Model
import Site.Org.Options qualified as O
import System.FilePath (isAbsolute, isRelative, makeRelative, splitDirectories, splitExtension, takeDirectory, (</>))
import UnliftIO (MonadUnliftIO)
import UnliftIO.Directory (canonicalizePath)
import Data.Time (UTCTime)

loadOrgFile ::
  forall m.
  (MonadUnliftIO m, MonadLogger m) =>
  HasCallStack =>
  O.Options ->
  -- | Source directories
  Map Text FilePath ->
  -- | Source filepath (relative to previous argument)
  OrgPath ->
  Set (FilePath, UTCTime) ->
  OrgDocument ->
  m [OrgData]
loadOrgFile opts sources orgPath staticFiles doc = do
  self <- getAp $ maybeToList <$> docToData 0 mempty mempty postWithTags
  rest <- getAp $ queryOrgInContext processSectionsWithId postWithTags
  pure $ self ++ rest
  where
    dir = sources ! _opSource orgPath

    findSource :: FilePath -> m (Maybe OrgPath)
    findSource fp =
      asum
        <$> forM (toPairs sources) \(srcKey, srcFp) -> do
          absSrc <- canonicalizePath srcFp
          let mbRel = makeRelative absSrc fp
          return
            if isRelative mbRel
              then Just (OrgPath srcKey mbRel)
              else Nothing

    relDir = takeDirectory (_opPath orgPath)

    fpTags =
      if relDir == "."
        then []
        else toText <$> splitDirectories relDir

    postWithTags = over docTag (fpTags <>) doc

    toIdentifier = Identifier orgPath . fmap OrgID

    -- Walks over the document but only maps the "zero-level" elements (no nesting)
    processLinks :: OrgDocument -> WriterT Backlinks m OrgDocument
    processLinks = mapContentM $ bitraverse f (walkM mapSection)
      where
        f = mapM processBlock
        mapSection s@OrgSection {sectionChildren = c} = do
          c' <- f c
          return s {sectionChildren = c'}

    processBlock :: OrgElement -> WriterT Backlinks m OrgElement

    processBlock (PlainList attrs ltype items) =
      fmap (PlainList attrs ltype) $
        flip evalStateT 0 $
          forM items \(ListItem bul con box tag els) -> do
            maybe (modify (+ 1)) put con
            con' <- gets Just
            let item = ListItem bul con' box tag els
                toBl = BacklinkData . PlainList attrs ltype . one
            lift $
              mapW toBl $
                walkListItemM processLink item
    processBlock (GreaterBlock a b els) =
      GreaterBlock a b <$> walkM processBlock els
    processBlock el =
      mapW BacklinkData $ walkM processLink el

    mapW f = mapWriterT $
      fmap $
        \ ~(x, p) -> (x, fromList $ zip p (repeat (f x)))

    processLink :: OrgObject -> WriterT [Either OrgPath OrgID] m OrgObject

    processLink l@(Link (URILink "id" uid) _) = tell [Right $ OrgID uid] >> pure l
    processLink l@(Link (URILink "file" (toString -> path)) c)
      | (path', ".org") <- splitExtension path = do
          trueFp <-
            canonicalizePath
              if isAbsolute path'
                then path'
                else (dir </> relDir </> path')
          lift (findSource trueFp) >>= \case
            Just oP -> do
              tell [Left oP]
              let uri = "source:" <> _opSource oP
                  target = toText $ _opPath oP
              return $ Link (URILink uri target) c
            Nothing -> pure l
    processLink x = pure x

    docToData :: Int -> Tags -> Properties -> OrgDocument -> (Ap m) (Maybe OrgData)
    docToData level inhTags inhProps docToAdd =
      let tags = docToAdd ^. docTag <> inhTags
          docID = lookupProperty "id" docToAdd
          ex = lookupProperty "roam_exclude" docToAdd
          shouldAdd =
            (level == 0 || isJust docID)
              && (isNothing ex || ex == Just "nil")
              && (null (O.privateTags opts `intersect` tags))
              && (null (O.publicTags opts) || not (null (O.publicTags opts `intersect` tags)))
       in if shouldAdd
            then do
              (doc', bklinks) <- Ap $ runWriterT $ processLinks docToAdd
              let identifier = toIdentifier docID
              pure $
                Just $
                  OrgData
                    { _identifier = identifier,
                      _tags = tags,
                      _document = doc' & #documentProperties %~ (<> inhProps),
                      _level = level,
                      _parent =
                        if level == 0
                          then Nothing
                          else Just (toIdentifier $ lookup "id" inhProps),
                      _staticFiles = staticFiles,
                      _linksTo = bklinks
                    }
            else pure Nothing

    processSectionsWithId :: Tags -> Properties -> OrgSection -> (Ap m) [OrgData]
    processSectionsWithId tags props section =
      let level = sectionLevel section
       in maybeToList <$> docToData level tags props (isolateSection section doc)
