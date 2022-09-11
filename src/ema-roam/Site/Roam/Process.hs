module Site.Roam.Process where

import Common (docTag, isolateSection, queryOrgInContext)
import Control.Monad.Logger (MonadLogger)
import Data.List (intersect)
import Optics.Core
import Org.Types
import Org.Walk (Walkable (query))
import Relude.Extra (lookup)
import Site.Roam.Model
import Site.Roam.Options
import System.FilePath (isAbsolute, makeRelative, splitDirectories, takeDirectory, takeExtension, (</>))
import UnliftIO (MonadUnliftIO)
import UnliftIO.Directory (canonicalizePath)

loadOrgFile ::
  forall m.
  (MonadUnliftIO m, MonadLogger m) =>
  HasCallStack =>
  Options ->
  -- | Source directory
  FilePath ->
  -- | Source filepath (relative to previous argument)
  FilePath ->
  OrgDocument ->
  m [OrgData]
loadOrgFile opts dir fp doc = do
  self <- getAp $ maybeToList <$> docToData 0 mempty mempty postWithTags
  rest <- getAp $ queryOrgInContext processSectionsWithId postWithTags
  pure $ self ++ rest
  where
    relDir = takeDirectory fp

    fpTags =
      if relDir == "."
        then []
        else toText <$> splitDirectories relDir

    postWithTags = over docTag (fpTags <>) doc

    toIdentifier = Identifier (fromRawPath fp) . fmap OrgID

    -- Walks over the document but only maps the "zero-level" elements (no nesting)
    processLinks :: OrgDocument -> (Ap m) [Backlink]
    processLinks doc' = f (documentChildren doc') <> query mapSections doc'
      where
        f = foldMap processBlock
        mapSections OrgSection {sectionChildren = c} = f c

    processBlock :: OrgElement -> (Ap m) [Backlink]
    processBlock (PlainList attrs ltype items) =
      flip evalState 0 $
        flip foldMapM items \(ListItem bul con box tag els) -> do
          maybe (modify (+ 1)) put con
          con' <- gets Just
          pure $
            zipWith Backlink <$> query processLink els
              ?? (repeat $ one $ PlainList attrs ltype [ListItem bul con' box tag els])
    processBlock (GreaterBlock _ _ els) = query processBlock els
    processBlock blk = zipWith Backlink <$> query processLink blk ?? repeat [blk]

    processLink :: OrgObject -> (Ap m) [Either OrgPath OrgID]
    processLink (Link (URILink "id" uid) _) = pure [Right $ OrgID uid]
    processLink (Link (URILink "path" (toString -> path)) _)
      | takeExtension path == "org" = do
          trueFp <- Ap $ makeRelative dir
            <$> canonicalizePath
                  if isAbsolute path
                    then path
                    else (dir </> relDir </> path)
          pure [Left $ OrgPath trueFp]
    processLink _ = pure []

    docToData :: Int -> Tags -> Properties -> OrgDocument -> (Ap m) (Maybe OrgData)
    docToData level inhTags inhProps docToAdd =
      let parentId = lookup "id" inhProps
          tags = docToAdd ^. docTag <> inhTags
          docID = lookupProperty "id" docToAdd
          ex = lookupProperty "roam_exclude" docToAdd
          shouldAdd =
            (isNothing parentId || parentId /= docID)
              && (isNothing ex || ex == Just "nil")
              && (null (privateTags opts `intersect` tags))
              && (null (publicTags opts) || not (null (publicTags opts `intersect` tags)))
       in if shouldAdd
            then do
              bklinks <- processLinks docToAdd
              let identifier = toIdentifier docID
              pure $
                Just $
                  OrgData
                    { _identifier = identifier,
                      _tags = tags,
                      _document = docToAdd & #documentProperties %~ (<> inhProps),
                      _level = level,
                      _parent = if level == 0 then Nothing else Just (toIdentifier parentId),
                      _attachDir = Nothing,
                      _staticFiles = [],
                      _linksTo = bklinks
                    }
            else pure Nothing

    processSectionsWithId :: Tags -> Properties -> OrgSection -> (Ap m) [OrgData]
    processSectionsWithId tags props section =
      let level = sectionLevel section
       in maybeToList <$> docToData level tags props (isolateSection section doc)
