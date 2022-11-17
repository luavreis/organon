{-# LANGUAGE RankNTypes #-}

module Site.Org.Process where

import Control.Monad.Logger (MonadLogger, logWarnNS)
import Control.Monad.Trans.Writer.CPS
import Data.Map ((!))
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Optics.Core
import Org.Exporters.Processing.OrgData (OrgData (filetags))
import Org.Types
import Org.Walk
import Relude.Extra (lookup, toPairs)
import Site.Org.Common (isolateSection, queryOrgInContext)
import Site.Org.Model
import Site.Org.Options qualified as O
import Site.Org.Utils.MonoidalMap
import System.FilePath (isAbsolute, isRelative, makeRelative, normalise, splitDirectories, splitExtension, takeDirectory, (</>))
import UnliftIO (MonadUnliftIO)
import UnliftIO.Directory (canonicalizePath)
import Org.Exporters.Processing (continuePipeline, resolveLinks)

type Backlinks = MonoidalMap UnresolvedLocation (NonEmpty (Maybe InternalRef))

type ProcessM m = WriterT Backlinks (StateT Int m)

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
  (OrgDocument, OrgData) ->
  m [OrgEntry]
loadOrgFile opts sources orgPath staticFiles doc = do
  self <- getAp $ maybeToList <$> docToEntry 0 mempty mempty postWithTags
  rest <- getAp $ queryOrgInContext processSectionsWithId postWithTags
  pure $ self ++ rest
  where
    dir = sources ! _opSource orgPath

    findSource :: FilePath -> m (Maybe OrgPath)
    findSource fp
      -- If path already is relative, we use the document source.
      | isRelative fp = return $ Just orgPath {_opPath = normalise fp}
      -- Otherwise, attempt to find another source.
      | otherwise =
          asum <$> forM (toPairs sources) \(srcKey, srcFp) -> do
            absSrc <- canonicalizePath srcFp
            let mbRel = normalise $ makeRelative absSrc fp
            return
              if isRelative mbRel
                then Just (OrgPath srcKey mbRel)
                else Nothing

    relDir = takeDirectory (_opPath orgPath)

    fpTags =
      if relDir == "."
        then []
        else toText <$> splitDirectories relDir

    postWithTags = over (_2 % #filetags) (fpTags <>) doc

    -- Walks over the document but only maps the "zero-level" elements (no nesting)
    processLinks :: OrgDocument -> ProcessM m OrgDocument
    processLinks = buildMultiW \f l ->
      l
        .> processBlock f
        .> processLink f

    -- processLink first writes backlinks with null internal references to the
    -- writer state. Then, if processBlock finds any of those backlinks with
    -- null references in its children, it replaces those null references with a
    -- new anchor and creates a div around the block to receive that anchor.

    -- This should be tought of as a stack of pending references, that don't
    -- have a block yet. Then, the closest block in the upward tree picks up
    -- those null references and replace them with a reference to itself.

    processBlock :: WalkM (ProcessM m) -> OrgElement -> ProcessM m OrgElement
    processBlock f elm = do
      (elm', blks) <- lift $ runWriterT $ f elm
      if any null blks
        then do
          n <- lift $ get <* modify (+ 1)
          let anchor = "bltarget" <> show n
          let blks' = (<|> Just (Anchor anchor)) <<$>> blks
          tell blks'
          return $ GreaterBlock (Map.singleton "portal-target" $ ValueKeyword anchor) (Special "portal") [elm']
        else tell blks >> pure elm'

    -- \| Process links to other Org files and register the backlinks.
    processLink :: WalkM (ProcessM m) -> OrgObject -> ProcessM m OrgObject
    processLink f l = case l of
      -- ID link to another Org file
      (Link (URILink "id" uid) _) -> do
        tell (MonoidalMap $ Map.singleton (Right (OrgID uid)) (one Nothing))
        pure l
      -- File link to another Org file.
      (Link (URILink "file" (toString -> path)) c)
        -- NOTE TODO: unify this findSource logic to the static files links?
        -- Because right now, only Org links work across sources.
        | (path', ".org") <- splitExtension path -> do
            trueFp <-
              canonicalizePath
                if isAbsolute path'
                  then path'
                  else dir </> relDir </> path'
            lift (lift $ findSource trueFp) >>= \case
              Just oP -> do
                tell (MonoidalMap $ Map.singleton (Left oP) (one Nothing))
                let uri = "source:" <> _opSource oP
                    target = toText $ _opPath oP
                return $ Link (URILink uri target) c
              Nothing -> pure l
      _ -> f l

    toIdentifier = Identifier orgPath . fmap OrgID

    -- FIXME This shit is a mess. Organize this

    docToEntry :: Int -> Tags -> Properties -> (OrgDocument, OrgData) -> (Ap m) (Maybe OrgEntry)
    docToEntry level inhTags inhProps (docToAdd, datum) = Ap $ runMaybeT do
      let tags = filetags datum <> inhTags
          docID = lookupProperty "id" docToAdd
          ex = lookupProperty "roam_exclude" docToAdd

      guard $ (level == 0 || isJust docID) && (isNothing ex || ex == Just "nil")

      let (docWithResolvedLinks, newData) =
            continuePipeline datum $ getCompose $ resolveLinks docToAdd

      (linkedDoc, linksTo) <- lift $ evalStateT (runWriterT $ processLinks docWithResolvedLinks) 0

      let identifier = toIdentifier docID
          docWithProps = linkedDoc & #documentProperties %~ (<> inhProps)
          layout =
            fromMaybe "org-page" $
              lookupProperty "layout" docWithProps

      mtimes <-
        fromMaybe []
          <$> runMaybeT do
            p <- hoistMaybe $ lookupProperty "mtime" docWithProps
            forM (T.split (== ' ') p) \t ->
              hoistMaybe (parseTimeM True defaultTimeLocale "%Y%m%d%H%M%S" (toString t))
                <|> do
                  lift . lift $
                    logWarnNS "Loading" $
                      "Could not parse ctime timestamp "
                        <> t
                        <> " in "
                        <> prettyOrgPath orgPath
                  empty
      ctime <-
        runMaybeT do
          t <- hoistMaybe $ lookupProperty "ctime" docWithProps
          hoistMaybe (parseTimeM True defaultTimeLocale "%Y%m%d%H%M%S.%6N" (toString t))
            <|> do
              lift . lift $
                logWarnNS "Loading" $
                  "Could not parse ctime timestamp "
                    <> t
                    <> " in "
                    <> prettyOrgPath orgPath
              empty
      pure $
        OrgEntry
          { _identifier = identifier,
            _ctime = ctime,
            _mtime = mtimes,
            _tags = tags,
            _document = docWithProps,
            _orgData = newData,
            _level = level,
            _layout = layout,
            _parent =
              if level == 0
                then Nothing
                else Just (toIdentifier $ lookup "id" inhProps),
            _staticFiles = staticFiles,
            _linksTo = coerce linksTo
          }

    processSectionsWithId :: Tags -> Properties -> OrgSection -> (Ap m) [OrgEntry]
    processSectionsWithId tags props section =
      let level = sectionLevel section
       in maybeToList <$> docToEntry level tags props (isolateSection section doc)
