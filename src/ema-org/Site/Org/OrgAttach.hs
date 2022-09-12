{-# LANGUAGE UndecidableInstances #-}

module Site.Org.OrgAttach where

import Control.Monad.Logger
import Control.Monad.Trans.Writer.CPS
import Data.Time (UTCTime)
import Org.Types
import Org.Walk (Walkable (walkM), query)
import Relude.Extra.Map (lookup)
import Site.Org.Common (walkOrgInContextM)
import System.FilePath (takeExtension, (</>))
import UnliftIO (MonadUnliftIO)
import UnliftIO.Directory (doesDirectoryExist, getModificationTime)
import UnliftIO.Exception

processFileLinks ::
  forall m.
  (MonadUnliftIO m, MonadLogger m) =>
  (Text, FilePath) ->
  FilePath ->
  FilePath ->
  OrgDocument ->
  m (OrgDocument, Set (FilePath, UTCTime))
processFileLinks src relDir attDir =
  runWriterT
    . walkOrgInContextM \_ props ->
      processFileLinksInSection src relDir attDir (lookup "id" props)

processFileLinksInSection ::
  forall m.
  (MonadUnliftIO m, MonadLogger m) =>
  (Text, FilePath) ->
  FilePath ->
  FilePath ->
  Maybe Text ->
  [OrgElement] ->
  WriterT (Set (FilePath, UTCTime)) m [OrgElement]
processFileLinksInSection s@(_, srcDir) relDir attDir (Just key) content
  | hasAttachLink content = do
      putStrLn $ show possibleDirs
      findM (doesDirectoryExist . (srcDir </>)) possibleDirs
        >>= \attDir' -> walkM (processFileLink s relDir attDir') content
  where
    rid = toString key
    possibleDirs =
      map
        (attDir </>)
        [ orgAttachIdTSFolderFormat,
          orgAttachIdUUIDFolderFormat,
          rid
        ]

    orgAttachIdTSFolderFormat = take 6 rid </> drop 6 rid
    orgAttachIdUUIDFolderFormat = take 2 rid </> drop 2 rid

    findM p = foldr (\x -> ifM (p x) (pure $ Just x)) (pure Nothing)
processFileLinksInSection s relDir _ _ content =
  walkM (processFileLink s relDir Nothing) content

processFileLink ::
  forall m.
  (MonadUnliftIO m, MonadLogger m) =>
  (Text, FilePath) ->
  FilePath ->
  Maybe FilePath ->
  OrgObject ->
  WriterT (Set (FilePath, UTCTime)) m OrgObject
processFileLink (srcKey, srcDir) relDir attachDir = \case
  (Link t c) -> Link <$> doTarget t ?? c
  (Image t) -> Image <$> doTarget t
  x -> pure x
  where
    findAndInclude fp = do
      lastModified <-
        lift $
          (Just <$> getModificationTime (srcDir </> fp))
            `catch` \(_ :: IOException) -> do
              logWarnN $ "One file links to " <> toText (srcDir </> fp) <> ", but this file does not exist."
              pure Nothing
      whenJust lastModified \lm ->
        tell (one (fp, lm))
      return $ URILink ("source:" <> srcKey) (toText fp)

    doTarget :: LinkTarget -> WriterT (Set (FilePath, UTCTime)) m LinkTarget
    doTarget (URILink "attachment" att)
      | Just aDir <- attachDir =
          findAndInclude (aDir </> toString att)
    doTarget (URILink "file" fp)
      | takeExtension (toString fp) /= ".org" =
          findAndInclude (relDir </> toString fp)
    doTarget l = pure l

hasAttachLink :: Walkable OrgObject a => a -> Bool
hasAttachLink = getAny . query isFileLink
  where
    isFileLink (Link t _) = isFileTarget t
    isFileLink (Image t) = isFileTarget t
    isFileLink _ = mempty

    isFileTarget (URILink "attachment" _) = Any True
    isFileTarget _ = mempty
