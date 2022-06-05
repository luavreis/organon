-- |

module Site.Roam.OrgAttach where
import Org.Types
import Org.Walk
import Site.Roam.Model
import UnliftIO (MonadUnliftIO)
import UnliftIO.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import Relude.Extra (insert)
import qualified Data.Set as Set
import Site.Roam.Options as O
import Control.Monad.Logger (logWarnNS, MonadLogger)

getAttachment :: OrgInline -> Set FilePath
getAttachment (Link (URILink "attachment" att) _) = one (toString att)
getAttachment (Image (URILink "attachment" att))  = one (toString att)
getAttachment _ = mempty

processAttachments ::
  (Walkable OrgInline a, MonadUnliftIO m, MonadLogger m, MonadReader Options m) =>
  RoamID ->
  a ->
  m (Endo Model)
processAttachments key@(toString -> rid) target = do
  attDir <- liftA2 (</>) (asks O.mount) (asks O.orgAttachDir)

  let possibleDirs = map (attDir </>) [ orgAttachIdTSFolderFormat
                                      , orgAttachIdUUIDFolderFormat
                                      , rid ]

  findM doesDirectoryExist possibleDirs >>= \case
    Just dir ->
      pure $ Endo \m ->
        m { attachDirs = insert key dir (attachDirs m)
          , attachments = Set.union atts (attachments m) }
    Nothing | null atts -> pure mempty
            | otherwise -> logWarnNS "OrgAttach" ("Could not find a suitable org-attach dir for id " <> toText rid)
                           $> mempty
  where
    toAttach = AttachPath key . toText
    atts = Set.map toAttach $ query getAttachment target

    orgAttachIdTSFolderFormat = take 6 rid </> drop 6 rid
    orgAttachIdUUIDFolderFormat = take 2 rid </> drop 2 rid

    findM p = foldr (\x -> ifM (p x) (pure $ Just x)) (pure Nothing)
