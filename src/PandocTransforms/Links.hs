-- |

module PandocTransforms.Links (handleOrgLinks) where
import Models
import Text.Regex.TDFA
import PandocTransforms.Utilities
import System.FilePath
import Network.URI.Encode
import Control.Monad.Logger
import Network.URI.Slug
import Data.Char (isAlphaNum)
import Caching
import Control.Monad.Trans.Writer.Strict
import qualified Text.Pandoc.Builder as B
import qualified Data.Text as T

warnAbsPath :: FilePath -> Text
warnAbsPath fp =
  "For now, absolute filepath links are not supported.\
  \ Some images in file " <> toText fp <> " will not work!"

warnUseAttach :: Place -> Text
warnUseAttach (src, fp) =
  "For now, you should not use filepath links in " <> show src <>
  " directory. Use attachments instead. (From file " <> toText fp <> ")"

warnAttachNoId :: FilePath -> Text
warnAttachNoId fp =
  "There is an attachment: link in file " <> toText fp <>
  " whose corresponding heading appears to not have and id."

warnIdNoZettel :: FilePath -> Text
warnIdNoZettel fp =
  "For now, id: links are only supported in Zettelkasten files.\
  \ Some links in file " <> toText fp <> " use id: and will not work!"

warnUnhandledScheme :: Text -> FilePath -> Text
warnUnhandledScheme kind fp =
  "I don't know what to do with a link of scheme " <> kind <>
  " in file " <> toText fp

linkLikeInl ::
  Inline ->
  Maybe ( [Inline] -> Target -> Inline
        , [Inline], Target, Bool)
linkLikeInl (Link at al t) = Just (Link at, al, t, True)
linkLikeInl (Image at al t) = Just (Image at, al, t, False)
linkLikeInl (Span (id', "spurious-link":cls, ("target", t):kvs) [Emph al])
  | Just ('*', _) <- T.uncons t = Just (Link (id', cls, kvs), al, (t, ""), True)
  -- TODO should check if the anchor (e.g. due to #+name) exists
  | otherwise = Just (Link (id', cls, kvs), al, (T.cons '#' t, ""), True)
linkLikeInl _ = Nothing

readOrgQuick :: Text -> Either PandocError [Block]
readOrgQuick txt = second toBlocks $ runPure $ readOrg readerOptions txt
  where
    toBlocks (Pandoc _ blocks) = blocks

toIdent :: Text -> Text
toIdent (readOrgQuick -> Right [Para inls]) =
  inlineListToIdentifier emptyExtensions inls
toIdent txt = txt

handleOrgLink ::
  MonadLogger m =>
  Place ->
  Maybe UUID ->
  Inline ->
  CacheT Model m Inline
handleOrgLink place@(src, srcFp) mID inl@(linkLikeInl -> Just (cons, alt, (link, titl), isLink))
  | isLink, Just anchor <- toString <$> match' "#"
  = pure $ inl' alt
    (selfLink ++ "#" ++ encode anchor) titl
  | isLink, Just headingText <- match' "*" -- TODO this does not work if the headline has custom_id property
  = pure $ inl' alt
    (selfLink ++ "#" ++ encode (toString $ toIdent headingText)) titl
  | Just attPath <- match' "attachment:"
  = if | Just uuid <- mID
         -> let posfix = "data" </> toString (unSlug uuid) </> toString attPath
                relPath = case src of
                  Zettel -> posfix
                  _ -> takeDirectory srcFp </> posfix
            in tell (insertSA src relPath)
               *> fileRelative relPath
       | otherwise
         -> log (warnAttachNoId srcFp) $> inl
  | isLink, Just _ <- match' "id:"
  = when (src /= Zettel)
      (log $ warnIdNoZettel srcFp)
    $> inl -- We passthrough bc those links are handled in processRoam
  | let (scheme, path) = T.break (== ':') link,
    T.all isAlphaNum scheme && not (T.null path),
    scheme /= "file"
  = when (scheme `notElem` ["http", "https", "data", "mailto"])
      (log $ warnUnhandledScheme scheme srcFp)
    $> inl
  | Just linkedFp <- normalise . toString <$>
                     -- To assume every other link is a file is not entirely
                     -- correct, but is a work around some Pandoc conventions.
                     (match' "file:" <|> match' "./" <|> Just link)
  = case src of
      Content
        -> if "/" `isPrefixOf` linkedFp
           then -- linkedFp is absolute (unsupported)
             log (warnAbsPath srcFp) $> inl
           else -- linkedFp is relative to `place`
             let relPath = takeDirectory srcFp </> linkedFp
             in tell (insertSA src relPath)
                *> fileRelative relPath
      _ -> log (warnUseAttach place) $> inl
  where
    match' m = T.strip <$> T.stripPrefix m link
    log = logWarnNS "handleLink"
    strippedExts = [".org"] -- TODO
    filterExt p
      | takeExtension p `elem` strippedExts = dropExtension p
      | otherwise = p
    inl' a p t = cons a (toText $ servepath (src, p), t)

    selfLink
      | src == Zettel,
        Just uuid <- mID = toString $ unSlug uuid
      | otherwise = filterExt srcFp

    fileRelative filePath
      | [path,linenum] <- submatches $ filePath
                          =~ ("^(.+)::([[:digit:]]+)$" :: String)
      = let linenumAnnotation = "(linha " <> toText linenum <> ")"
            newAlt = alt ++ toList (B.text linenumAnnotation)
            newTitle = titl <> " " <> linenumAnnotation
        in pure $ inl' newAlt (filterExt path) newTitle
      | [path,header] <- submatches $ filePath
                         =~ ("^(.+)::\\*+([^*]+)$" :: String)
      = let link' = filterExt path ++ "#" ++ encode (toString $ toIdent $ toText header)
        in pure $ inl' alt link' titl
      | [path,anchor] <- submatches $ filePath
                         =~ ("^(.+)::#(.+)$" :: String)
      = pure $ inl' alt (filterExt path ++ "#" ++ encode anchor) titl
      | otherwise
      = pure $ inl' alt (filterExt filePath) titl
      where
        submatches :: (String, String, String, [String]) -> [String]
        submatches (_,_,_,s) = s

handleOrgLink _ _ inl = pure inl

handleOrgLinksInBlock ::
  MonadLogger m =>
  Place ->
  Maybe UUID ->
  Block ->
  CacheT Model m Block
handleOrgLinksInBlock p uuid (Div props@(_, "section":_, kvs) blks) =
  let uuid' = decodeSlug . snd <$> find ((== "id") . fst) kvs <|> uuid
  in Div props <$> mapM (handleOrgLinksInBlock p uuid') blks
handleOrgLinksInBlock p uuid blk = walkM (handleOrgLink p uuid) blk

handleOrgLinks ::
  MonadLogger m =>
  Place ->
  Pandoc ->
  CacheT Model m Pandoc
handleOrgLinks p (Pandoc meta blocks) = do
  let topId = case lookupMeta "id" meta of
        Just (MetaString txt) -> Just $ decodeSlug txt
        _ -> Nothing
  meta' <- walkM (handleOrgLink p topId) meta
  blocks' <- makeSections False Nothing blocks
             & mapM (handleOrgLinksInBlock p topId)
  return $ Pandoc meta' blocks'
