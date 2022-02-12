-- |

module PandocTransforms.Links (handleOrgLinks) where
import Models
import Text.Regex.TDFA
import PandocTransforms.Utilities
import System.FilePath
import Network.URI.Encode
import qualified Text.Pandoc.Builder as B
import qualified Data.Text as T
import Control.Monad.Logger
import Ema (unSlug, Slug (Slug), decodeSlug)
import Data.Char (isSpace, isAlphaNum)
import Caching
import Control.Monad.Trans.Writer.Strict

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

linkOrImg ::
  Inline ->
  Maybe ( [Inline] -> Target -> Inline
        , [Inline] , Target, Bool)
linkOrImg (Link at al t) = Just (Link at, al, t, True)
linkOrImg (Image at al t) = Just (Image at, al, t, False)
linkOrImg _ = Nothing

handleOrgLink ::
  MonadLogger m =>
  Place ->
  Maybe UUID ->
  Inline ->
  CacheT Model m Inline
handleOrgLink place@(src, srcFp) mID inl@(linkOrImg -> Just (cons, alt, (link, titl), isLink))
  | isLink, Just anchor <- toString <$> match' (traceShow (place, T.take 30 link) "#")
  = pure $ inl' alt
    (filterExt srcFp ++ "#" ++ encode anchor)
    titl
  | isLink, Just headingText <- match' "*"
  = pure $ inl' alt
    (filterExt srcFp ++ "#" ++ encode (toIdent headingText))
    titl
  | Just linkedFp <- normalise . toString <$>
                     (match' "file:" <|> match' "./" <|> match' "/" $> link)
  = case src of
      Content
        -> if "/" `isPrefixOf` linkedFp
           then -- linkedFp is absolute (unsupported)
             log (warnAbsPath srcFp) $> inl
           else -- linkedFp is relative to `place`
             let path = takeDirectory srcFp </> linkedFp
                 fp = filepath (src, path)
             in tell (insertSA src fp)
                *> fileRelative path
      _ -> log (warnUseAttach place) $> inl
  | Just attPath <- match' "attachment:"
  = if | Just uuid <- mID
         -> let posfix = "data" </> toString (unSlug uuid) </> toString attPath
                path = case src of
                  Zettel -> posfix
                  _ -> takeDirectory srcFp </> posfix
                attFp = filepath (src, path)
            in tell (insertSA src attFp)
               *> fileRelative path
       | otherwise
         -> log (warnAttachNoId srcFp) $> inl
  | isLink, Just _ <- match' "id:"
  = when (src /= Zettel)
      (log $ warnIdNoZettel srcFp)
    $> inl -- We also passthrough in zettelkasten, as those links are handled
           -- in processRoam
  where
    submatches :: (String, String, String, [String]) -> [String]
    submatches (_,_,_,s) = s
    match' m = T.strip <$> T.stripPrefix m link
    log = logWarnNS "handleLink"
    strippedExts = [".org"] -- TODO
    filterExt p
      | takeExtension p `elem` strippedExts = dropExtension p
      | otherwise = p
    inl' a p t = cons a (toText $ servepath (src, p), t)
    fileRelative filePath
      | [path,linenum] <- submatches $ filePath
                          =~ ("^(.+)::([[:digit:]]+)$" :: String)
      = let linenumAnnotation = "(linha " <> toText linenum <> ")"
            newAlt = alt ++ toList (B.text linenumAnnotation)
            newTitle = titl <> " " <> linenumAnnotation
        in pure $ inl' newAlt (filterExt path) newTitle
      | [_,_] <- submatches $ filePath
                 =~ ("^(.+)::\\*+([^*]+)$" :: String)
      = log ("Headline links between files are not supported. " <> -- TODO
              "Some links in file " <> toText srcFp <> " will not work!")
        $> inl
      | [path,anchor] <- submatches $ filePath
                         =~ ("^(.+)::(#.+)$" :: String)
      = pure $ inl' alt (filterExt path ++ encode anchor) titl
      | otherwise
      = pure $ inl' alt (filterExt filePath) titl

    -- TODO this probably does not work well with markdown
    -- https://github.com/jgm/pandoc/blob/899feec4d341cf464e9fc6d72e40477ac7fbac15/src/Text/Pandoc/Shared.hs#L503
    toIdent = toString . T.intercalate "-" . T.words . filterPunct . T.toLower
    filterPunct = T.filter (\c -> isSpace c || isAlphaNum c || isAllowedPunct c)
    isAllowedPunct c = c == '_' || c == '-' || c == '.'
handleOrgLink _ _ inl = pure inl

handleOrgLinksInBlock ::
  MonadLogger m =>
  Place ->
  Maybe UUID ->
  Block ->
  CacheT Model m Block
handleOrgLinksInBlock p _ (Div props@(_, "section":_, kvs) blks)
  | uuid@(Just _) <- Slug . snd <$> find ((== "id") . fst) kvs
  = Div props <$> mapM (handleOrgLinksInBlock p uuid) blks
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
  blocks' <- mapM (handleOrgLinksInBlock p topId) blocks
  return $ Pandoc meta' blocks'
