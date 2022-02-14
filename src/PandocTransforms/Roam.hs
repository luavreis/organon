{-# LANGUAGE BlockArguments #-}
-- |

module PandocTransforms.Roam where
import Models
import PandocTransforms.Utilities hiding (Writer)
import Network.URI.Slug
import PandocTransforms.Org
import PandocTransforms.LaTeX
import PandocTransforms.Links
import Control.Monad.Trans.Writer.Strict
import System.FilePath
import Caching
import Text.Pandoc.Citeproc
import UnliftIO
import Control.Monad.Logger
import Data.Time (Day(ModifiedJulianDay))
import qualified Data.Text as T

processRoam
  :: forall m. (MonadLogger m)
  => FilePath -> (Pandoc, Block) -> CacheT Model m ()
processRoam fp (Pandoc meta blocks, preamble) = do

  uuids <- query processDiv blocks

  case lookupMeta "id" meta of
    Just (MetaString uuid) -> do
      addToModel blocks (docTitle meta) (decodeSlug uuid)
      tell $ mapFilterRD fp (decodeSlug uuid : uuids)
    _ -> tell $ mapFilterRD fp uuids

  where
    addToModel :: [Block] -> [Inline] -> UUID -> CacheT Model m ()
    addToModel blks rawtitle uuid = do
      let (blks', bklinks) = runWriter $ processLinks blks
          result = do
            tit <- writeHtml $ Pandoc meta [Plain rawtitle]
            bod <- writeHtml $ Pandoc meta (preamble : blks')
            pure (tit, bod)
      case runPure result of
        Left e ->
          logErrorNS "Roam" ("Pandoc writer failed with:\n" <> renderError e)
        Right (tit, bod) -> do
          tell $ insertRP uuid (RoamPost tit bod (ModifiedJulianDay 1) [])
          tell $ addToRD uuid $
            map (\ ~(ruuid, excrpt) -> (ruuid, RoamBacklink uuid tit excrpt))
            bklinks

    processDiv :: Block -> CacheT Model m [UUID]
    processDiv (Div (_, "section":_, kvs) blks)
      | Just uuid <- decodeSlug . snd <$> find ((== "id") . fst) kvs = do
          case viaNonEmpty headTail blks of
            Just (Header i  _ inl, t) ->
              addToModel (walk (shift (-i + 1)) t)
              (Span ("", [], [("style", "font-size: 90%")]) (docTitle meta)
                : LineBreak
                : Span ("", [], [ ( "style"
                                  , "font-family: sans; \
                                    \ font-weight: lighter; \
                                    \ font-size: 85%"
                                  ) ]) [Str "→"]
                : Space : inl) uuid
            _ -> addToModel blks (docTitle meta) uuid
          pure [uuid]
      where
        headTail (h :| t) = (h, t)
        shift n (Header level attr inner)
          | level + n > 0  = Header (level + n) attr inner
          | otherwise      = Para inner
        shift _ x            = x
    processDiv _ = mempty

    orderedContext :: ListAttributes -> Int -> Block -> Block
    orderedContext attr i blk = OrderedList (chgFst i attr) [[blk]]
      where chgFst w ~(_,y,z) = (w,y,z)

    defListContext :: [Inline] -> Block -> Block
    defListContext term blk = DefinitionList [(term, [[blk]])]

    bulletListContext :: Block -> Block
    bulletListContext blk = BulletList [[blk]]

    processBlock :: (Block -> Block) -> Block -> Writer [(UUID, Text)] Block
    processBlock _ (BlockQuote blks) = BlockQuote <$> mapM (processBlock id) blks
    processBlock _ (Div attr blks) = Div attr <$> mapM (processBlock id) blks
    processBlock _ (OrderedList attrs blks) = OrderedList attrs <$> mapM (\(i, blk) -> mapM (processBlock (orderedContext attrs i)) blk) (zip [0..] blks)
    processBlock _ (BulletList blks) = BulletList <$> mapM (mapM $ processBlock bulletListContext) blks
    processBlock _ (DefinitionList blks) = DefinitionList <$> mapM (\(term, etc) -> (term,) <$> mapM (mapM $ processBlock (defListContext term)) etc) blks
    processBlock context blk = mapWriter mapper $ walkM processLink blk
      where
        clearAttr :: Attr -> Attr
        clearAttr (_, xs, _) = ("", xs, [])

        clearAttrs :: Block -> Block
        clearAttrs = bottomUp clearAttr

        mapper :: (Block, [UUID]) -> (Block, [(UUID, Text)])
        mapper ~(b, xs) =
          let excerpt = fromRight "" $
                runPure $ writeHtml $ Pandoc meta [clearAttrs $ context b]
          in (b, map (, excerpt) xs)

    processLinks :: [Block] -> Writer [(UUID, Text)] [Block]
    processLinks = mapM $ processBlock id

    processLink :: Inline -> Writer [UUID] Inline
    processLink (Link attr alt (url, titl)) =
      let same = pure $ Link attr alt (url, titl)
      in case T.breakOn ":" url of
        ("id", uid) -> do
          let uuid = decodeSlug $ T.tail uid
          tell [uuid]
          return $ Link attr alt ("/zettelkasten/" <> encodeSlug uuid, titl)
        _ -> same
    processLink x = pure x

convertRoam
  :: (MonadUnliftIO m, MonadLogger m)
  => Place
  -> Text
  -> CacheT Model m ()
convertRoam place@(_,fp) txt = do
  let
    realFp = filepath place
    dir = takeDirectory realFp

  doc :: Maybe (Pandoc, Block) <- liftIO $ runIOorExplode $ do
    setResourcePath [".", dir]
    parsed' <- readOrg readerOptions [(realFp, txt)]
               <&> setOrgVars

    let meta = getMeta parsed'

    parsed <- parsed'
              & applyTransforms
              & if isJust (lookupMeta "bibliography" meta)
                then processCitations
                else pure

    preamble <- preambilizeKaTeX $ getMeta parsed
    pure $ Just (parsed, preamble)

  flip (maybe $ tell $ deleteFromRD fp) doc $
    mapFst (handleOrgLinks place >=> renderLaTeX dir)
    >=> processRoam fp

  where
    mapFst f ~(x,y) = (, y) <$> f x

    applyTransforms p = foldr ($) p transforms

    transforms :: [Pandoc -> Pandoc]
    transforms =
      [ headerShift 1
      , setMetaP "lang" "pt-BR"
      , setMetaP "csl" "data/citstyle.csl"
      ]
