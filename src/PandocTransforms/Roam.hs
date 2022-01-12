{-# LANGUAGE BlockArguments #-}
-- |

module PandocTransforms.Roam where
import Ema
import PandocTransforms.Utilities hiding (Writer)
import Models
import PandocTransforms.Org
import PandocTransforms.LaTeX
import PandocTransforms.Links
import Control.Monad.Trans.Writer.Strict
import System.FilePath
import Caching
import Text.Pandoc.Citeproc
import qualified Data.Text as T
import UnliftIO
import Control.Monad.Logger
import Data.Time (Day(ModifiedJulianDay))

processRoam
  :: forall m. (MonadLogger m)
  => FilePath -> (Pandoc, Block) -> CacheT Model m ()
processRoam fp (Pandoc meta blocks, preamble) = do
  let blocks' = makeSections False Nothing blocks

  uuids <- query processDiv blocks'
  tell $ mapFilterRD fp uuids

  case lookupMeta "id" meta of
    Just (MetaString uuid) -> addToModel blocks' (docTitle meta) (Slug uuid)
    _ -> mempty

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
          tell $ insertRP uuid (BlogPost tit bod $ ModifiedJulianDay 1)
          tell $ addToRD uuid $
            map (\ ~(ruuid, excrpt) -> (ruuid, RoamBacklink uuid tit excrpt))
            bklinks

    processDiv :: Block -> CacheT Model m [UUID]
    processDiv (Div (_, "section":_, kvs) blks) =
      viaNonEmpty head (filter ((== "id") . fst) kvs)
      & maybe mempty \(_, uuid) -> do
      let uuid' = Slug uuid
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
           : Space : inl) uuid'
        _ -> addToModel blks (docTitle meta) uuid'
      pure [uuid']
      where
        headTail (h :| t) = (h, t)
        shift n (Header level attr inner)
          | level + n > 0  = Header (level + n) attr inner
          | otherwise      = Para inner
        shift _ x            = x
    processDiv _ = mempty

    processBlock :: Block -> Writer [(UUID, Text)] Block
    processBlock (BlockQuote blks) = BlockQuote <$> mapM processBlock blks
    processBlock (Div attr blks) = Div attr <$> mapM processBlock blks
    processBlock (OrderedList attrs blks) = OrderedList attrs <$> mapM (mapM processBlock) blks
    processBlock (BulletList blks) = BulletList <$> mapM (mapM processBlock) blks
    processBlock (DefinitionList blks) = DefinitionList <$> mapM (mapSnd $ mapM (mapM processBlock)) blks
      where mapSnd f ~(x, y) = (x,) <$> f y
    processBlock blk = mapWriter mapper $ walkM processLink blk
      where
        clearAttr :: Attr -> Attr
        clearAttr (_, xs, _) = ("", xs, [])

        clearAttrs :: Block -> Block
        clearAttrs = bottomUp clearAttr

        mapper :: (Block, [UUID]) -> (Block, [(UUID, Text)])
        mapper ~(b, xs) =
          let excerpt = fromRight "" $
                runPure $ writeHtml $ Pandoc meta [clearAttrs b]
          in (b, map (, excerpt) xs)

    processLinks :: [Block] -> Writer [(UUID, Text)] [Block]
    processLinks = mapM processBlock

    processLink :: Inline -> Writer [UUID] Inline
    processLink (Link attr alt (url, titl)) =
      let same = pure $ Link attr alt (url, titl)
      in case T.breakOn ":" url of
        ("id", uid) -> do
          let uuid = Slug $ T.tail uid
          tell [uuid]
          return $ Link attr alt ("/zettelkasten/" <> encodeSlug uuid, titl)
        _ -> same
    processLink x = pure x

convertRoam
  :: (MonadUnliftIO m, MonadLogger m)
  => FilePath
  -> Text
  -> CacheT Model m ()
convertRoam fp txt = do
  let dir = takeDirectory fp

  doc <- liftIO $ runIOorExplode $ do
    setResourcePath [".", dir]
    parsed <- readOrg readerOptions [(dir, txt)]
              <&> applyTransforms
              <&> setOrgVars
              <&> walk (fixLinks dir) -- Maybe we should move this out of this function and instead take a list of transforms
    preamble <- preambilizeKaTeX $ getMeta parsed
    if isJust . lookupMeta "bibliography" . getMeta $ parsed
    then (, preamble) <$> processCitations parsed
    else pure (parsed, preamble)

  processRoam fp =<< mapFst (renderLaTeX dir) doc

  where
    mapFst f ~(x,y) = (, y) <$> f x

    applyTransforms p = foldr ($) p transforms

    transforms :: [Pandoc -> Pandoc]
    transforms =
      [ headerShift 1
      , setMetaP "lang" "pt-BR"
      , setMetaP "csl" "data/citstyle.csl"
      ]
