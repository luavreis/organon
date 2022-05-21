-- |

module Site.Roam.Process
  ( Place (..)
  , absolute
  , processRoam
  , tell
  ) where
import Site.Roam.Model hiding (tags)
import Control.Monad.Trans.Writer.Strict
import Org.Types
import Org.Walk
import Data.Bitraversable (bimapM)
import Network.URI.Encode (encodeText)
import System.FilePath (takeDirectory, splitDirectories, (</>))
import Data.Text qualified as T

data Place = Place { relative :: FilePath, dir :: FilePath}

absolute :: Place -> FilePath
absolute p = dir p </> relative p

processRoam :: OrgDocument -> Place -> Model -> Model
processRoam post place = appEndo . execWriter $ do
  let fp = absolute place
      fpTags = toText <$> splitDirectories (takeDirectory $ relative place)
      docTags = foldMap sepTags (lookupKeyword "filetags" post)
      tags = fpTags <> docTags

  uuids <- foldMapM (processSections tags fp) (documentSections post)

  case lookupProperty "id" post of
    Just (RoamID -> uuid) -> do
      addToModel tags fp post uuid
      tell $ filterIds fp (uuid : uuids)
    _ -> tell $ filterIds fp uuids

  where
    sepTags (ValueKeyword _ t) =
      t & T.dropAround (':' ==)
        & T.split (':' ==)
    sepTags _ = []

    shift i sec@OrgSection { sectionLevel = level }
      | level + i > 0  = sec { sectionLevel = level + i }
      | otherwise      = sec { sectionLevel = 1 }

    isolateSection :: OrgSection -> OrgDocument
    isolateSection section@OrgSection { sectionLevel = l } =
      let section' = walk (shift (-l + 1)) section
          title = sectionTitle section
      in post { documentKeywords =
               ("title", ParsedKeyword [] title) : filter (("title" /=) . fst) (documentKeywords post)
             }
         & mapContent (const $ sectionContent section')

    addToModel :: [Text] -> FilePath -> OrgDocument -> RoamID -> Writer (Endo Model) ()
    addToModel tags _fp doc' rid = do
      let (doc'', bklinks) = runWriter $ processLinks doc'
          -- doc''' = doc'' { documentChildren = preamble : documentChildren doc'' }
      --           & processCitationsInDoc
      --               (CiteprocOptions True False)
      --               (takeDirectory fp)
      --               ["/home/lucas/Zotero/bibs/all.json"]
      --               (Just "data/citstyle.csl")
      tell $ insertPost rid (Post doc'' tags) $
        map
          (\ ~(ruuid, excrpt) -> (ruuid, Backlink rid (documentTitle post) excrpt))
          bklinks

    processSections :: [Text] -> FilePath -> OrgSection -> Writer (Endo Model) [RoamID]
    processSections tags fp section@OrgSection
      { sectionTags = ((tags <>) -> inhtags)
      , sectionSubsections = (foldMapM (processSections inhtags fp) -> others)
      } = case lookupSectionProperty "id" section of
            Just (RoamID -> uuid) -> do
              addToModel inhtags fp (isolateSection section) uuid
              (<>) [uuid] <$> others
            Nothing -> others


    -- orderedContext :: ListAttributes -> Int -> Block -> Block
    -- orderedContext attr i blk = OrderedList (chgFst i attr) [[blk]]
    --   where chgFst w ~(_,y,z) = (w,y,z)

    -- defListContext :: [Inline] -> Block -> Block
    -- defListContext term blk = DefinitionList [(term, [[blk]])]

    -- bulletListContext :: Block -> Block
    -- bulletListContext blk = BulletList [[blk]]

    processBlock :: (OrgElement -> OrgElement) -> OrgElement -> Writer [(RoamID, [OrgElement])] OrgElement
    -- processBlock _ (PlainList aff kind items) =
    --   PlainList attrs <$> mapM (\(i, blk) -> mapM (processBlock (orderedContext attrs i)) blk) (zip [0..] blks)
    processBlock context blk = mapWriter mapper $ walkM processLink blk
      where
        mapper :: (OrgElement, [RoamID]) -> (OrgElement, [(RoamID, [OrgElement])])
        mapper ~(b, xs) =
          let excerpt = [context b]
          in (b, map (, excerpt) xs)

    -- Walks over the document but only maps the "zero-level" elements (no nesting)
    processLinks :: OrgDocument -> Writer [(RoamID, [OrgElement])] OrgDocument
    processLinks = mapContentM $ bimapM f (walkM mapSections)
      where
        f = mapM (processBlock id)
        mapSections sec@OrgSection { sectionChildren = c } = do
          c' <- f c
          pure $ sec { sectionChildren = c' }

    processLink :: OrgInline -> Writer [RoamID] OrgInline
    processLink (Link (URILink "id" uid) inside) = do
      tell [RoamID uid]
      return $ Link (URILink "http" $ "/zettelkasten/" <> encodeText uid) inside
    processLink x = pure x
