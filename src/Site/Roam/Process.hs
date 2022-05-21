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

  uuids <- foldMapM (processSections tags) (documentSections post)

  case lookupProperty "id" post of
    Just (RoamID -> uuid) -> do
      addToModel tags post uuid
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

    addToModel :: [Text] -> OrgDocument -> RoamID -> Writer (Endo Model) ()
    addToModel tags doc' rid = do
      let bklinks = processLinks doc'
      tell $ insertPost rid (Post doc' tags) $
        map
          (\ ~(ruuid, excrpt) -> (ruuid, Backlink rid (documentTitle doc') excrpt))
          bklinks

    processSections :: [Text] -> OrgSection -> Writer (Endo Model) [RoamID]
    processSections tags section@OrgSection
      { sectionTags = ((tags <>) -> inhtags)
      , sectionSubsections = (foldMapM (processSections inhtags) -> others)
      } = case lookupSectionProperty "id" section of
            Just (RoamID -> uuid) -> do
              addToModel inhtags (isolateSection section) uuid
              (<>) [uuid] <$> others
            Nothing -> others

    -- Walks over the document but only maps the "zero-level" elements (no nesting)
    processLinks :: OrgDocument -> [(RoamID, [OrgElement])]
    processLinks doc' = f (documentChildren doc') <> query mapSections doc'
      where
        f = foldMap processBlock
        mapSections OrgSection{ sectionChildren = c } = f c

    processBlock :: OrgElement -> [(RoamID, [OrgElement])]
    processBlock (PlainList attrs ltype items) =
      flip evalState 0 $
        flip foldMapM items \(ListItem bul con box tag els) -> do
          maybe (modify (+ 1)) put con
          con' <- gets Just
          pure . zip (query processLink els) . repeat . one $
            PlainList attrs ltype [ListItem bul con' box tag els]
    processBlock blk = zip (query processLink blk) (repeat [blk])

    processLink :: OrgInline -> [RoamID]
    processLink (Link (URILink "id" uid) _) = [RoamID uid]
    processLink _ = []
