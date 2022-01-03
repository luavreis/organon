{-# LANGUAGE DeriveGeneric, BlockArguments #-}

module Main where

import Models hiding (date, title, body)
import qualified Data.Time as DT
import qualified Ema as E
import qualified Data.Text as T
import System.FilePattern
import System.FilePath
import Text.Regex.TDFA ((=~))
import Locale
import Render
import Path
import PandocTransforms
import Caching (cachedMountOnLVar)
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Logger
import PandocTransforms.Roam
import qualified Data.Set as Set
import Ema.Helper.FileSystem
import Data.Binary

data Source
  = Layout
  | Blog
  | Content
  | Zettel
  deriving (Eq,Ord,Show,Enum,Bounded,Generic)

instance Binary Source

mountPoint :: IsString p => Source -> p
mountPoint Zettel = "/home/lucas/Lucas/notas"
mountPoint Blog = "/home/lucas/Lucas/blog"
mountPoint Content = "content"
mountPoint Layout = "assets/html"

mountSet :: Set (Source, FilePath)
mountSet = Set.fromList [(s, mountPoint s) | s <- [minBound .. maxBound]]

filesToInclude :: [(MarkupFormat, FilePattern)]
filesToInclude =
  [ (Html, "**/*.html")
  , (Org, "**/*.org")
  , (Md, "**/*.md")
  ]

filesToExclude :: [FilePattern]
filesToExclude =
  [ "**/imports/**/*"
  , "**/.*/*"
  ]

main :: IO ()
main =
  E.runEma render $ \_action ->
    cachedMountOnLVar
    mountSet
    filesToInclude
    filesToExclude
    defaultModel
    \fmt fp src -> do
      let realFp = mountPoint src </> fp
      case src of
        Content -> do
          let path = urlToPath $
                    if takeBaseName fp == "index"
                    then fp -<.> "html"
                    else dropExtension fp </> "index.html"
          \case
            Refresh _ () -> do
              fileText <- readFileText realFp
              (title, body) <- convertIO realFp fmt fileText
              warnNullTitle title fp
              tell $ insertSP path (StructuralPage title body)
            Delete -> tell $ deleteSP path

        Blog -> do
          let baseName = takeBaseName fp
              hasDate = baseName =~ ("^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}-" :: String)
              date = if hasDate
                    then DT.parseTimeOrError False ptTimeLocale "%Y-%m-%d" (take 10 baseName)
                    else DT.ModifiedJulianDay 1
              slug = E.decodeSlug $ toText if hasDate
                                          then drop 10 baseName
                                          else baseName
          \case
            Refresh _ () -> do
              fileText <- readFileText realFp
              (title, body) <- convertIO realFp fmt fileText
              warnNullTitle title fp
              tell $ insertBP slug (BlogPost title body date)
            Delete -> tell $ deleteBP slug

        Zettel -> \case
          Refresh _ () -> do
            fileText <- readFileText realFp
            convertRoam realFp fileText

          Delete -> tell $ deleteFromRD realFp

        Layout -> \case
          Refresh _ () -> do
            txt <- readFileText realFp
            tell $ insertL (takeBaseName fp) txt
          Delete -> tell $ deleteL (takeBaseName fp)
  where
    warnNullTitle title fp =
      when (T.null title) $
        logWarnN $ "File " <> (toText fp) <> " has empty title."
