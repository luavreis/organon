{-# LANGUAGE BlockArguments #-}

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
import Ema.Helper.FileSystem

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
                    if twice takeBaseName fp == "index"
                    then dropExtensions fp <.> "html"
                    else dropExtensions fp </> "index.html"
              locale = drop 1 (takeExtensions fp)
                       & takeWhile (/= '.')
                       & fromMaybe defLocale . toLocale
          \case
            Refresh _ () -> do
              fileText <- readFileText realFp
              (title, body) <- convertIO realFp Content fmt fileText
              warnNullTitle title fp
              tell $ insertSP path locale (StructuralPage title body)
            Delete -> tell $ deleteSP path locale

        Blog -> do
          if fmt /= Html then do
            let baseName = takeBaseName fp
                hasDate = baseName =~ ("^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}-" :: String)
                date = if hasDate
                      then DT.parseTimeOrError False ptTimeLocale "%Y-%m-%d" (take 10 baseName)
                      else DT.ModifiedJulianDay 1
                slug = E.decodeSlug $ toText if hasDate
                                            then drop 11 baseName
                                            else baseName
            \case
              Refresh _ () -> do
                fileText <- readFileText realFp
                (title, body) <- convertIO realFp Blog fmt fileText
                warnNullTitle title fp
                tell $ insertBP slug (BlogPost title body date)
              Delete -> tell $ deleteBP slug
          else \_ -> pure ()
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
    twice f = f . f
    warnNullTitle title fp =
      when (T.null title) $
        logWarnN $ "File " <> toText fp <> " has empty title."
