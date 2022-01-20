{-# LANGUAGE BlockArguments #-}

module Main where

import Models hiding (date, title, body)
import qualified Ema as E
import System.FilePattern
import System.FilePath
import Locale
import Render
import Path
import PandocTransforms
import Caching (cachedMountOnLVar)
import Control.Monad.Trans.Writer.Strict
import PandocTransforms.Roam
import Ema.Helper.FileSystem
import Data.List (stripPrefix)

filesToInclude :: [(MarkupFormat, FilePattern)]
filesToInclude =
  [ (Html, "**/*.html")
  , (Org, "**/*.org")
  , (Md, "**/*.md")
  , (Raw, "**/*")
  ]

filesToExclude :: [FilePattern]
filesToExclude =
  [ "**/imports/**/*"
  , "**/.*/**/*"
  , "**/.*"
  ]

main :: IO ()
main = void $
  E.runEma render $ \ _action ->
    cachedMountOnLVar
      mountSet
      filesToInclude
      filesToExclude
      defaultModel
      \ fmt fp src -> do
        let realFp = mountPoint src </> fp
        if fmt == Raw
        then \case
          Refresh _ () -> tell $ insertSA src fp
          Delete       -> tell $ deleteSA src fp
        else case src of
          Content ->
            let path = urlToPath $
                    if twice takeBaseName fp == "index"
                    then dropExtensions fp <.> "html"
                    else dropExtensions fp </> "index.html"
                locale = drop 1 (takeExtensions fp)
                         & takeWhile (/= '.')
                         & fromMaybe defLocale . toLocale
            in \case
                 Refresh _ () -> do
                   fileText <- readFileText realFp
                   (title, body) <- convertIO realFp Content fmt fileText
                   tell $ insertSP path locale (StructuralPage title body)
                 Delete -> tell $ deleteSP path locale
          Zettel -> \case
            Refresh _ () -> do
              fileText <- readFileText realFp
              convertRoam fp src fileText
            Delete -> tell $ deleteFromRD realFp
          Asset ->
            case stripPrefix "html/" fp of
              Just fp' | fmt == Html -> \case
                Refresh _ () -> tell . insertL (dropExtension fp')
                                  =<< readFileText realFp
                Delete       -> tell $ deleteL (dropExtension fp')
              _ -> \case
                Refresh _ () -> tell $ insertSA src fp
                Delete       -> tell $ deleteSA src fp
  where
    twice f = f . f
