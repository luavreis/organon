{-# LANGUAGE BlockArguments #-}

module Main where

import Models hiding (title, body)
import Locale
import Render
import Path
import PandocTransforms
import Caching (cachedMountOnLVar)
import Ema (runEma)
import System.UnionMount
import System.FilePattern
import System.FilePath
import Control.Monad.Trans.Writer.Strict
import PandocTransforms.Roam
import Data.List (stripPrefix)

mountSet :: Set (Source, FilePath)
mountSet = fromList [(s, mountPoint s) | s <- [Asset, Content, Zettel]]

filesToInclude :: [(Format, FilePattern)]
filesToInclude =
  [ (Markup Org, "**/*.org")
  , (Markup Md, "**/*.md")
  , (Raw Image, "**/*.png")
  , (Raw Image, "**/*.jpg")
  , (Raw Html, "**/*.html")
  ]

filesToExclude :: [FilePattern]
filesToExclude =
  [ "**/imports/**/*"
  , "**/.*/**/*"
  , "**/.*"
  ]

main :: IO ()
main = void $
  runEma render $ \ _action ->
    cachedMountOnLVar
      mountSet
      filesToInclude
      filesToExclude
      defaultModel
      \ fmt fp src ->
        let place = (src, fp)
            realFp = filepath place
        in case fmt of
          Raw rtype ->
            case src of
              Asset ->
                case stripPrefix "html/" fp of
                  Just fp' | rtype == Html -> \case
                    Refresh _ () -> tell . insertL (dropExtension fp')
                                      =<< readFileText realFp
                    Delete       -> tell $ deleteL (dropExtension fp')
                  _ -> \case
                    Refresh _ () -> tell $ insertSA src fp
                    Delete       -> tell $ deleteSA src fp
              _ -> const $ pure ()
          Markup mtype ->
            case src of
              Content ->
                let path = urlToPath $ dropExtensions fp <.> "html" -- do this bc fileserver searches for index.html (not just index)
                    locale = drop 1 (takeExtensions fp)
                            & takeWhile (/= '.')
                            & fromMaybe defLocale . toLocale
                in \case
                    Refresh _ () -> do
                      fileText <- readFileText realFp
                      (title, body) <- convertIO place mtype fileText
                      tell $ insertSP path locale (StructuralPage title body)
                    Delete -> tell $ deleteSP path locale
              Zettel -> \case
                Refresh _ () -> do
                  fileText <- readFileText realFp
                  convertRoam place fileText
                Delete -> tell $ deleteFromRD realFp
              Asset -> const $ pure ()
