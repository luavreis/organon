{-# LANGUAGE BlockArguments #-}

module Main where

import Models hiding (title, body)
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
  [ (Org, "**/*.org")
  , (Md, "**/*.md")
  -- , (Raw Image, "**/*.png")
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
  E.runEma render $ \ _action ->
    cachedMountOnLVar
      mountSet
      filesToInclude
      filesToExclude
      defaultModel
      \case
        Raw rtype -> \ fp -> \ case
          Asset -> undefined
          _ -> undefined
        fmt -> \ fp -> \ case
            Asset ->
              case stripPrefix "html/" fp of
                Just fp' | fmt == Raw Html -> \case
                  Refresh _ () -> tell . insertL (dropExtension fp')
                                    =<< readFileText realFp
                  Delete       -> tell $ deleteL (dropExtension fp')
                _ -> \case
                  Refresh _ () -> tell $ insertSA src fp
                  Delete       -> tell $ deleteSA src fp
            Content ->
              let path = urlToPath $ dropExtensions fp <.> "html" -- do this bc fileserver searches for index.html (not just index)
                  locale = drop 1 (takeExtensions fp)
                          & takeWhile (/= '.')
                          & fromMaybe defLocale . toLocale
              in \case
                  Refresh _ () -> do
                    fileText <- readFileText realFp
                    (title, body) <- convertIO place fmt fileText
                    tell $ insertSP path locale (StructuralPage title body)
                  Delete -> tell $ deleteSP path locale
            Zettel -> \case
              Refresh _ () -> do
                fileText <- readFileText realFp
                convertRoam place fileText
              Delete -> tell $ deleteFromRD realFp
