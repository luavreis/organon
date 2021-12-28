{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, BlockArguments #-}

module Main where

import Models hiding (date, title, body)
import qualified Ema.Helper.FileSystem as FS
import qualified Data.Time as DT
import qualified Ema as E
import qualified Data.Text as T
import System.FilePath (takeBaseName, (</>), dropExtension, (-<.>))
import Text.Regex.TDFA ((=~))
import Locale (ptTimeLocale)
import Render (render)
import Path (urlToPath)
import PandocTransforms
import Caching (cachedMountOnLVar)
import Control.Monad.Trans.Writer
import Control.Monad.Logger

data FileTag
  = AssetTag RawTag
  | BlogTag MarkupFormat
  | ContentTag MarkupFormat
  deriving (Eq,Ord,Show)

data RawTag
  = RawHtmlTag
  deriving (Eq,Ord,Show)

main :: IO ()
main =
  E.runEma render $ \_action -> do
  -- FIXME get rid of this. reuse routing logic
    let filesToInclude =
          [ (AssetTag RawHtmlTag, "assets/html/*.html")
          , (BlogTag Org, "blog/*.org")
          , (ContentTag Org, "content/**/*.org")
          , (ContentTag Html, "content/**/*.html")
          , (ContentTag Md, "content/**/*.md")
          ]
        filesToExclude =
          [ "content/**/imports/**/*"
          , "**/.*/*"
          ]

    void . cachedMountOnLVar
      "."
      filesToInclude
      filesToExclude
      defaultModel
      \ tag fp -> \ case
          FS.Refresh _ () ->
            case tag of
              ContentTag ext -> do
                fileText <- readFileText fp
                (title, body) <- convertIO fp ext fileText

                when (T.null title) $
                  logWarnN $ "File " <> (toText fp) <> " has empty title."

                let page = StructuralPage title body
                    path = urlToPath (drop (T.length "content/") $
                                      if takeBaseName fp == "index"
                                      then fp -<.> "html"
                                      else dropExtension fp </> "index.html")

                tell $ insertSP path page

              AssetTag atag -> do
                txt <- readFileText fp
                tell $ insertRA (assetKey atag fp) txt

              BlogTag ext -> do
                fileText <- readFileText fp

                let baseName = takeBaseName fp
                    hasDate = baseName =~ ("^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}-" :: String)

                unless hasDate $ do
                  logWarnN $ "Blog file " <> (toText baseName) <> " has no date in its name."

                let date =
                      if hasDate
                      then DT.parseTimeOrError False ptTimeLocale "%Y-%m-%d" (take 10 baseName)
                      else DT.ModifiedJulianDay 1 -- TODO read date metadata from Pandoc
                -- let s = if hasDate then drop 11 baseName else baseName

                (title, body) <- convertIO fp ext fileText

                when (T.null title) $
                  logWarnN $ "File " <> (toText fp) <> " has empty title."

                let post = BlogPost title body date

                tell $ insertBP (slug fp) post

              BlogTag ext -> do
                fileText <- readFileText fp

                let baseName = takeBaseName fp
                    hasDate = baseName =~ ("^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}-" :: String)

                unless hasDate $ do
                  logWarnN $ "Blog file " <> (toText baseName) <> " has no date in its name."

                let date =
                      if hasDate
                      then DT.parseTimeOrError False ptTimeLocale "%Y-%m-%d" (take 10 baseName)
                      else DT.ModifiedJulianDay 1 -- TODO read date metadata from Pandoc
                -- let s = if hasDate then drop 11 baseName else baseName

                (title, body) <- convertIO fp ext fileText

                when (T.null title) $
                  logWarnN $ "File " <> (toText fp) <> " has empty title."

                let post = BlogPost title body date

                tell $ insertBP (slug fp) post

          FS.Delete -> case tag of
            AssetTag atag ->
              tell $ deleteRA (assetKey atag fp)
            BlogTag _ ->
              tell $ deleteBP (slug fp)
            ContentTag _ ->
              tell $ deleteSP (urlToPath fp)
  where
    rawTagToKey RawHtmlTag = RawHtmlId
    slug = E.decodeSlug . toText . takeBaseName
    assetKey tag fp = rawTagToKey tag (slug fp)
