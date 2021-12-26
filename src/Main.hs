{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, BlockArguments #-}

module Main where

import qualified Ema.Helper.FileSystem as FS
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Walk as PW
import qualified Data.Time as DT
import qualified Models as M
import qualified Ema as E
import qualified Data.Text as T
import Relude.Extra.Map (delete, insert)
import System.FilePath (takeBaseName, (</>), dropExtension, (-<.>))
import Text.Regex.TDFA ((=~))
import Locale (ptTimeLocale)
import Render (render)
import Path (urlToPath)
import Models (StructuralPage(StructuralPage))
import Lucid (Html)
import PandocUtils
import Caching
import Caching (cachedMountOnLVar)

warn msg = putStrLn ("\ESC[33m\ESC[1m[Warn] " <> msg <> "\ESC[0m")

data FileTag
  = AssetTag RawTag
  | BlogTag MarkupFormat
  | ContentTag MarkupFormat
  deriving (Eq,Ord,Show)

data RawTag
  = RawHtmlTag
  deriving (Eq,Ord,Show)

main :: IO ()
main = -- Main loop!
  E.runEma render $ \_action -> do -- This integrates the whole thing.

    -- First, we need to tell Ema the files that should be watched for changes.
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

    -- Now we set things up.
    -- "void" means we discart the return value.
    void . cachedMountOnLVar
      "."
      filesToInclude
      filesToExclude
      M.defaultModel -- This is my defautl model in Models
      \ tag fp fa model -> case fa of
          FS.Refresh _ () ->
            case tag of
              ContentTag ext -> do
                fileText <- readFileText fp
                (title, body) <- convertIO fp ext fileText

                when (T.null title) $
                  warn $ "File " <> fp <> " has empty title."

                let page = M.StructuralPage title body
                    path = urlToPath (drop (length "content/") $
                                      if takeBaseName fp == "index"
                                      then fp -<.> "html"
                                      else dropExtension fp </> "index.html")

                return (insertSP path page model)

              AssetTag tag -> do
                txt <- readFileText fp
                return (insertRA (assetKey tag fp) txt model)

              BlogTag ext -> do
                fileText <- readFileText fp

                let baseName = takeBaseName fp
                    hasDate = baseName =~ "^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}-"

                unless hasDate $ do
                  warn $ "Blog file " <> baseName <> " has no date in its name."

                let date =
                      if hasDate
                      then DT.parseTimeOrError False ptTimeLocale "%Y-%m-%d" (take 10 baseName)
                      else DT.ModifiedJulianDay 1 -- TODO read date metadata from Pandoc
                let s = if hasDate then drop 11 baseName else baseName

                (title, body) <- convertIO fp ext fileText

                when (T.null title) $
                  warn $ "File " <> fp <> " has empty title."

                let post = M.BlogPost title body date

                return (insertBP (slug fp) post model)

          FS.Delete -> case tag of
            AssetTag tag ->
              return (deleteRA (assetKey tag fp) model)
            BlogTag _ ->
              return (deleteBP (slug fp) model)
            ContentTag _ ->
              return (deleteSP (urlToPath fp) model)
  where
    rawTagToKey RawHtmlTag = M.RawHtmlId
    slug = E.decodeSlug . toText . takeBaseName
    assetKey tag fp = rawTagToKey tag (slug fp)
    deleteRA k m = m { M.rawAssets = delete k (M.rawAssets m) }
    deleteBP k m = m { M.blogPosts = delete k (M.blogPosts m) }
    deleteSP k m = m { M.structuralPages = delete k (M.structuralPages m) }
    insertRA k v m = m { M.rawAssets = insert k v (M.rawAssets m) }
    insertBP k v m = m { M.blogPosts = insert k v (M.blogPosts m) }
    insertSP k v m = m { M.structuralPages = insert k v (M.structuralPages m) }
