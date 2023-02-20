{-# LANGUAGE DeriveTraversable #-}

module Site.Organon.Extra.Query where

import Data.Attoparsec.Text hiding (take)
import Data.Char (isSpace)
import Data.Generics.Sum (_As)
import Data.IxSet.Typed qualified as Ix
import Data.List qualified as L
import Data.Text qualified as T
import Ondim.Extra (attributes, ifElse)
import Ondim.Targets.HTML (HtmlNode)
import Optics.Core (Prism', preview, (%))
import Org.Exporters.Processing.OrgData (OrgData (parsedTitle))
import Relude.Extra (lookup)
import Site.Org.Model
import Site.Org.Options (mount, srcToAliasMap)
import Site.Org.Render
import System.FilePattern ((?==))
import Prelude hiding (takeWhile)

data QueryMod a
  = Ensure a
  | Perhaps a
  | Exclude a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

groupQuery :: [QueryMod a] -> ([a], [a], [a])
groupQuery = foldMap go
  where
    go (Ensure x) = ([x], [], [])
    go (Perhaps x) = ([], [x], [])
    go (Exclude x) = ([], [], [x])

parseQuery :: Text -> [QueryMod Text]
parseQuery =
  fromRight (error "parseQuery errors should not happen (please open issue)")
    . parseOnly (queryP <* endOfInput)

queryP :: Parser [QueryMod Text]
queryP = term `sepBy` takeWhile1 isSpace
  where
    term = do
      f <-
        option Perhaps $
          char '+' $> Ensure <|> char '-' $> Exclude
      f . mconcat
        <$> many
          ( asum
              [ takeWhile1 (\c -> c /= ' ' && c /= '\\')
              , escape
              , T.singleton <$> char '\\'
              ]
          )
    escape = try do
      _ <- char '\\'
      T.singleton
        <$> satisfy (inClass " \\+-")

(@/) :: (Ix.Indexable ixs a, Ix.IsIndexOf ix ixs) => Ix.IxSet ixs a -> ix -> Ix.IxSet ixs a
s @/ ix = foldr Ix.delete s $ s Ix.@= ix

(@+?) :: (Ix.Indexable ixs a, Ix.IsIndexOf ix ixs) => Ix.IxSet ixs a -> [ix] -> Ix.IxSet ixs a
s @+? [] = s
s @+? ix = s Ix.@+ ix

(@/*) :: (Ix.Indexable ixs a, Ix.IsIndexOf ix ixs) => Ix.IxSet ixs a -> [ix] -> Ix.IxSet ixs a
s @/* ix = foldl' Ix.intersection s $ map (s @/) ix

filterFiles :: [QueryMod Text] -> Pages -> Pages
filterFiles q p = p Ix.@+ filter (fPred . (.relpath)) files
  where
    files :: [OrgPath] = Ix.indexKeys p
    (en, ph, ex) = groupQuery q
    fPred x =
      getAll (foldMap (All . (?== x) . toString) en)
        && (null ph || getAny (foldMap (Any . (?== x) . toString) ph))
        && not (getAny $ foldMap (Any . (?== x) . toString) ex)

queryExp :: Prism' FilePath Route -> Model -> Expansion HtmlNode
queryExp rp m node = do
  attrs <- attributes node
  let
    route = T.break (== '#') <$> L.lookup "route" attrs
    routePath = fst <$> route
    routeAnchor = Anchor . T.drop 1 . snd <$> route
    parse' x =
      parseQuery <$> L.lookup x attrs
    doRoute =
      fromMaybe id do
        path <- routePath
        return $ fromMaybe (const mempty) do
          id_ <- preview (rp % _As @Identifier) (toString path)
          return (Ix.@= id_)
    doLinksTo =
      fromMaybe (map (,routeAnchor) . toList) do
        path <- L.lookup "links-to" attrs
        pure $ fromMaybe (const []) do
          id_ <- preview (rp % _As @Identifier) (toString path)
          pure $ flip lookupBacklinks id_
    doSources =
      fromMaybe id do
        terms <- parse' "sources"
        (en, ph, ex) <-
          fmap groupQuery $
            forM terms $
              traverse $
                fmap SourceIx . (`lookup` srcAliases)
        pure $ \p -> p Ix.@* en @+? ph @/* ex
    doFiles =
      maybe id filterFiles $
        parse' "paths"
    doTags =
      fromMaybe id $
        (fmap TagIx <<$>> parse' "tags")
          <&> \(groupQuery -> (en, ph, ex)) p ->
            p Ix.@* en @+? ph @/* ex
    doTake =
      maybe id take $
        readMaybe . toString =<< L.lookup "take" attrs
    doSort =
      maybe id sorting $ L.lookup "sort-by" attrs

    srcAliases = srcToAliasMap m.options.mount

    pages' =
      doTake . doSort . doLinksTo . doRoute . doTags . doFiles . doSources $
        m.pages

  ifElse (not (null pages')) node
    `binding` do
      "q:result" ## \node' ->
        join <$> forM pages' \(p, ref) ->
          let page = bindPage rp m.pages p (liftChildren @HtmlNode node')
           in case ref of
                Just (Anchor ref') ->
                  page `bindingText` do
                    "q:target" ## pure ref'
                _ -> page
  where
    sorting s' =
      let (rev, s) = case T.uncons s' of
            Just ('-', r) -> (\f x y -> f y x, r)
            _ -> (id, s')
       in sortBy $
            rev case s of
              "title" -> comparing $ parsedTitle . (.orgData) . fst
              "created" -> comparing $ (.ctime) . fst
              "modified" -> comparing $ (.mtime) . fst
              _ -> comparing $ const ()
