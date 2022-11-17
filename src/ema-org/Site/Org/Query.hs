{-# LANGUAGE DeriveFunctor #-}

module Site.Org.Query where

import Data.Attoparsec.Text hiding (take)
import Data.Char (isSpace)
import Data.IxSet.Typed qualified as Ix
import Data.List qualified as L
import Data.Text qualified as T
import Ondim.Extra (attributes)
import Ondim.HTML (HtmlNode)
import Optics.Core (Prism')
import Site.Org.Model
import Site.Org.Render
import System.FilePattern ((?==))
import Prelude hiding (takeWhile)
import Org.Exporters.Processing.OrgData (OrgData(parsedTitle))

data QueryMod a
  = Ensure a
  | Perhaps a
  | Exclude a
  deriving (Eq, Ord, Show, Functor)

groupQuery :: [QueryMod a] -> ([a], [a], [a])
groupQuery = foldMap go
  where
    go (Ensure x) = ([x], [], [])
    go (Perhaps x) = ([], [x], [])
    go (Exclude x) = ([], [], [x])

parseQuery :: Text -> [QueryMod Text]
parseQuery = fromRight [] . parseOnly (queryP <* endOfInput)

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
              [ takeWhile1 (\c -> c /= ' ' && c /= '\\'),
                escape,
                T.singleton <$> char '\\'
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
filterFiles q p = p Ix.@+ filter (fPred . _opPath) files
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
  let parse' x =
        parseQuery <$> L.lookup x attrs
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
  case L.lookup "from" attrs of
    Just "notes" -> do
      let pages' =
            doTake $
              doSort $
                toList $
                  doTags $
                    doFiles $
                      _mPages m
      join <$> forM pages' \p ->
        bindPage "q:" rp (_mPages m) p (children node)
    _ -> pure []
  where
    sorting s' =
      let (rev, s) = case T.uncons s' of
            Just ('-', r) -> (\f x y -> flipOrd $ f x y, r)
            _ -> (id, s')
       in sortBy $
            rev case s of
              "title" -> comparing $ parsedTitle . _orgData
              "created" -> comparing _ctime
              "modified" -> comparing _mtime
              _ -> comparing $ const ()
    flipOrd GT = LT
    flipOrd EQ = EQ
    flipOrd LT = GT
