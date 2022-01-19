-- |

module Path where
import Ema
import Prelude hiding (fromList)
import System.FilePath (splitDirectories)
import qualified Data.Map.NonEmpty as Map

type Path = [Slug]

newtype Ord a => Tree a
  = Tree { unTree :: Map.NEMap a (Node a)}
  deriving (Eq)

data Node a = Leaf | Subtree (Tree a)
  deriving (Eq)

type Forest a = Maybe (Tree a)

instance forall a. Ord a => Semigroup (Tree a) where
  (Tree m1) <> (Tree m2) =
    let merge (Subtree t1) (Subtree t2) = Subtree (t1 <> t2)
        merge n1 _ = n1
    in Tree $ Map.unionWith merge m1 m2

treeLookup :: Ord a => a -> Tree a -> Maybe (Node a)
treeLookup k = Map.lookup k . unTree

forestLookup :: Ord a => a -> Forest a -> Maybe (Node a)
forestLookup k f = Map.lookup k . unTree =<< f

treeFromList :: Ord a => NonEmpty a -> Tree a
treeFromList (x :| []) = Tree $ Map.singleton x Leaf
treeFromList (x :| xs) = Tree $ Map.singleton x $
  fromMaybe Leaf $ viaNonEmpty (Subtree . treeFromList) xs

forestFromList :: Ord a => [a] -> Forest a
forestFromList [] = Nothing
forestFromList (x:xs) = Just $ treeFromList (x :| xs)

treeInsert :: Ord a => [a] -> Tree a -> Tree a
treeInsert [] = id
treeInsert (x:xs) = (treeFromList (x :| xs) <>)

forestInsert :: Ord a => [a] -> Forest a -> Forest a
forestInsert xs f = treeInsert xs <$> f

treeDelete :: forall a. Ord a => [a] -> Tree a -> Forest a
treeDelete [] = pure -- !! pure means we ignore directories. Should be const
                     -- Nothing if we want to delete entire directories.
treeDelete (x:xs) = fmap Tree . Map.nonEmptyMap . Map.update delIn x . unTree
  where
    delIn :: Node a -> Maybe (Node a)
    delIn Leaf = Nothing
    delIn (Subtree t) = Subtree <$> treeDelete xs t

forestDelete :: Ord a => [a] -> Forest a -> Forest a
forestDelete xs f = treeDelete xs =<< f

(/>) :: (Semigroup a, IsString a) => a -> a -> a
r /> s = r <> "/" <> s

pathToUrl :: Path -> FilePath
pathToUrl = toString . foldr (\s u -> u /> encodeSlug s) ""

urlToPath :: FilePath -> Path
urlToPath = map (decodeSlug . toText) . splitDirectories

isPathOfTree :: Ord a => [a] -> Tree a -> Bool
isPathOfTree [] _ = False
isPathOfTree (x:xs) tree = case treeLookup x tree of
  Nothing -> False
  Just Leaf -> null xs
  Just (Subtree subtree) -> xs `isPathOfTree` subtree

isPathOfForest :: Ord a => [a] -> Forest a -> Bool
isPathOfForest xs = maybe False (isPathOfTree xs)
