-- |

module Site.Org.Meta.Types where

import Org.Parser.Definitions (ListItem (..), ListType (..), OrgElement (..), OrgObject (..))
import Relude.Extra.Map (member)
import Site.Org.Utils.MonoidalMap (MonoidalMap)

elementToMetaMap :: OrgElement -> Maybe MetaMap
elementToMetaMap = \case
  PlainList a Descriptive is
    | "meta" `member` a ->
        Just $ fromList $ map itemToValue is
  _ -> Nothing
  where
    itemToValue :: ListItem -> (Text, MetaValue)
    itemToValue (ListItem _ _ _ d els) = (key, val)
      where
        key = case d of
          [Plain x] -> x
          _ -> ""
        val = case els of
          [PlainList _ Descriptive is] ->
            MetaMap $ fromList $ map itemToValue is
          [PlainList _ _ is] ->
            MetaList $ map (snd . itemToValue) is
          [Paragraph _ os] -> MetaObjects os
          _ -> MetaObjects []

type MetaMap = MonoidalMap Text MetaValue

data MetaValue
  = MetaMap MetaMap
  | MetaList [MetaValue]
  | MetaObjects [OrgObject]
  deriving (Eq, Ord, Read, Show, Typeable, Generic, NFData)

instance Semigroup MetaValue where
  MetaMap x <> MetaMap y = MetaMap (x <> y)
  MetaList x <> MetaList y = MetaList (x <> y)
  x <> _ = x
