{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
module Query.Aggregate where

import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import Data.Sequence (ViewR(EmptyR, (:>)), ViewL (EmptyL, (:<)), (<|), (|>), (><))

-- from: https://hackage.haskell.org/package/reducers-3.12.3/docs/Data-Semigroup-Union.html#t:HasUnion
class HasUnion a where
  union :: a -> a -> a

class Merge a where
  merge :: a -> a -> MergeStrategy a

data MergeStrategy a
  = KeepBoth a a
  | KeepL a
  | KeepR a
  deriving (Eq, Show)

newtype MergeSeq a =
  MergeSeq {getMerged :: S.Seq a}
  deriving stock (Show)
  deriving (Semigroup, Monoid) via (S.Seq a)

singleton :: a -> MergeSeq a
singleton = MergeSeq . S.singleton

instance Merge a => HasUnion (MergeSeq a) where
  union (MergeSeq s1) (MergeSeq s2) = 
    MergeSeq $ doMerge s1Last s2First 
    where
      s1Last = S.viewr s1
      s2First = S.viewl s2
      doMerge EmptyR EmptyL = mempty
      doMerge EmptyR (x :< xs) = x <| xs
      doMerge (xs :> x) EmptyL = xs |> x
      doMerge (xs :> x) (y :< ys) =
        case merge x y of
          KeepBoth a b -> (xs |> a) >< (b <| ys)
          KeepL a -> (xs |> a) >< ys
          KeepR b -> xs >< (b <| ys)

-- newtype somewhat inspired by:
-- https://stackoverflow.com/questions/32160350/folding-over-a-list-and-counting-all-occurrences-of-arbitrarily-many-unique-and
newtype Aggregate grouping row =
  Aggregate { getAggregate :: M.Map grouping row}

instance (Ord k, HasUnion v) => Semigroup (Aggregate k v) where
  (Aggregate a1) <> (Aggregate a2) =
    Aggregate $ M.unionWith union a1 a2

instance (Ord k, HasUnion v) => Monoid (Aggregate k v) where
  mempty = Aggregate M.empty

-- | An aggregate where each row is a merge of results
type Grouped a b = Aggregate a (MergeSeq b)
