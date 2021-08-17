{-# LANGUAGE DerivingStrategies #-}
module Query.Aggregate where

import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import Data.Sequence (ViewR(EmptyR, (:>)), ViewL (EmptyL, (:<)), (<|), (|>), (><))

class Merge a where
  merge :: a -> a -> MergeStrategy a

data MergeStrategy a
  = ReplaceBoth a a
  | ReplaceL a
  | ReplaceR a
  | Merge a
  | KeepBoth
  deriving (Eq, Show)

newtype MergeSeq a =
  MergeSeq {getMerged :: S.Seq a}
  deriving stock (Show)

singleton :: a -> MergeSeq a
singleton = MergeSeq . S.singleton

instance Merge a => Semigroup (MergeSeq a) where
  (MergeSeq s1) <> (MergeSeq s2) = 
    MergeSeq $ doMerge s1Last s2First 
    where
      s1Last  = S.viewr s1
      s2First = S.viewl s2
      doMerge EmptyR EmptyL    = mempty
      doMerge EmptyR (x :< xs) = x <| xs
      doMerge (xs :> x) EmptyL = xs |> x
      doMerge (xs :> x) (y :< ys) =
        case merge x y of
          ReplaceBoth a b -> (xs |> a) >< (b <| ys)
          Merge       a   -> (xs |> a) >< ys
          ReplaceL    a   -> (xs |> a) >< (y <| ys)
          ReplaceR      b -> (xs |> x) >< (b <| ys)
          KeepBoth        -> (xs |> x) >< (y <| ys)

instance Merge a => Monoid (MergeSeq a) where
  mempty = MergeSeq mempty

-- newtype somewhat inspired by:
-- https://stackoverflow.com/questions/32160350/folding-over-a-list-and-counting-all-occurrences-of-arbitrarily-many-unique-and
newtype Aggregate grouping row =
  Aggregate { getAggregate :: M.Map grouping row}
  deriving stock (Show)

instance (Ord k, Semigroup v) => Semigroup (Aggregate k v) where
  (Aggregate a1) <> (Aggregate a2) =
    Aggregate $ M.unionWith (<>) a1 a2

-- NOTE: we _could_ use DerivingVia to automatically derive
-- this trivial instance... except that it seems to re-define
-- `(<>)`, tossing away the one we define for @Semigroup@, above.
instance (Ord k, Semigroup v) => Monoid (Aggregate k v) where
  mempty = Aggregate mempty

-- | An aggregate where each row is a merge of results
type Grouped a b = Aggregate a (MergeSeq b)
