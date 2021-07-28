module Query.Aggregate where

import qualified Data.Map.Strict as M

-- from: https://hackage.haskell.org/package/reducers-3.12.3/docs/Data-Semigroup-Union.html#t:HasUnion
class HasUnion a where
  union :: a -> a -> a

-- newtype somewhat inspired by:
-- https://stackoverflow.com/questions/32160350/folding-over-a-list-and-counting-all-occurrences-of-arbitrarily-many-unique-and
newtype Aggregate grouping row =
  Aggregate { getAggregate :: M.Map grouping row}

instance (Ord k, HasUnion v) => Semigroup (Aggregate k v) where
  (Aggregate a1) <> (Aggregate a2) =
    Aggregate $ M.unionWith union a1 a2

instance (Ord k, HasUnion v) => Monoid (Aggregate k v) where
  mempty = Aggregate M.empty
