module Query.QueryArr where

import qualified Data.Map.Strict as M
import Data.Foldable (Foldable(foldMap'))
import qualified Control.Category as C
import Control.Arrow
import qualified Data.Bifunctor as B

-- from: https://hackage.haskell.org/package/reducers-3.12.3/docs/Data-Semigroup-Union.html#t:HasUnion
class HasUnion a where
  union :: a -> a -> a

newtype Aggregate grouping row =
  Aggregate { getAggregate :: M.Map grouping row}

instance (Ord k, HasUnion v) => Semigroup (Aggregate k v) where
  (Aggregate a1) <> (Aggregate a2) =
    Aggregate $ M.unionWith union a1 a2

instance (Ord k, HasUnion v) => Monoid (Aggregate k v) where
  mempty = Aggregate M.empty

-- inspired by:
-- https://hackage.haskell.org/package/opaleye-0.7.3.0/docs/Opaleye-Internal-QueryArr.html
newtype QueryArr i o =
  QueryArr { getQueryArrow :: i -> o }

instance C.Category QueryArr where
  id = QueryArr id
  (QueryArr f) . (QueryArr g) = QueryArr $ f . g

instance Arrow QueryArr where
  arr f = QueryArr f
  first (QueryArr f) = QueryArr (B.first f)

runQuery :: (Foldable t, Monoid r) => QueryArr i r -> t i -> r
runQuery (QueryArr f) = foldMap' f
