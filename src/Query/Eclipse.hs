module Query.Eclipse where

import SwissEphemeris.Precalculated
import SwissEphemeris
import Query.Aggregate
import qualified Streaming.Prelude as S
import Streaming (Stream, Of)
import EclipticLongitude

data Eclipse 
  = SolarEclipse !SolarEclipseType !JulianDayUT1 
  | LunarEclipse !LunarEclipseType !JulianDayUT1

solarEclipses :: JulianDayUT1 -> JulianDayUT1 -> IO [Eclipse]
solarEclipses start end = undefined


---
-- loops:
-- https://hackage.haskell.org/package/monad-loops-0.4.3/docs/src/Control-Monad-Loops.html
---
-- | Analogue of @('Prelude.until')@
-- Yields the result of applying f until p holds.
iterateUntilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntilM p f v 
    | p v       = return v
    | otherwise = f v >>= iterateUntilM p f

-- |Execute an action repeatedly until its result satisfies a predicate,
-- and return that result (discarding all others).
iterateUntil :: Monad m => (a -> Bool) -> m a -> m a
iterateUntil p x = x >>= iterateUntilM p (const x)

-- |Execute an action repeatedly until its result fails to satisfy a predicate,
-- and return that result (discarding all others).
iterateWhile :: Monad m => (a -> Bool) -> m a -> m a
iterateWhile p = iterateUntil (not . p)
