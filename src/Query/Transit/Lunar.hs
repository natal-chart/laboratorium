{-# LANGUAGE NamedFieldPuns #-}

-------------------------------------------------------------------------------
-- LUNAR ASPECTS
-------------------------------------------------------------------------------

-- | We use a different approach to lunar aspects: since the moon can move up to
-- 15 degrees per day, iterating over all ephemeris in an interval in day-wise steps
-- is too coarse: an entire applying/trigger/separating cycle can fall through!
-- Instead, we use the `moonCrossing` function provided by SwissEphemeris, which
-- does somewhat more clever interpolation with non-precalc ephemeris to find the
-- next time the moon will cross a longitude -- made possible by the fact that the Moon
-- never presents retrograde motion from a geocentric perspective. By the same token,
-- once we know when the aspect is triggered exactly, we can estimate a rough application/
-- separation interval without consulting any ephemeris. We incur many more IO actions here,
-- so lunar aspects should be reserved to daily/montly use cases, vs. heavier weight yearly
-- use cases that would not only do a lot of IO, also end up with a lot of results (since
-- in a given month, the Moon is likely to present almost every aspect.)
--
-- A similar function could be written for the Sun, with the appropriate mean solar speed.

{-# LANGUAGE TupleSections #-}
module Query.Transit.Lunar where

import SwissEphemeris
import SwissEphemeris.Precalculated
import Query.Transit.Types
import qualified Data.Map as M
import EclipticLongitude
import Data.List ( nub )
import Data.Either
import Query.Aggregate
import Data.Foldable
import Data.Sequence (fromList)

selectLunarTransits :: JulianDayTT -> JulianDayTT -> Ephemeris Double -> IO TransitMap
selectLunarTransits start end natalEphemeris =
  foldMap' mkLunarTransit (ephePositions natalEphemeris)
  where
    mkLunarTransit :: EphemerisPosition Double -> IO TransitMap
    mkLunarTransit pos = do
      transit <- lunarAspects start end pos --(EclipticLongitude . epheLongitude $ pos)
      if null transit then
        mempty
      else
        pure $ Aggregate $ M.fromList [((Moon, ephePlanet pos), MergeSeq $ fromList transit)]

lunarAspects
  :: HasEclipticLongitude a
  => JulianDayTT
  -> JulianDayTT
  -> a
  -> IO [Transit]
lunarAspects start end pos =
  foldMap' crossings aspects
  where
    crossings :: Aspect -> IO [Transit]
    crossings Aspect{aspectName, angle} = do
      let crossA = toEclipticLongitude pos + EclipticLongitude angle
          crossB = toEclipticLongitude pos - EclipticLongitude angle
      crossesA <- moonCrossingBetween (getEclipticLongitude crossA) start end
      crossesB <- moonCrossingBetween (getEclipticLongitude crossB) start end
      pure 
        . map (toTransit aspectName angle) 
        . nub 
        . rights 
        $ [(,crossA) <$> crossesA, (,crossB) <$> crossesB]

    toTransit aspname angl (exactCrossingTime, exactCrossingLng) =
      Transit {
        aspect = aspname,
        lastPhase = ApplyingDirect,
        transitAngle = angl,
        transitOrb = 0,
        transitStarts = estimateStart exactCrossingTime,
        transitIsExact = Just exactCrossingTime,
        transitEnds = estimateEnd exactCrossingTime,
        transitProgress = mempty,
        transitPhases = mempty,
        transitCrosses = exactCrossingLng
      }
    -- from:
    -- https://github.com/aloistr/swisseph/blob/40a0baa743a7c654f0ae3331c5d9170ca1da6a6a/sweph.c#L8494
    meanLunarSpeed = 360.0 / 27.32
    -- given the mean lunar speed, estimate when the Moon was 5 degrees before
    -- and 5 degrees after the moment of exact crossing
    estimateStart t' =
      mkJulianDay STT tBefore
      where
        tBefore = getJulianDay t' - 5/meanLunarSpeed
    estimateEnd t' =
      mkJulianDay STT tAfter
      where
        tAfter = getJulianDay t' + 5/meanLunarSpeed
