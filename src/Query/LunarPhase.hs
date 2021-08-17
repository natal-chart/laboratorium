{-# LANGUAGE MultiWayIf #-}
module Query.LunarPhase where

import SwissEphemeris.Precalculated
import SwissEphemeris
import Query.Aggregate
import qualified Streaming.Prelude as S
import Streaming (Stream, Of)
import EclipticLongitude

data LunarPhaseName
  = NewMoon
  | WaxingCrescent
  | FirstQuarter
  | WaxingGibbous
  | FullMoon
  | WaningGibbous
  | LastQuarter
  | WaningCrescent
  deriving (Eq, Show, Ord)

data LunarPhase = LunarPhase
  { lunarPhaseName :: !LunarPhaseName
  , lunarPhaseStarts :: !JulianDayTT
  , lunarPhaseEnds :: !JulianDayTT
  } deriving (Eq, Show)

instance Merge LunarPhase where
  x `merge` y =
    if lunarPhaseName x == lunarPhaseName y then
      Merge merged
    else
      KeepBoth
    where
      merged = x {
        lunarPhaseEnds = lunarPhaseEnds y
      }

lunarPhases :: Monad m
  => Stream (Of (Ephemeris Double)) m b
  -> m (Of (MergeSeq LunarPhase) b)
lunarPhases =
  S.foldMap mapLunarPhases

mapLunarPhases :: Ephemeris Double -> MergeSeq LunarPhase
mapLunarPhases dayEphe =
  let sunPos  = dayEphe `forPlanet` Sun
      moonPos = dayEphe `forPlanet` Moon
      phase   = mkLunarPhase <$> sunPos <*> moonPos
      build n = LunarPhase n (epheDate dayEphe) (epheDate dayEphe)
  in maybe mempty (singleton . build) phase


mkLunarPhase :: EphemerisPosition Double -> EphemerisPosition Double -> LunarPhaseName
mkLunarPhase sun moon =
  if | diffDeg < 1+meanSpd                               -> NewMoon 
     | diffDeg >= 1+meanSpd   && diffDeg < 90            -> WaxingCrescent
     | diffDeg >= 90          && diffDeg < 90 + meanSpd  -> FirstQuarter 
     | diffDeg >= 90+meanSpd  && diffDeg < 180           -> WaxingGibbous 
     | diffDeg >= 180         && diffDeg < 180 + meanSpd -> FullMoon
     | diffDeg >= 180+meanSpd && diffDeg < 270           -> WaningGibbous
     | diffDeg >= 270         && diffDeg < 270 + meanSpd -> LastQuarter
     | otherwise                                         -> WaningCrescent
  where
  -- mean lunar speed: the "coarseness" of a day-to-day ephemeris is unlikely
  -- to yield more precision than this when comparing longitudes.
  meanSpd = 360.0/27.32
  diffDeg = getEclipticLongitude $ toEclipticLongitude moon - toEclipticLongitude sun
