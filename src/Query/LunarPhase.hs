{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
module Query.LunarPhase where

import SwissEphemeris.Precalculated
import SwissEphemeris
import Query.Aggregate
import EclipticLongitude
import Data.Sequence (Seq ((:<|)))
import Query.EventTypes

mapLunarPhases :: Ephemeris Double -> MergeSeq Event 
mapLunarPhases dayEphe =
  let sunPos  = dayEphe `forPlanet` Sun
      moonPos = dayEphe `forPlanet` Moon
      phase   = mkLunarPhase <$> sunPos <*> moonPos
      build n = LunarPhase n (pred $ epheDate dayEphe) (epheDate dayEphe)
  in maybe mempty (singleton . LunarPhaseChange . build) phase

-- | Just to make it play nice with the other folds that look at two days
-- at once.
mapLunarPhases' :: Seq (Ephemeris Double) -> MergeSeq Event
mapLunarPhases' (_day1 :<| day2 :<| _) =
  mapLunarPhases day2 
mapLunarPhases'  _ = mempty

-- | Get the phase the Moon is currently in
mkLunarPhase :: EphemerisPosition Double -> EphemerisPosition Double -> LunarPhaseName
mkLunarPhase sun moon =
  if | diffDeg <  1+speed                            -> NewMoon
     | diffDeg >= 1+speed   && diffDeg < 90          -> WaxingCrescent
     | diffDeg >= 90        && diffDeg < 90 + speed  -> FirstQuarter
     | diffDeg >= 90+speed  && diffDeg < 180         -> WaxingGibbous
     | diffDeg >= 180       && diffDeg < 180 + speed -> FullMoon
     | diffDeg >= 180+speed && diffDeg < 270         -> WaningGibbous
     | diffDeg >= 270       && diffDeg < 270 + speed -> LastQuarter
     | otherwise                                     -> WaningCrescent
  where
  diffDeg = getEclipticLongitude $ toEclipticLongitude moon - toEclipticLongitude sun
  speed   = epheSpeed moon
