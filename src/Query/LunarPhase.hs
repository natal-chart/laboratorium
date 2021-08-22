{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
module Query.LunarPhase where

import SwissEphemeris.Precalculated
import SwissEphemeris
import Query.Aggregate
import qualified Streaming.Prelude as S
import Streaming (Stream, Of)
import EclipticLongitude
import Data.Sequence (Seq ((:<|)))

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
  , lunarLongitude :: !(Maybe EclipticLongitude)
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

instance Temporal LunarPhase where
  type TemporalIndex LunarPhase = JulianDayTT
  startTime = lunarPhaseStarts
  endTime = lunarPhaseEnds

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
      build (n,pos) = LunarPhase n (epheDate dayEphe) (epheDate dayEphe) pos
  in maybe mempty (singleton . build) phase

-- | Just to make it play nice with the other folds that look at two days
-- at once.
mapLunarPhases' :: Seq (Ephemeris Double) -> MergeSeq LunarPhase
mapLunarPhases' (_day1 :<| day2 :<| _) =
  mapLunarPhases day2 
mapLunarPhases'  _ = mempty

-- | Get the phase the Moon is currently in, and, if New or Full, try to find the exact
-- longitude of crossing.
mkLunarPhase :: EphemerisPosition Double -> EphemerisPosition Double -> (LunarPhaseName, Maybe EclipticLongitude)
mkLunarPhase sun moon =
  if | diffDeg <  1+speed                            -> (NewMoon, findCrossingLng 0)
     | diffDeg >= 1+speed   && diffDeg < 90          -> (WaxingCrescent, Nothing)
     | diffDeg >= 90        && diffDeg < 90 + speed  -> (FirstQuarter, Nothing)
     | diffDeg >= 90+speed  && diffDeg < 180         -> (WaxingGibbous, Nothing)
     | diffDeg >= 180       && diffDeg < 180 + speed -> (FullMoon, findCrossingLng 180)
     | diffDeg >= 180+speed && diffDeg < 270         -> (WaningGibbous, Nothing)
     | diffDeg >= 270       && diffDeg < 270 + speed -> (LastQuarter, Nothing)
     | otherwise                                     -> (WaningCrescent, Nothing)
  where
  diffDeg = getEclipticLongitude $ toEclipticLongitude moon - toEclipticLongitude sun
  speed   = epheSpeed moon
  findCrossingLng x = 
    Just loc
    where
      off = diffDeg - x
      loc = toEclipticLongitude moon - EclipticLongitude speed + EclipticLongitude off
