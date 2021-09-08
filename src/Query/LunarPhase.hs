{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
module Query.LunarPhase where

import SwissEphemeris.Precalculated
import SwissEphemeris
import Query.Aggregate
import EclipticLongitude
import Data.Sequence (Seq ((:<|)))
import Query.EventTypes
import Control.Applicative (liftA2)
import Numeric.RootFinding
import Numeric.MathFunctions.Constants (m_epsilon)
import qualified Debug.Trace as Debug
import Data.Fixed (mod')

-- | Just to make it play nice with the other folds that look at two days
-- at once.
mapLunarPhases' :: Seq (Ephemeris Double) -> MergeSeq Event
mapLunarPhases' (day1 :<| day2 :<| _) =
    let sunPos1  = day1 `forPlanet` Sun
        moonPos1 = day1 `forPlanet` Moon
        sunPos2  = day2 `forPlanet` Sun
        moonPos2 = day2 `forPlanet` Moon
        sunPositions = liftA2 (,) sunPos1 sunPos2
        moonPositions = liftA2 (,) moonPos1 moonPos2
        phase   = mkLunarPhase (epheDate day1) (epheDate day2) <$> sunPositions <*> moonPositions
        build (n,pos) = LunarPhase n (epheDate day1) (epheDate day2) pos
  in maybe mempty (singleton . LunarPhaseChange . build) phase

mapLunarPhases'  _ = mempty

-- | Get the phase the Moon is currently in, and, if New or Full, try to find the exact
-- longitude of crossing.
mkLunarPhase
  :: JulianDayTT
  -> JulianDayTT
  -> (EphemerisPosition Double, EphemerisPosition Double)
  -> (EphemerisPosition Double, EphemerisPosition Double)
  -> (LunarPhaseName, Maybe JulianDayTT)
mkLunarPhase day1 day2 (sun1, sun2) (moon1, moon2) =
  if | diffDeg <  1+speed                            -> (NewMoon, findCrossingLng 0)
     | diffDeg >= 1+speed   && diffDeg < 90          -> (WaxingCrescent, Nothing)
     | diffDeg >= 90        && diffDeg < 90 + speed  -> (FirstQuarter, findCrossingLng 90)
     | diffDeg >= 90+speed  && diffDeg < 180         -> (WaxingGibbous, Nothing)
     | diffDeg >= 180       && diffDeg < 180 + speed -> (FullMoon, findCrossingLng 180)
     | diffDeg >= 180+speed && diffDeg < 270         -> (WaningGibbous, Nothing)
     | diffDeg >= 270       && diffDeg < 270 + speed -> (LastQuarter, findCrossingLng 270)
     | otherwise                                     -> (WaningCrescent, Nothing)
  where
  diffDeg = getEclipticLongitude $ toEclipticLongitude moon2 - toEclipticLongitude sun2
  speed   = epheSpeed moon2
  approxMoonPos t =
    let dt = abs (getJulianDay day1 - getJulianDay day2)
        t' = abs (getJulianDay day1 - t)
        accel = (epheSpeed moon2 - epheSpeed moon1)/dt
    in if accel > 0 then 
      toEclipticLongitude moon1  + EclipticLongitude (epheSpeed moon1*t' + 0.5*accel*(t' ** 2))
    else
      toEclipticLongitude moon1  + EclipticLongitude (epheSpeed moon2*t' - 0.5*accel*(t' ** 2))
  approxSunPos t =
    let dt = abs (getJulianDay day1 - getJulianDay day2)
        t' = abs (getJulianDay day1 - t)
        accel = (epheSpeed sun2 - epheSpeed sun1)/dt
    in if accel > 0 then
      toEclipticLongitude sun1  + EclipticLongitude (epheSpeed sun1*t' + 0.5*accel*(t' ** 2))
    else
      toEclipticLongitude sun1  + EclipticLongitude (epheSpeed sun2*t' - 0.5*accel*(t' ** 2))
  approxMoonPhase theta t =
    let phaseAngle = EclipticLongitude (approxMoonPos t /-/ approxSunPos t)
    in EclipticLongitude (theta `mod'` 180) /-/ phaseAngle
    --in Debug.trace ("trying: " <> show theta <> "--" <> show trying <> " gives " <> show result) result
  findCrossingLng x =
    let root' = 
          ridders
            RiddersParam {riddersMaxIter = 50, riddersTol = RelTol (4 * m_epsilon)}
            (getJulianDay day1, getJulianDay day2)
            (approxMoonPhase x)
    in case root' of
      Root t -> Debug.trace ("TT" <> show t) $ Just $ mkJulianDay STT t
      NotBracketed -> Debug.trace "not brack" Nothing
      SearchFailed -> Debug.trace "search failed" Nothing
      
    
