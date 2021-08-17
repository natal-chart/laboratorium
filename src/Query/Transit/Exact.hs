{-# LANGUAGE NamedFieldPuns #-}
module Query.Transit.Exact where

import Query.Transit.Types
import SwissEphemeris
import EclipticLongitude
import Numeric.RootFinding
import Numeric.MathFunctions.Constants (m_epsilon)
import System.IO.Unsafe (unsafePerformIO)
import SwissEphemeris.Precalculated


exactCrossing :: Planet -> EclipticLongitude -> TransitPhase -> IO (Either String JulianDayTT)
exactCrossing planet crossing TransitPhase{phaseName, phaseStarts, phaseEnds} =
  if phaseName `elem` [TriggeredDirect, TriggeredRetrograde ] then
    exactCrossingOn planet crossing phaseStarts phaseEnds
  else
    pure . Left $ "not triggered"

exactCrossingOn
  :: Planet
  -> EclipticLongitude
  -> JulianDayTT
  -> JulianDayTT
  -> IO (Either String JulianDayTT)
exactCrossingOn transiting pos start end =
  case root' of
    Root t -> pure . Right $ mkJulianDay STT t
    NotBracketed -> pure . Left $ "not bracketed" 
    SearchFailed -> pure . Left $ "search failed" 
  where
    root' =
      ridders
        -- keeping tolerance at 5 epsilon, needs to interpolate ~50 times to get
        -- a fix on the sun's position; much fewer for others.
        RiddersParam {riddersMaxIter = 50, riddersTol = RelTol (5 * m_epsilon)}
        (getJulianDay start, getJulianDay end)
        (longitudeIntersects transiting pos)

longitudeIntersects :: Planet -> EclipticLongitude -> Double -> Double
longitudeIntersects p soughtLongitude t =
  -- for the kind of comparisons we're making, the difference in linear distance
  -- should be the same as the "shortest circle" ecliptic longitude distance; if it isn't,
  -- it means there's a "jump" over the 0/360 line, and we ought to invert the operands
  -- to get the desired signum.
  if abs linearDist /= circleDist then
    negate linearDist
  else
    linearDist
  where
    circleDist = soughtLongitude <-> position
    linearDist = getEclipticLongitude soughtLongitude - getEclipticLongitude position
    -- TODO: ahhhhhhhhhhhhhhhhhhhhhhhhhh
    position = unsafePerformIO $ do
      --Debug.traceM $ "seeking: " <> show t <> " for planet " <>  show p <> "intersecting " <> show soughtLongitude
      Right ephe <- readEphemerisEasy True (mkJulianDay STT t)
      case ephe `forPlanet` p of
        Nothing -> fail "no ephe"
        Just EphemerisPosition{epheLongitude} -> do
          pure . EclipticLongitude $ epheLongitude
