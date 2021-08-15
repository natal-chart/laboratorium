{-# LANGUAGE NamedFieldPuns #-}
module Query.Transit.Exact where

import Query.Transit.Types
import SwissEphemeris
import EclipticLongitude
import Numeric.RootFinding
import Numeric.MathFunctions.Constants (m_epsilon)
import System.IO.Unsafe (unsafePerformIO)
import SwissEphemeris.Precalculated
import qualified Debug.Trace as Debug


exactCrossing :: Planet -> EclipticLongitude -> TransitPhase -> IO (Maybe JulianDayTT)
exactCrossing planet crossing TransitPhase{phaseName, phaseStarts, phaseEnds} =
  case phaseName of
    TriggeredDirect -> exactCrossingOn planet crossing phaseStarts phaseEnds
    TriggeredRetrograde -> exactCrossingOn planet crossing phaseStarts phaseEnds
    _ -> pure Nothing

exactCrossingOn
  :: Planet
  -> EclipticLongitude
  -> JulianDayTT
  -> JulianDayTT
  -> IO (Maybe JulianDayTT)
exactCrossingOn transiting pos start end =
  case root' of
    Root t -> pure $ Just $ mkJulianDay STT t
    NotBracketed -> Debug.trace "not bracketed" $ pure Nothing
    SearchFailed -> Debug.trace "search failed" $ pure Nothing
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
    circleDist = soughtLongitude <-> EclipticLongitude position
    linearDist = getEclipticLongitude soughtLongitude - position
    -- TODO: ahhhhhhhhhhhhhhhhhhhhhhhhhh
    position = unsafePerformIO $ do
      --Debug.traceM $ "seeking: " <> show t <> " for planet " <>  show p <> "intersecting " <> show soughtLongitude
      Right ephe <- readEphemerisEasy True (mkJulianDay STT t)
      case ephe `forPlanet` p of
        Nothing -> fail "no ephe"
        Just EphemerisPosition{epheLongitude} -> do
          --Debug.traceM $ "Lng found " <> show epheLongitude
          pure epheLongitude
