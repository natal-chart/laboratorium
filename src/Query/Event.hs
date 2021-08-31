{-# LANGUAGE NamedFieldPuns #-}

module Query.Event where

-- fns:
-- tuple of (indexAt, evt) -- fromMaybe starts 
-- calendar: map index >>> foldMap' calendar (unionWith <>)

import Query.EventTypes
import Data.Time
import SwissEphemeris
import Data.Function
import EclipticLongitude
import Data.Foldable
import Query.Eclipse (getEclipseDate)
import Data.List (nub)
import qualified Data.Sequence as S
import qualified Data.Map as M
import Data.Functor
import Data.Bifunctor (first)

-- | Get all moments of exactitude in the span of an @Event@; in reality, 
-- only Transits are liable to have more than one moment of exactitude (if they
-- span long enough -- notice that we don't handle the edge case of a transit
-- being exact twice in the same day: we just produce one of the crossings for
-- that day.) In a future update, we'll be able to correctly indicate
-- said multiple crossings (e.g. if a planet happens to just change
-- direction in a given interval and do a double-crossing.)
eventExactAt :: Event -> IO [UTCTime]
eventExactAt (DirectionChange PlanetStation{stationStarts, stationEnds, stationPlanet}) = do
  changesAt <- directionChangeBetween stationPlanet stationStarts stationEnds
  case changesAt of
    Left _e -> pure []
    Right (dirChangesAt, _) -> mapM fromJulianDay [dirChangesAt]
eventExactAt (LunarPhaseChange LunarPhase{lunarPhaseStarts, lunarPhaseEnds, lunarLongitude}) = do
  case lunarLongitude of
    Nothing -> pure []
    Just (EclipticLongitude l) ->
      crossingBetween Moon l lunarPhaseStarts lunarPhaseEnds
      >>= crossingAsList
eventExactAt (EclipseMaximum ecl) =
  mapM fromJulianDay [getEclipseDate ecl]
eventExactAt (PlanetaryTransit t) = transitExactAt t
eventExactAt (HouseTransit t) = transitExactAt t
eventExactAt (ZodiacIngress xn)= crossingExactAt xn
eventExactAt (HouseIngress xn) = crossingExactAt xn

crossingExactAt :: HasEclipticLongitude a => Crossing a -> IO [UTCTime]
crossingExactAt Crossing{crossingPlanet, crossingCrosses, crossingStarts, crossingEnds}=
  crossingBetween crossingPlanet (getEclipticLongitude crossingCrosses) crossingStarts crossingEnds
  >>= crossingAsList

transitExactAt :: Transit a -> IO [UTCTime]
transitExactAt Transit{transitPhases, transitCrosses, transiting} =
  transitPhases
  & toList
  & filter ((`elem` [TriggeredDirect, TriggeredRetrograde]) . phaseName)
  & foldMap' triggeredAt
  where
    triggeredAt TransitPhase{phaseStarts, phaseEnds} = do
      crossingBetween transiting (getEclipticLongitude transitCrosses) phaseStarts phaseEnds
      >>= crossingAsList

crossingAsList :: Either String JulianDayTT -> IO [UTCTime]
crossingAsList crossesAt = do
  case crossesAt of
    Left _e -> pure []
    Right crossesAtTT -> mapM fromJulianDay [crossesAtTT]


-- Get the moment an event starts at, in UTC
eventStartsAt :: Event -> IO UTCTime
eventStartsAt (DirectionChange evt) = fromJulianDay $ stationStarts evt
eventStartsAt (LunarPhaseChange evt) = fromJulianDay $ lunarPhaseStarts evt
eventStartsAt (EclipseMaximum evt) = fromJulianDay $ getEclipseDate evt
eventStartsAt (PlanetaryTransit evt) = fromJulianDay $ transitStarts evt
eventStartsAt (HouseTransit evt) = fromJulianDay $ transitStarts evt
eventStartsAt (ZodiacIngress evt) = fromJulianDay $ crossingStarts evt
eventStartsAt (HouseIngress evt) = fromJulianDay $ crossingEnds evt

-- Get the moment an event ends at, in UTC
eventEndsAt :: Event -> IO UTCTime
eventEndsAt (DirectionChange evt) = fromJulianDay $ stationEnds evt
eventEndsAt (LunarPhaseChange evt) = fromJulianDay $ lunarPhaseEnds evt
eventEndsAt (EclipseMaximum evt) = fromJulianDay $ getEclipseDate evt
eventEndsAt (PlanetaryTransit evt) = fromJulianDay $ transitEnds evt
eventEndsAt (HouseTransit evt) = fromJulianDay $ transitEnds evt
eventEndsAt (ZodiacIngress evt) = fromJulianDay $ crossingEnds evt
eventEndsAt (HouseIngress evt) = fromJulianDay $ crossingEnds evt

eventDates :: Event -> IO [(UTCTime, S.Seq Event)]
eventDates evt = do
  exacts <- eventExactAt evt
  starts <- eventStartsAt evt
  ends <- eventEndsAt evt
  let uniqTimes = nub $ [starts] <> exacts <> [ends]
  pure $ zip uniqTimes (repeat $ S.singleton evt)

-- | Given a timezone and a sequence of events, index them by day (in the given timezone,)
-- with entries for the event's start, moments of exactitude, and end.
indexByDay :: TimeZone -> S.Seq Event -> IO (M.Map Day (S.Seq Event))
indexByDay tz events =
  foldMap' eventDates events
    <&> map (first $ getDay . utcToZonedTime tz)
    <&> M.fromListWith (<>)
  where
    getDay (ZonedTime (LocalTime d _tod) _tz) = d
