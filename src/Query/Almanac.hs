{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
module Query.Almanac where


import Query.Transit
import Query.Crossing
import Query.Retrograde

import qualified Control.Foldl as L
import qualified Streaming.Prelude as S
import Data.Function
import Streaming (Stream, Of ((:>)))
import SwissEphemeris.Precalculated
import Query.Streaming
import qualified Data.Map as M
import Data.Time
import SwissEphemeris
import Query.Aggregate
import Data.Foldable (foldMap', Foldable (toList))
import Control.Category ((>>>))
import Query.Common (concatForEach)
import Query.LunarPhase
import Query.Eclipse
import Util (show')

data EphemerisEvent
  = EphemerisRetrograde     (Planet, PlanetStation)
  | EphemerisZodiacCrossing (Planet, Crossing Zodiac)
  | EphemerisTransit        ((Planet, Planet), Transit)
  | EphemerisLunarPhase     LunarPhase
  | EphemerisEclipse        Eclipse

instance Show EphemerisEvent where
  show (EphemerisRetrograde (pl, PlanetStation{stationType})) =
    "Retrograde: " <> show' pl <> show stationType
  show (EphemerisZodiacCrossing (pl, Crossing{crossingSubject})) =
    "Crossing: " <> show' pl <> (show . signName $ crossingSubject)
  show (EphemerisTransit ((p1, p2), Transit{aspect, lastPhase, transitOrb})) =
    "Transit: " <> show' p1 <> show' aspect <> show' p2 <> show' lastPhase <> show transitOrb
  show (EphemerisLunarPhase LunarPhase{lunarPhaseName, lunarLongitude}) =
    "Moon Phase: " <> show' lunarPhaseName <> zodiac
    where
      zodiac =
        let split =
              splitDegreesZodiac . getEclipticLongitude <$> lunarLongitude
        in case split of
          Nothing -> ""
          Just s ->
            if lunarPhaseName `elem` [NewMoon, FullMoon, FirstQuarter, LastQuarter] then
              "in " <> show' (longitudeZodiacSign s) <> show lunarLongitude
            else
              ""
  show (EphemerisEclipse ecl) =
    "Eclipse: " <> show ecl

type Calendar = Aggregate Day [EphemerisEvent]
type CalendarJD = Aggregate JulianDayTT [EphemerisEvent]

-- | Given a range of time, produce a map of days and events happening each day;
-- useful for displaying a calendar where one simply wants to list what happens
-- each day, without regard for "merging" things together. See the individual
-- @select@ functions for that.
-- To tally differently, we'd have to:
-- * Add an EventType (Starts | Ends) data type, attributed to Events
-- * For each tallyX function, add /both/ the start and end timestamps
-- * Use the `L.foldMap mapX tallyX` approach so the tallying happens at the end
--   and merging is applied; i.e. we'd only get the days when an event starts or ends
--   populated by said event, instead of every day that it is in effect.
worldAlmanac :: UTCTime -> UTCTime -> Stream (Of (Ephemeris Double)) IO () -> IO Calendar
worldAlmanac start end ephe = do
  (retro, cross, trns, lun) :> _ <-
    ephe
    & ephemerisWindows 2
    & L.purely S.fold mkAlmanac

  Just startUT1 <- toJulianDay start
  Just endUT1   <- toJulianDay end
  ecl <- allEclipses startUT1 endUT1
  talliedEcl <- tallyEclipses ecl

  asCal <- toCalendar $ retro <> cross <> trns <> lun
  pure $ asCal <> talliedEcl

  where
    -- if you do `L.foldMap mapX tallyX`, you'll only get the events
    -- on the days they begin and end; if you do
    -- `L.foldMap (mapX >>> tallyX) id`, you get them for each day
    -- they occur. For our calendar usage, the latter is useful; but
    -- for other applications, the former makes more sense.
    mkAlmanac =
      (,,,) <$> L.foldMap (mapRetrogrades >>> tallyRetrogrades) id
            <*> L.foldMap (mapCrossings zodiacs >>> tallyCrossings) id
            <*> L.foldMap (mapTransits' chosenPairs >>> tallyTransits) id
            <*> L.foldMap (mapLunarPhases' >>> tallyLunarPhases) id
    zodiacs = westernZodiacSigns
    chosenPairs =
      filteredPairs
        uniquePairs
        (tail defaultPlanets) -- everyone but the Moon
        defaultPlanets

toCalendar :: CalendarJD -> IO Calendar
toCalendar =
  getAggregate >>> M.toList >>> foldMap' toUTC
  where
    toUTC (jd, evts) = do
      dateUT <- dayFromJDTT jd
      pure . Aggregate . M.fromList $ [(dateUT, evts)]

westernZodiacSigns :: [Zodiac]
westernZodiacSigns =
  zipWith Zodiac [Aries .. Pisces] zodiacs
  where
    zodiacs = take 12 $ iterate (+ 30) 0

tallyEclipses :: [Eclipse] -> IO Calendar
tallyEclipses =
  foldMap' locateEclipse
  where
    locateEclipse ecl = do
      dateUT <- dayFromJDUT . getEclipseDate $ ecl
      pure . Aggregate . M.fromList $ [
        (dateUT, [EphemerisEclipse ecl])
        ]


tallyLunarPhases :: MergeSeq LunarPhase -> CalendarJD
tallyLunarPhases =
  getMerged >>> toList >>> foldMap' locateLunarPhase
  where
    locateLunarPhase phase@LunarPhase{lunarPhaseStarts} =
      calendarAggregate [
        (lunarPhaseStarts, [EphemerisLunarPhase phase])
        ]

tallyTransits :: TransitMap -> CalendarJD
tallyTransits =
  getAggregate >>> M.toList >>> foldMap' locateTransit
  where
    locateTransit (planets, transits') =
      concatForEach (toList transits') $ \tr@Transit{transitStarts} ->
        calendarAggregate [
          (transitStarts, [EphemerisTransit (planets, tr)])
          ]

tallyCrossings :: CrossingMap Zodiac -> CalendarJD
tallyCrossings =
  getAggregate >>> M.toList >>> foldMap' locateCrossing
  where
    locateCrossing (planet, crossings') =
      concatForEach (toList crossings') $ \cr@Crossing{crossingEnters} ->
        calendarAggregate (start crossingEnters cr)
      where
        start :: Maybe JulianDayTT -> Crossing Zodiac -> [(JulianDayTT, [EphemerisEvent])]
        start Nothing _ = mempty
        start (Just jd) cr = do
          [(jd, [EphemerisZodiacCrossing (planet, cr)])]

tallyRetrogrades :: RetrogradeMap -> CalendarJD
tallyRetrogrades =
  getAggregate >>> M.toList >>> foldMap' locateRetrograde
  where
    locateRetrograde (planet, rs) =
      concatForEach (toList rs) $ \st@PlanetStation{stationStarts} ->
        calendarAggregate [
          (stationStarts, [EphemerisRetrograde (planet, st)])
          ]

calendarAggregate :: [(JulianDayTT, [EphemerisEvent])] -> CalendarJD
calendarAggregate = Aggregate . M.fromList

dayFromJDTT :: JulianDayTT -> IO Day
dayFromJDTT jd = do
  (UTCTime ut _) <- fromJulianDay jd :: IO UTCTime
  pure ut

dayFromJDUT :: JulianDayUT1 -> IO Day
dayFromJDUT jd = do
  (UTCTime ut _) <- fromJulianDay jd :: IO UTCTime
  pure ut
