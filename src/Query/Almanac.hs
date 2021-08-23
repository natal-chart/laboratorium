{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
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
  | EphemerisHouseCrossing  (Planet, Crossing House)
  | EphemerisTransit        ((Planet, Planet), Transit)
  | EphemerisHouseTransit   ((Planet, House), Transit)
  | EphemerisLunarPhase     LunarPhase
  | EphemerisEclipse        Eclipse

instance Show EphemerisEvent where
  show (EphemerisRetrograde (pl, PlanetStation{stationType})) =
    "Retrograde: " <> show' pl <> show stationType
  show (EphemerisZodiacCrossing (pl, Crossing{crossingSubject})) =
    "Crossing: " <> show' pl <> (show . signName $ crossingSubject)
  show (EphemerisHouseCrossing (pl, Crossing{crossingSubject})) =
    "Crossing: " <> show' pl <> (show . houseName $ crossingSubject)
  show (EphemerisTransit ((p1, p2), Transit{aspect, lastPhase, transitOrb})) =
    "Transit: " <> show' p1 <> show' aspect <> show' p2 <> show' lastPhase <> show transitOrb
  show (EphemerisHouseTransit ((p1, p2), Transit{aspect, lastPhase, transitOrb})) =
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

exactEvent :: EphemerisEvent -> IO (Either String JulianDayTT)
exactEvent (EphemerisTransit ((transiting, _transited), Transit{transitPhases, transitCrosses, transitIsExact})) =
  if transiting == Moon then
    case transitIsExact of
      Nothing -> pure . Left $ "not triggered"
      Just tt -> pure . Right $ tt
  else
    case toList . getMerged $ transitPhases of
      [] -> pure . Left $ "not triggered"
      (phase:_) ->  exactCrossing transiting transitCrosses phase
exactEvent _ = pure . Left $ "not implemented"

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
    -- if you do `foldByStartEnd`, you'll only get the events
    -- on the days they begin and end; if you do
    -- `foldEachDay`, you get them for each day
    -- they occur. For our calendar usage, the latter is useful; but
    -- for other applications, the former makes more sense.
    mkAlmanac =
      (,,,) <$> foldEachDay mapRetrogrades EphemerisRetrograde
            <*> foldEachDay (mapCrossings zodiacs) EphemerisZodiacCrossing
            <*> foldEachDay (mapTransits' chosenPairs) EphemerisTransit
            <*> L.foldMap (mapLunarPhases' >>> tallyLunarPhases) id
    zodiacs = westernZodiacSigns
    chosenPairs =
      filteredPairs
        uniquePairs
        (tail defaultPlanets) -- everyone but the Moon
        defaultPlanets

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
natalAlmanac :: GeographicPosition -> UTCTime -> UTCTime -> UTCTime -> Stream (Of (Ephemeris Double)) IO () -> IO Calendar
natalAlmanac geo birth start end ephe = do
  -- TODO: these dates are okay, but the dates in @ephe@ are off due to the oversimplification
  -- done in `julianDayRange` -- ideally we obtain ephemeris for the /exact/ UTC range, 
  -- vs. the off-by-one range obtained by @julianDayRange@'s abuse of @gregorianToFake...@
  Just startTT <- toJulianDay start
  Just endTT   <- toJulianDay end
  Just birthTT  <- toJulianDay birth
  Just birthUT1 <- toJulianDay birth

  natalEphe' <- readEphemerisEasy True birthTT
  case natalEphe' of
    Left e -> fail e
    Right natalEphe -> do
      CuspsCalculation{houseCusps} <- calculateCusps Placidus birthUT1 geo
      let houses = zipWith House [I .. XII] houseCusps
      (cross, trns, cuspTrns) :> _ <-
        ephe
        & ephemerisWindows 2
        & L.purely S.fold (mkAlmanac natalEphe houses)

      lun <- selectLunarTransits startTT endTT natalEphe
      let talliedEcl = asCalendar EphemerisTransit lun

      asCal <- toCalendar $ cross <> trns <> cuspTrns
      asCalLun <- toCalendar talliedEcl
      pure $ asCal <> asCalLun

  where
    mkAlmanac n houses =
      (,,) <$> foldEachDay (mapCrossings houses)   EphemerisHouseCrossing
           <*> foldEachDay (mapNatalTransits n chosenPairs) EphemerisTransit
           <*> foldEachDay (mapCuspTransits  (filterHouses houses) sansMoon) EphemerisHouseTransit
    filterHouses houses =
      houses & filter (houseName >>> (`elem` [I, X]))
    chosenPairs =
      filteredPairs
        allPairs
        (tail defaultPlanets)
        defaultPlanets
    sansMoon = filter (Moon /=) defaultPlanets


-- | Given a function to obtain an aggregate of events, produce a Fold
-- that will populate a "calendar" with an entry for every day each
-- event happens.
foldEachDay
  :: (Ord (TemporalIndex a1), Temporal a1, Foldable t)
  => (a2 -> Aggregate x (t a1))
  -> ((x, a1) -> b)
  -> L.Fold a2 (Aggregate (TemporalIndex a1) [b])
foldEachDay f g =
  L.foldMap (f >>> asCalendar g) id

-- | Given a function to obtain an aggregate of events, produce a Fold
-- that will populate a "calendar" with entries only when the event
-- starts and/or ends.
foldByStartEnd
  :: (Semigroup (t a1), Temporal a1, Ord x, Ord (TemporalIndex a1), Foldable t)
  => (a2 -> Aggregate x (t a1))
  -> ((x, a1) -> b)
  -> L.Fold a2 (Aggregate (TemporalIndex a1) [b])
foldByStartEnd f g=
  L.foldMap f (asCalendarSpan g)

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
    locateLunarPhase phase =
      tallyStart phase EphemerisLunarPhase phase

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

-- | Given an aggregate of temporal values, produce an aggregate of
-- said values indexed by their start time.
asCalendar
  :: (Temporal a, Ord (TemporalIndex a), Foldable t)
  => ((x,a) -> b)
  -> Aggregate x (t a)
  -> Aggregate (TemporalIndex a) [b]
asCalendar ins =
  getAggregate >>> M.toList >>> foldMap' calendarize
  where
    calendarize (x,a) =
      concatForEach (toList a) $ \el ->
        tallyStart el ins (x,el)

-- | Given an aggregate of temporal values, produce an aggregate of
-- said values indexed by their start and end time.
asCalendarSpan
  :: (Temporal a, Ord (TemporalIndex a), Foldable t)
  => ((x,a) -> b)
  -> Aggregate x (t a)
  -> Aggregate (TemporalIndex a) [b]
asCalendarSpan ins =
  getAggregate >>> M.toList >>> foldMap' calendarize
  where
    calendarize (x,a) =
      concatForEach (toList a) $ \el ->
        tallyStart el ins (x,el) <> tallyEnd el ins (x,el)

tallyStart
  :: (Ord (TemporalIndex a1), Temporal a1)
  => a1
  -> (t -> a2)
  -> t
  -> Aggregate (TemporalIndex a1) [a2]
tallyStart el f el' =
  Aggregate . M.fromList $ [(startTime el, [f el'])]

tallyEnd
  :: (Ord (TemporalIndex a1), Temporal a1)
  => a1
  -> (t -> a2)
  -> t
  -> Aggregate (TemporalIndex a1) [a2]
tallyEnd el f el' =
  Aggregate . M.fromList $ [(startTime el, [f el'])]
