{-# LANGUAGE NamedFieldPuns #-}
module Query.Example where

import Query.EventTypes
import Query.Transit
import Query.Crossing
import Query.Retrograde

import qualified Control.Foldl as L
import qualified Streaming.Prelude as S
import Data.Function
import Streaming (Of ((:>)), Stream)
import Query.Streaming
import Data.Time
import SwissEphemeris
import Query.Aggregate
import Control.Category ((>>>))
import Query.LunarPhase
import Query.Eclipse
import qualified Data.Sequence as Sq
import qualified Data.Foldable as F
import SwissEphemeris.Precalculated (readEphemerisEasy, Ephemeris)
import qualified Data.Map as M
import Data.Functor ((<&>))
import Data.Foldable (foldMap')
import Data.Bifunctor (first)
import Query.Event
import Data.Sequence (Seq)

-------------------------------------------------------------------------------
-- FUNCTIONS THAT AGGREGATE EVENTS
-------------------------------------------------------------------------------

worldAlmanac :: UTCTime -> UTCTime -> IO (Sq.Seq Event)
worldAlmanac start end = do
  Just ttStart <- toJulianDay start
  Just ttEnd   <- toJulianDay end
  -- TODO: expose the function that can produce both at the same time
  Just utStart <- toJulianDay start
  Just utEnd   <- toJulianDay end
  let ephe = streamEpheJDF ttStart ttEnd
  (retro, cross, slowTransits, lun) :> _ <-
    ephe
    & ephemerisWindows 2
    & L.purely S.fold mkAlmanac

  ecl <- allEclipses utStart utEnd
  let eclSq = Sq.fromList $ map EclipseMaximum ecl

  pure $ retro <> cross <> slowTransits <> lun <> eclSq
  where
    mkAlmanac =
      (,,,) <$> L.foldMap getRetrogrades collapse
            <*> L.foldMap (getZodiacCrossings (tail defaultPlanets) westernZodiacSigns) collapse
            <*> L.foldMap (getTransits slowPairs) collapse
            <*> L.foldMap mapLunarPhases' getMerged

chosenPairs :: [(Planet, Planet)]
chosenPairs =
  filteredPairs
    uniquePairs
    (tail defaultPlanets) -- everyone but the Moon
    defaultPlanets

slowPairs :: [(Planet, Planet)]
slowPairs =
  filteredPairs
    uniquePairs
    (drop 5 defaultPlanets) -- everyone but the Moon
    defaultPlanets


collapse :: Aggregate grouping (MergeSeq Event) -> Sq.Seq Event
collapse = getAggregate >>> F.fold >>> getMerged

natalAlmanac :: GeographicPosition -> UTCTime -> UTCTime -> UTCTime -> IO (Sq.Seq Event)
natalAlmanac geo birth start end = do
  Just startTT <- toJulianDay start
  Just endTT   <- toJulianDay end
  Just birthTT  <- toJulianDay birth
  Just birthUT1 <- toJulianDay birth

  let ephe = streamEpheJDF startTT endTT
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

      -- TODO: also want lunar transits for house cusps
      lun <- selectLunarTransits startTT endTT natalEphe

      pure $ cross <> trns <> cuspTrns <> collapse lun
  where
    mkAlmanac n houses =
      (,,) <$> L.foldMap (getHouseCrossings defaultPlanets houses) collapse
           <*> L.foldMap (getNatalTransits n chosenPairs) collapse
           <*> L.foldMap (getCuspTransits  (filterHouses houses) sansMoon) collapse
    filterHouses houses =
      houses & filter (houseName >>> (`elem` [I, X]))
    sansMoon = filter (Moon /=) defaultPlanets

-------------------------------------------------------------------------------
-- INDEXING UTILITIES 
-------------------------------------------------------------------------------

type EventExactDates = (Event, [UTCTime])

eventDates :: TimeZone -> Event -> IO [(ZonedTime, Sq.Seq EventExactDates)]
eventDates tz evt = do
  exactsUTC <- eventExactAt evt
  startsUTC <- eventStartsAt evt
  let uniqTimes =
        if null exactsUTC then
          [startsUTC]
        else
          exactsUTC
  pure $ zip (map (utcToZonedTime tz) uniqTimes) (repeat $ Sq.singleton (evt,exactsUTC))

-- | Given a timezone and a sequence of events, index them by day (in the given timezone,)
-- with entries for the event's start, moments of exactitude, and end.
indexByDay :: TimeZone -> Sq.Seq Event -> IO (M.Map Day (Sq.Seq EventExactDates))
indexByDay tz events =
  foldMap' (eventDates tz) events
    <&> map (first getDay)
    <&> M.fromListWith (<>)
  where
    getDay (ZonedTime (LocalTime d _tod) _tz) = d

findEclipses :: UTCTime -> UTCTime -> IO (Seq Event)
findEclipses start end = do
  (utStart, utEnd) <- julianDaysUT (start, end)
  ecl <- allEclipses utStart utEnd
  pure $ Sq.fromList $ map EclipseMaximum ecl

findLunarPhases :: UTCTime -> UTCTime -> IO (Seq Event)
findLunarPhases start end = do
  ephe <- ephemeris start end
  phases :> _ <-
    ephe & S.foldMap mapLunarPhases'
  pure (getMerged phases)

findMundaneTransits :: UTCTime -> UTCTime -> IO (Seq Event)
findMundaneTransits start end = do
  ephe <- ephemeris start end
  transits :> _ <-
    ephe & S.foldMap (getTransits chosenPairs)
  pure $ collapse transits

findCrossings :: UTCTime -> UTCTime -> IO (Seq Event)
findCrossings start end = do
  ephe <- ephemeris start end
  crossings :> _ <-
    ephe & S.foldMap (getZodiacCrossings sansMoon westernZodiacSigns)
  pure $ collapse crossings
  where
    sansMoon = tail defaultPlanets

findRetrogrades :: UTCTime -> UTCTime -> IO (Seq Event)
findRetrogrades start end = do
  ephe <- ephemeris start end
  retro :> _ <-
    ephe & S.foldMap getRetrogrades
  pure $ collapse retro

findNatalTransits :: GeographicPosition -> ZonedTime -> UTCTime -> UTCTime -> IO (Seq Event)
findNatalTransits = error "not implemented"

findLunarTransits :: UTCTime -> UTCTime -> IO (Seq Event)
findLunarTransits = error "not implemented"

ephemeris :: UTCTime -> UTCTime -> IO (Stream (Of (Seq (Ephemeris Double))) IO ())
ephemeris start end = do
  (startTT, endTT) <- julianDays (start, end)
  pure $ streamEpheJDF startTT endTT & ephemerisWindows 2

julianDays :: (UTCTime, UTCTime) -> IO (JulianDayTT, JulianDayTT)
julianDays (a, b)= do
  Just att <- toJulianDay a
  Just btt <- toJulianDay b
  pure (att, btt)

julianDaysUT :: (UTCTime, UTCTime) -> IO (JulianDayUT1, JulianDayUT1)
julianDaysUT (a, b)= do
  Just att <- toJulianDay a
  Just btt <- toJulianDay b
  pure (att, btt)
