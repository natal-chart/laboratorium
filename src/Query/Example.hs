{-# LANGUAGE NamedFieldPuns #-}
module Query.Example where

import Query.EventTypes
import Query.Transit
import Query.Crossing
import Query.Retrograde

import qualified Control.Foldl as L
import qualified Streaming.Prelude as S
import Data.Function
import Streaming (Of ((:>)))
import Query.Streaming
import Data.Time
import SwissEphemeris
import Query.Aggregate
import Control.Category ((>>>))
import Query.LunarPhase
import Query.Eclipse
import qualified Data.Sequence as Sq
import qualified Data.Foldable as F
import SwissEphemeris.Precalculated (readEphemerisEasy)
import qualified Data.Map as M
import Data.Functor ((<&>))
import Data.Foldable (foldMap')
import Data.Bifunctor (first)
import Query.Event
import Data.List (nub)

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
  (retro, cross, trns, lun) :> _ <-
    ephe
    & ephemerisWindows 2
    & L.purely S.fold mkAlmanac

  ecl <- allEclipses utStart utEnd
  let eclSq = Sq.fromList $ map EclipseMaximum ecl

  pure $ retro <> cross <> trns <> lun <> eclSq
  where
    mkAlmanac =
      (,,,) <$> L.foldMap getRetrogrades collapse
            <*> L.foldMap (getZodiacCrossings defaultPlanets westernZodiacSigns) collapse
            <*> L.foldMap (getTransits  chosenPairs) collapse
            <*> L.foldMap mapLunarPhases' getMerged

chosenPairs :: [(Planet, Planet)]
chosenPairs =
  filteredPairs
    uniquePairs
    (tail defaultPlanets) -- everyone but the Moon
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

eventDates :: Event -> IO [(UTCTime, Sq.Seq EventExactDates)]
eventDates evt = do
  exacts <- eventExactAt evt
  starts <- eventStartsAt evt
  ends <- eventEndsAt evt
  let uniqTimes = nub $ [starts] <> exacts <> [ends]
  pure $ zip uniqTimes (repeat $ Sq.singleton (evt,exacts))

-- | Given a timezone and a sequence of events, index them by day (in the given timezone,)
-- with entries for the event's start, moments of exactitude, and end.
indexByDay :: TimeZone -> Sq.Seq Event -> IO (M.Map Day (Sq.Seq EventExactDates))
indexByDay tz events =
  foldMap' eventDates events
    <&> map (first $ getDay . utcToZonedTime tz)
    <&> M.fromListWith (<>)
  where
    getDay (ZonedTime (LocalTime d _tod) _tz) = d
