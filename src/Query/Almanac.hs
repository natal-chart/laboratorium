{-# LANGUAGE NamedFieldPuns #-}
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
import Data.List (nubBy)

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
    "Crossing: " <> show' pl <> show crossingSubject
  show (EphemerisTransit ((p1, p2), Transit{aspect, lastPhase, transitOrb})) =
    "Transit: " <> show' p1 <> show' aspect <> show' p2 <> show' lastPhase <> show transitOrb
  show (EphemerisLunarPhase LunarPhase{lunarPhaseName}) =
    "Moon Phase: " <> show lunarPhaseName
  show (EphemerisEclipse ecl) =
    "Eclipse: " <> show ecl

type Calendar = Aggregate Day [EphemerisEvent]

-- | Given a range of time, produce a map of days and events happening each day;
-- useful for displaying a calendar where one simply wants to list what happens
-- each day, without regard for "merging" things together. See the individual
-- @select@ functions for that.
worldAlmanac :: UTCTime -> UTCTime -> Stream (Of (Ephemeris Double)) IO () -> IO Calendar
worldAlmanac start end ephe = do
  (retro, cross, trns, lun) :> _ <-
    ephe
    & ephemerisWindows 2
    & L.impurely S.foldM mkAlmanac
  Just startUT1 <- toJulianDay start
  Just endUT1   <- toJulianDay end
  ecl <- allEclipses startUT1 endUT1
  talliedEcl <- tallyEclipses ecl
  pure $ retro <> cross <> trns <> lun <> talliedEcl
  where
    mkAlmanac =
      (,,,) <$> L.sink (mapRetrogrades >>> tallyRetrogrades)
            <*> L.sink (mapCrossings zodiacs >>> tallyCrossings)
            <*> L.sink (mapTransits' chosenPairs >>> tallyTransits)
            <*> L.sink (mapLunarPhases' >>> tallyLunarPhases)
    zodiacs = westernZodiacSigns
    chosenPairs =
      filteredPairs
        uniquePairs
        (tail defaultPlanets) -- everyone but the Moon
        defaultPlanets

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
      calendarAggregate [
        (dateUT, [EphemerisEclipse ecl])
        ]


tallyLunarPhases :: MergeSeq LunarPhase -> IO Calendar
tallyLunarPhases =
  getMerged >>> toList >>> foldMap' locateLunarPhase
  where
    locateLunarPhase phase@LunarPhase{lunarPhaseStarts} = do
      startUT <- dayFromJDTT lunarPhaseStarts
      calendarAggregate [
        (startUT, [EphemerisLunarPhase phase])
        ]

tallyTransits :: TransitMap -> IO Calendar
tallyTransits =
  getAggregate >>> M.toList >>> foldMap' locateTransit
  where
    locateTransit (planets, transits') =
      concatForEach (toList transits') $ \tr@Transit{transitStarts} -> do
        startUT <- dayFromJDTT transitStarts
        calendarAggregate [
          (startUT, [EphemerisTransit (planets, tr)])
          ]

tallyCrossings :: CrossingMap Zodiac -> IO Calendar
tallyCrossings =
  getAggregate >>> M.toList >>> foldMap' locateCrossing
  where
    locateCrossing (planet, crossings') =
      concatForEach (toList crossings') $ \cr@Crossing{crossingEnters} -> do
        sq <- sequence [start crossingEnters cr]
        calendarAggregate . mconcat $ sq
      where
        start :: Maybe JulianDayTT -> Crossing Zodiac -> IO [(Day, [EphemerisEvent])]
        start Nothing _ = mempty
        start (Just jd) cr = do
          (UTCTime startUT _) <- fromJulianDay jd :: IO UTCTime
          pure [(startUT, [EphemerisZodiacCrossing (planet, cr)])]
        
tallyRetrogrades :: RetrogradeMap -> IO Calendar
tallyRetrogrades =
  getAggregate >>> M.toList >>> foldMap' locateRetrograde
  where
    locateRetrograde (planet, rs) =
      concatForEach (toList rs) $ \st@PlanetStation{stationStarts} -> do
        (UTCTime startUT _) <- fromJulianDay stationStarts :: IO UTCTime
        calendarAggregate [
          (startUT, [EphemerisRetrograde (planet, st)])
          ]

calendarAggregate :: [(Day, [EphemerisEvent])] -> IO (Aggregate Day [EphemerisEvent])
calendarAggregate = pure . Aggregate . M.fromList . uniqueDates

dayFromJDTT :: JulianDayTT -> IO Day
dayFromJDTT jd = do
  (UTCTime ut _) <- fromJulianDay jd :: IO UTCTime
  pure ut

dayFromJDUT :: JulianDayUT1 -> IO Day
dayFromJDUT jd = do
  (UTCTime ut _) <- fromJulianDay jd :: IO UTCTime
  pure ut

uniqueDates :: [(Day, [EphemerisEvent])] -> [(Day, [EphemerisEvent])]
uniqueDates = nubBy (\x y -> fst x == fst y)
