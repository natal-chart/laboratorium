{-# LANGUAGE NamedFieldPuns #-}
module Query.Main where

import Data.Time
import SwissEphemeris
import SwissEphemeris.Precalculated
import Query.Retrograde
import Query.Common ( Station(Retrograde, Direct) )
import Options.Applicative
import OptionParser (dayReader)
import qualified Data.Map as M
import Control.Monad (forM_)
import Text.Read (readMaybe)
import Query.Crossing
import Query.Transit
import Query.Streaming (streamEpheJDF)
import Query.Aggregate
import Data.Function
import Streaming (Stream, Of, Of((:>)))
import Query.PlanetEphe
import Data.Ord (Down(..))
import Data.List (sortOn)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Foldable (toList)
import Data.Either (rights)
import Query.LunarPhase
import Query.Eclipse
import Query.Almanac

data QueryType
  = Retrogrades
  | Crossings
  | Transits
  | Averages
  | LunarTransits
  | NatalTransits
  | LunarPhases
  | Eclipses
  | WorldAlmanac
  | NatalAlmanac
  deriving (Show, Read)

data Options = Options
  { optRangeStart :: !Day
  , optRangeEnd :: !Day
  , query :: QueryType
  }

type IntervalEphemeris = [Either String (Ephemeris Double)]
type EphemerisStream = Stream (Of (Ephemeris Double)) IO ()

main :: Options -> IO ()
main opts@Options{optRangeStart, optRangeEnd, query} = do
  Just tt1 <- toJulianDay (UTCTime optRangeStart 0)
  Just tt2 <- toJulianDay (UTCTime optRangeEnd 0)
  let epheStream = streamEpheJDF tt1 tt2
  case query of
    Retrogrades -> doRetrogrades epheStream
    Crossings -> doCrossings epheStream
    Transits -> doTransits epheStream
    LunarTransits -> doLunarTransits opts
    NatalTransits -> doNatalTransits opts epheStream
    Averages -> doPlanetAverages epheStream
    LunarPhases -> doLunarPhases epheStream
    Eclipses -> doEclipses opts
    WorldAlmanac -> doWorldAlmanac opts epheStream
    NatalAlmanac -> doNatalAlmanac opts epheStream

doNatalAlmanac :: Options -> Stream (Of (Ephemeris Double)) IO () -> IO ()
doNatalAlmanac Options{optRangeStart, optRangeEnd} ephe = do
  bdUT <- iso8601ParseM "1989-01-06T23:30:00-06:00" :: IO ZonedTime
  let place = GeographicPosition {geoLat = 14.0839053, geoLng = -87.2750137}
  almanac <- ephe & natalAlmanac place (zonedTimeToUTC bdUT) (UTCTime optRangeStart 0) (UTCTime optRangeEnd 0)
  forM_ (almanac & getAggregate & M.toAscList) $ \(day, events) -> do
    print day
    putStrLn "============="
    forM_ events $ \event -> do
      print event
      exc <- exactEvent event
      case exc of
        Left _ -> pure ()
        Right exct -> do
          exactUT <- fromJulianDay exct :: IO UTCTime
          print ("\tExact at:", exactUT)
 
doWorldAlmanac :: Options -> Stream (Of (Ephemeris Double)) IO () -> IO ()
doWorldAlmanac Options{optRangeStart, optRangeEnd} ephe = do
  almanac <- ephe & worldAlmanac (UTCTime optRangeStart 0) (UTCTime optRangeEnd 0)
  forM_ (almanac & getAggregate & M.toAscList) $ \(day, events) -> do
    print day
    putStrLn "============="
    forM_ events $ \event -> do
      print event
      exc <- exactEvent event
      case exc of
        Left _ -> pure ()
        Right exct -> do
          exactUT <- fromJulianDay exct :: IO UTCTime
          print ("Exact at:", exactUT)
         


doEclipses :: Options -> IO ()
doEclipses Options{optRangeStart, optRangeEnd} = do
  Just start <- toJulianDay (UTCTime optRangeStart 0) :: IO (Maybe JulianDayUT1)
  Just end   <- toJulianDay (UTCTime optRangeEnd 0) :: IO (Maybe JulianDayUT1)
  eclipses <- allEclipses start end
  forM_ eclipses $ \ecl -> do
    case ecl of
      SolarEclipse t d -> do
        exactAt <- fromJulianDay d :: IO UTCTime
        print ("Solar Eclipse: ", t, exactAt)
      LunarEclipse t d -> do
        exactAt <- fromJulianDay d :: IO UTCTime
        print ("Lunar Eclipse: ", t, exactAt)


doLunarPhases :: Stream (Of (Ephemeris Double)) IO () -> IO ()
doLunarPhases ephe = do
  phases :> _ <- ephe & lunarPhases
  forM_ (getMerged phases) $ \LunarPhase{lunarPhaseName, lunarPhaseStarts, lunarPhaseEnds} -> do
    print (lunarPhaseName, dayFromJulianDay lunarPhaseStarts, dayFromJulianDay lunarPhaseEnds)
    putStrLn "----------------------"

doNatalTransits :: Options -> Stream (Of (Ephemeris Double)) IO () -> IO ()
doNatalTransits _opts ephe = do
  bdUT <- iso8601ParseM "1989-01-06T23:30:00-06:00" :: IO ZonedTime
  Just julian <- toJulianDay $ zonedTimeToUTC bdUT
  Right natalEphe <- readEphemerisEasy False julian
  allTransits :> _ <- ephe & selectNatalTransits natalEphe allPairs
  forM_ (M.toAscList (getAggregate allTransits)) $ \(bodies@(transiting, _transited), transits) -> do
    print bodies
    putStrLn "-----------"
    forM_ (getMerged transits) $ \Transit{aspect,transitOrb,transitStarts,transitEnds,transitCrosses, transitPhases} -> do
      startsUT <- fromJulianDay transitStarts :: IO UTCTime
      endsUT   <- fromJulianDay transitEnds   :: IO UTCTime
      print (startsUT, endsUT, aspect, transitOrb, transitCrosses)
      exactAt <- mapM (exactCrossing transiting transitCrosses) (getMerged transitPhases)
      let exactCrossings = rights . toList $ exactAt
      if not . null $ exactCrossings then
        forM_ exactCrossings $ \exact -> do
          exactUT <- fromJulianDay exact :: IO UTCTime
          print ("Exact at", exactUT)
      else
        pure ()


doLunarTransits :: Options -> IO ()
doLunarTransits Options{optRangeStart, optRangeEnd} = do
  bdUT <- iso8601ParseM "1989-01-06T23:30:00-06:00" :: IO ZonedTime
  Just julian <- toJulianDay $ zonedTimeToUTC bdUT
  natalEphe <- readEphemerisEasy False julian
  case natalEphe of
    Left _ -> fail "fuck"
    Right ne -> do
      lunarTransits <-
        selectLunarTransits
          (julianMidnight $ dayToJulianDay optRangeStart)
          (julianMidnight $ dayToJulianDay optRangeEnd)
          ne
      forM_ (M.toAscList (getAggregate lunarTransits)) $ \(bodies@(_daMoon, _transited), transits) -> do
        putStrLn "\n\n"
        print bodies
        putStrLn "================="
        forM_ (getMerged transits) $ \Transit{aspect, transitIsExact, transitStarts, transitEnds} -> do
          let startsDay = dayFromJulianDay transitStarts
              endsDay = dayFromJulianDay transitEnds
          case transitIsExact of
            Nothing -> print (aspect, startsDay, endsDay)
            Just e -> do
              exact <- fromJulianDay e :: IO UTCTime
              print (aspect, exact, startsDay, endsDay)

doPlanetAverages :: EphemerisStream -> IO ()
doPlanetAverages  ephe = do
  avgs <- planetAverageSpeeds defaultPlanets ephe
  print (sortOn (Down . snd) $ M.toList $ getAverages avgs)


doRetrogrades :: EphemerisStream -> IO ()
doRetrogrades ephe = do
  retro :> _ <- ephe & retrogrades
  -- retrogrades for 2020: https://www.findyourfate.com/astrology/year2020/2020-planetretrogrades.html
  forM_ (M.toAscList (getAggregate retro)) $ \(planet, stations) -> do
    print planet
    putStrLn "-----------"
    forM_ (getMerged stations) $ \PlanetStation{stationStarts, stationEnds, stationType} -> do
      startsUT <- fromJulianDay stationStarts :: IO UTCTime
      endsUT <- fromJulianDay stationEnds :: IO UTCTime
      let interval =
            if stationType `elem` [Query.Common.Direct, Query.Common.Retrograde] then
              show startsUT
            else
              show startsUT <> " - " <> show endsUT
      putStrLn $ show stationType <> " ( " <> interval <> ")"

doCrossings:: EphemerisStream -> IO ()
doCrossings ephe = do
  let zodiacs = take 12 $ iterate (+ 30) 0
      signs = zipWith Zodiac [Aries .. Pisces] zodiacs
  cross :> _ <- ephe & crossings signs
  -- crossings for 2020:
  -- https://cafeastrology.com/astrology-of-2020.html
  forM_ (M.toAscList (getAggregate cross)) $ \(planet, crossings') -> do
    putStrLn ""
    print planet
    putStrLn "-----------"
    forM_ (getMerged crossings') $ \Crossing{crossingEnters, crossingExits, crossingSubject} -> do
      startsUT <- fromJulianDay crossingEnters :: (IO UTCTime)
      endsUT <- fromJulianDay  crossingExits :: (IO UTCTime)
      let interval =
            if startsUT == endsUT then
              "starts: " <>  show startsUT
            else
              show startsUT <> " - " <> show endsUT
      putStrLn $ "In " <> show (signName crossingSubject) <> " ( " <> interval <> ")"

doTransits :: EphemerisStream -> IO ()
doTransits ephe = do
  allTransits :> _ <- ephe & interplanetaryTransits
  forM_ (M.toAscList (getAggregate allTransits)) $ \(bodies@(transiting, _transited), transits) -> do
    print bodies
    putStrLn "-----------"
    forM_ (getMerged transits) $ \Transit{aspect,lastPhase,transitOrb,transitStarts,transitEnds,transitCrosses, transitPhases} -> do
      startsUT <- fromJulianDay transitStarts :: IO UTCTime
      endsUT   <- fromJulianDay transitEnds   :: IO UTCTime
      print (startsUT, endsUT, aspect, lastPhase, transitOrb)
      exactAt <- mapM (exactCrossing transiting transitCrosses) (getMerged transitPhases)
      let exactCrossings = rights . toList $ exactAt
      if not . null $ exactCrossings then
        forM_ exactCrossings $ \exact -> do
          exactUT <- fromJulianDay exact :: IO UTCTime
          print ("Exact at", exactUT)
      else
        pure ()


-- | Get all days in the given range, as @JulianDayTT@s
julianDayRange :: Day -> Day -> [JulianDayTT]
julianDayRange startDay endDay =
  [start .. end]
  where
    (startY, startM, startD) = toGregorian startDay
    (endY, endM, endD) = toGregorian endDay
    start = gregorianToFakeJulianDayTT startY startM startD 0
    end = gregorianToFakeJulianDayTT endY endM endD 0

mainOptions :: Parser Options
mainOptions =
  Options
    <$> option dayReader (long "start" <> short 's')
    <*> option dayReader (long "end" <> short 'e')
    <*> option queryTypeReader (long "query" <> short 'q')

queryTypeReader :: ReadM QueryType
queryTypeReader = eitherReader $ \arg ->
  case readMaybe arg of
    Nothing -> Left "Invalid query"
    Just q -> Right q
