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
import Query.Streaming (streamEpheF)
import Query.Aggregate
import Data.Function
import Streaming (Stream, Of, Of((:>)))
import Query.PlanetEphe
import Data.Ord (Down(..))
import Data.List (sortOn)
import Data.Time.Format.ISO8601 (iso8601ParseM)

data QueryType
  = Retrogrades
  | Crossings
  | Transits
  | Averages
  | LunarTransits
  deriving (Show, Read)

data Options = Options
  { optRangeStart :: !Day
  , optRangeEnd :: !Day
  , query :: QueryType
  }

type IntervalEphemeris = [Either String (Ephemeris Double)]
type EphemerisStream = Stream (Of (Ephemeris Double)) IO ()

data Zodiac = Zodiac
  { signName :: ZodiacSignName, signLng :: Double}
  deriving (Eq, Show)

instance HasEclipticLongitude Zodiac where
  getEclipticLongitude (Zodiac _ l) = l


main :: Options -> IO ()
main opts@Options{optRangeStart, optRangeEnd, query} = do
  let epheStream = streamEpheF optRangeStart optRangeEnd
  case query of
    Retrogrades -> doRetrogrades epheStream
    Crossings -> doCrossings epheStream
    Transits -> doTransits epheStream
    LunarTransits -> doLunarTransits opts
    Averages -> doPlanetAverages epheStream

-- | Show all average speeds over a given period, descending

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
              show endsUT
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
      let startsUT = fromJulianDay <$> crossingEnters :: (Maybe (IO UTCTime))
          endsUT = fromJulianDay <$> crossingExits :: (Maybe (IO UTCTime))
      interval <-
        case (startsUT, endsUT) of
          (Nothing, Nothing) ->  pure ""
          (Just starts, Nothing) -> do
            ("starts: " <> ) . show <$> starts
          (Nothing, Just ends) -> do
            ("ends: " <> ) . show <$> ends
          (Just starts, Just ends) -> do
            starts' <- starts
            ends' <- ends
            pure $ show starts' <> " - " <> show ends'
      putStrLn $ "In " <> show (signName crossingSubject) <> " ( " <> interval <> ")"

doTransits :: EphemerisStream -> IO ()
doTransits ephe = do
  allTransits :> _ <- ephe & interplanetaryTransits
  forM_ (M.toAscList (getAggregate allTransits)) $ \(bodies@(_transiting, _transited), transits) -> do
    print bodies
    putStrLn "-----------"
    forM_ (getMerged transits) $ \Transit{aspect,lastPhase,transitOrb,transitStarts,transitEnds} -> do
      startsUT <- fromJulianDay transitStarts :: IO UTCTime
      endsUT   <- fromJulianDay transitEnds   :: IO UTCTime
      print (startsUT, endsUT, aspect, lastPhase, transitOrb)


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
