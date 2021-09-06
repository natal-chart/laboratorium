{-# LANGUAGE NamedFieldPuns #-}
module Query.Main where

import Data.Time
import SwissEphemeris
import SwissEphemeris.Precalculated
import Options.Applicative
import OptionParser (dayReader)
import qualified Data.Map as M
import Control.Monad (forM_, unless)
import Text.Read (readMaybe)
import Streaming (Stream, Of) 
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Query.EventTypes
import Query.Example
import Query.Event

data QueryType
  = Retrogrades
  | Crossings
  | Transits
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
main Options{optRangeStart, optRangeEnd, query} = do
  let tz = read "EST" :: TimeZone
      start = zonedTimeToUTC $ ZonedTime (LocalTime optRangeStart midnight) tz
      end   = zonedTimeToUTC $ ZonedTime (LocalTime optRangeEnd midnight) tz
      place = GeographicPosition {geoLat = 14.0839053, geoLng = -87.2750137}
  bdUT <- iso8601ParseM "1989-01-06T23:30:00-06:00" :: IO ZonedTime
  events <-
    case query of
      Retrogrades -> findRetrogrades start end
      Crossings -> findCrossings start end
      Transits -> findMundaneTransits start end
      LunarTransits -> findLunarTransits start end
      NatalTransits -> findNatalTransits place bdUT start end
      LunarPhases -> findLunarPhases start end
      Eclipses -> findEclipses start end
      WorldAlmanac -> worldAlmanac start end
      NatalAlmanac -> natalAlmanac place (zonedTimeToUTC bdUT) start end

  putStrLn $ "All events in " <> timeZoneName tz <>  " : "
  putStrLn "----------------- "
  indexedByDay <- indexByDay tz events
  forM_ (M.toAscList indexedByDay) $ \(day, evts) -> do
    print day
    putStrLn "============"
    mapM_ (printEvent tz) evts

printEvent :: TimeZone -> EventExactDates -> IO ()
printEvent tz (evt, exacts) = do
  printEventInfo evt tz

  starts <- eventStartsAt evt
  ends   <- eventEndsAt evt
  putStrLn "Starts at: "
  print $ utcToZonedTime tz starts
  putStrLn "Ends at: "
  print $ utcToZonedTime tz ends

  unless (null exacts) $ do
    putStrLn "Exact at:"
    forM_ exacts $ \date -> do
      putStrLn $ "    " <> show (utcToZonedTime tz date)

  putStrLn "############################"

printEventInfo :: Event -> TimeZone -> IO ()
printEventInfo (DirectionChange PlanetStation{stationType, stationPlanet}) _ =
  print (stationPlanet, "is", stationType) 
printEventInfo (LunarPhaseChange LunarPhase{lunarPhaseName}) _ =
  print lunarPhaseName
printEventInfo (EclipseMaximum ecl) _ =
  print ecl
printEventInfo (PlanetaryTransit tr) tz = printTransit tr tz
printEventInfo (HouseTransit tr) tz = printTransit tr tz
printEventInfo (ZodiacIngress Crossing{crossingPlanet, crossingCrosses, crossingDirection}) _ =
  print (crossingPlanet, "enters", signName crossingCrosses, crossingDirection)
printEventInfo (HouseIngress Crossing{crossingPlanet, crossingCrosses, crossingDirection}) _ =
  print (crossingPlanet, "enters house", crossingCrosses, crossingDirection)


printTransit :: Show a => Transit a -> TimeZone -> IO ()
printTransit Transit{aspect, transiting, transited, transitPhases} tz = do
  print (transiting, aspect, transited)
  forM_ transitPhases $ \TransitPhase{phaseName, phaseStarts, phaseEnds} -> do
    putStrLn $ "    " <> show phaseName
    phaseStartsUTC <- fromJulianDay phaseStarts 
    phaseEndsUTC <- fromJulianDay phaseEnds
    putStrLn $ "   Starts: " <> (show . utcToZonedTime tz $ phaseStartsUTC)
    putStrLn $ "   Ends: " <> (show . utcToZonedTime tz $ phaseEndsUTC)


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
