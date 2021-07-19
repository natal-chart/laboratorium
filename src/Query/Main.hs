{-# LANGUAGE NamedFieldPuns #-}
module Query.Main where

import Data.Time
import SwissEphemeris
import Data.Traversable (for)
import SwissEphemeris.Precalculated
import Query.Retrograde
    ( RetrogradeMap(getRetrogradeMap),
      PlanetStation(PlanetStation, stationStarts, stationEnds,
                    stationType),
      PlanetStationSeq(getStations),
      windows,
      foldRetrograde )
import Query.Common ( Station(Retrograde, Direct) )
import Options.Applicative
import OptionParser (dayReader)
import qualified Data.Map as M
import Control.Monad (forM_)
import Text.Read (readMaybe)
import Query.Crossing (foldCrossings, CrossingMap (getCrossingMap), CrossingSeq (getCrossings), Crossing(..))

data QueryType
  = Retrogrades
  | Crossings
  deriving (Show, Read)

data Options = Options
  { optRangeStart :: !Day
  , optRangeEnd :: !Day
  , query :: QueryType
  }

type IntervalEphemeris = [Either String (Ephemeris Double)]

data Zodiac = Zodiac 
  { signName :: ZodiacSignName, signLng :: Double}
  deriving (Eq, Show)
  
instance HasEclipticLongitude Zodiac where
  getEclipticLongitude (Zodiac _ l) = l


main :: Options -> IO ()
main Options{optRangeStart, optRangeEnd, query} = do
  let days = julianDayRange optRangeStart optRangeEnd
  ephe <- for days (readEphemerisEasy False)
  case query of
    Retrogrades -> doRetrogrades ephe
    Crossings -> doCrossings ephe


doRetrogrades :: IntervalEphemeris -> IO ()
doRetrogrades ephe = do
  let retro = foldRetrograde (windows 2 ephe)
  -- retrogrades for 2020: https://www.findyourfate.com/astrology/year2020/2020-planetretrogrades.html
  forM_ (M.toAscList (getRetrogradeMap retro)) $ \(planet, stations) -> do
    print planet
    putStrLn "-----------"
    forM_ (getStations stations) $ \PlanetStation{stationStarts, stationEnds, stationType} -> do
      startsUT <- fromJulianDay stationStarts :: IO UTCTime
      endsUT <- fromJulianDay stationEnds :: IO UTCTime
      let interval =
            if stationType `elem` [Direct, Retrograde] then
              show endsUT
            else
              show startsUT <> " - " <> show endsUT
      putStrLn $ show stationType <> " ( " <> interval <> ")"

doCrossings:: IntervalEphemeris -> IO ()
doCrossings ephe = do
  let zodiacs = take 12 $ iterate (+ 30) 0
      signs = zipWith Zodiac [Aries .. Pisces] zodiacs
      cross = foldCrossings signs (windows 2 ephe)
  -- crossings for 2020:
  -- https://cafeastrology.com/astrology-of-2020.html
  forM_ (M.toAscList (getCrossingMap cross)) $ \(planet, crossings) -> do
    putStrLn ""
    print planet
    putStrLn "-----------"
    forM_ (getCrossings crossings) $ \Crossing{crossingEnters, crossingExits, crossingSubject} -> do
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
