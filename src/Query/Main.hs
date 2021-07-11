{-# LANGUAGE NamedFieldPuns #-}
module Query.Main where

import Data.Time
import SwissEphemeris
import Data.Traversable (for)
import SwissEphemeris.Precalculated
import Query.Retrograde
import Options.Applicative
import OptionParser (dayReader)
import qualified Data.Map as M
import Control.Monad (forM_)
data Options = Options
  { optRangeStart :: !Day
  , optRangeEnd :: !Day
  }

main :: Options -> IO ()
main Options{optRangeStart, optRangeEnd} = do
  let days = julianDayRange optRangeStart optRangeEnd
  ephe <- for days (readEphemerisEasy False)
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
