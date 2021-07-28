{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE NamedFieldPuns #-}
module Chart.Main where
import Options.Applicative
import SwissEphemeris
import Data.Time
import qualified Streaming as S
import Util (julianDays)
import SwissEphemeris.Precalculated
import Query.Streaming (streamEpheF)
import Query.PlanetEphe (planetEphemeris)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Foldable (traverse_)
import Chart.TransitOverview (transitChart)
import Text.Read (readMaybe)
import OptionParser
import Data.Function
import Streaming (Stream, Of)

deriving instance Read Planet

data QueryType
  = Overview
  | Progress
  deriving (Show, Read)

data Options = Options
  { optBirthday :: !UTCTime,
    optRangeStart :: !Day,
    optRangeEnd :: !Day,
    optNatalPlanets :: [Planet],
    optQueryType :: !QueryType
  }

main :: Options -> IO ()
main opts@Options{optRangeStart, optRangeEnd, optQueryType} = do
  let epheStream = streamEpheF optRangeStart optRangeEnd
  case optQueryType of
    Overview -> doOverview opts epheStream
    Progress -> undefined

doOverview :: Options -> Stream (Of (Ephemeris Double)) IO () -> IO ()
doOverview Options{optBirthday, optRangeStart, optRangeEnd, optNatalPlanets} ephe = do
  let days = julianDays optRangeStart optRangeEnd
      natalPlanets = optNatalPlanets

  Just julian <- toJulianDay optBirthday
  utcDays <- sequence $ fromJulianDay <$> days
  let allDays = zip days utcDays
  transitedEphe <- readEphemerisEasy False julian
  transitingEphe S.:> _ <- ephe & planetEphemeris

  case transitedEphe of
    Left err -> fail err
    Right trEphe -> do
      let transited = fromMaybe [] $ traverse (`forPlanet` trEphe) natalPlanets
      traverse_ (transitChart allDays transitingEphe) transited

 

---
--- OPT UTILS
--
planetListReader :: ReadM [Planet]
planetListReader = eitherReader $ \arg ->
  case mapMaybe readMaybe (words arg) of
    [] -> Left "No planets could be parsed"
    ps -> Right ps

optsParser :: ParserInfo Options
optsParser =
  info
    (helper <*> mainOptions)
    (fullDesc <> progDesc "Plot transit charts for the given natal planets, in the given time range, for a specific birth/event date")

mainOptions :: Parser Options
mainOptions =
  Options
    <$> option datetimeReader (long "date" <> short 'd')
    <*> option dayReader (long "start" <> short 's')
    <*> option dayReader (long "end" <> short 'e')
    <*> option planetListReader (long "planets" <> short 'p' <> help "space-separated list of planets")
    <*> option chartTypeReader (long "chart" <> short 'c')

chartTypeReader :: ReadM QueryType
chartTypeReader = eitherReader $ \arg ->
  case readMaybe arg of
    Nothing -> Left "Invalid chart type"
    Just q -> Right q
