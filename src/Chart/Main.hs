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
import Query.Streaming (streamEpheF, ephemerisWindows)
import Query.PlanetEphe (planetEphemeris)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Foldable (traverse_)
import Chart.TransitOverview (transitChart)
import Text.Read (readMaybe)
import OptionParser
import Data.Function
import Streaming (Stream, Of)
import Query.Transit
import Chart.TransitProgress
import qualified Streaming.Prelude as St

deriving instance Read Planet

data QueryType
  = Overview
  | WorldProgress
  | NatalProgress
  deriving (Show, Read)

data Options = Options
  { optBirthday :: !UTCTime,
    optRangeStart :: !Day,
    optRangeEnd :: !Day,
    optTransitedPlanets :: ![Planet],
    optTransitingPlanets :: ![Planet],
    optQueryType :: !QueryType,
    optDebug :: !Bool
  }

main :: Options -> IO ()
main opts@Options{optRangeStart, optRangeEnd, optQueryType} = do
  let epheStream = streamEpheF optRangeStart optRangeEnd
  case optQueryType of
    Overview -> doOverview opts epheStream
    WorldProgress -> doTransitProgress opts epheStream
    NatalProgress -> doNatalTransitProgress opts epheStream

doNatalTransitProgress :: Options -> Stream (Of (Ephemeris Double)) IO () -> IO ()
doNatalTransitProgress Options{optBirthday, optTransitedPlanets, optTransitingPlanets, optDebug} ephe =  do
  Just julian <- toJulianDay optBirthday
  transitedEphe <- readEphemerisEasy False julian
  case transitedEphe of
    Left err -> fail err
    Right transited -> do
      let chosenPairs = filteredPairs allPairs optTransitingPlanets optTransitedPlanets
      chosenTransits S.:> _ <-
          ephe & ephemerisWindows 2 & St.foldMap (getNatalTransits transited chosenPairs)
      transitProgressChart chosenTransits optDebug

doTransitProgress :: Options -> Stream (Of (Ephemeris Double)) IO () -> IO ()
doTransitProgress Options{optTransitedPlanets, optTransitingPlanets, optDebug} ephe = do
  let chosenPairs = filteredPairs uniquePairs optTransitingPlanets optTransitedPlanets
  chosenTransits S.:> _ <- 
    ephe & ephemerisWindows 2 & St.foldMap (getTransits chosenPairs)
  transitProgressChart chosenTransits optDebug

doOverview :: Options -> Stream (Of (Ephemeris Double)) IO () -> IO ()
doOverview Options{optBirthday, optRangeStart, optRangeEnd, optTransitedPlanets, optTransitingPlanets} ephe = do
  let days = julianDays optRangeStart optRangeEnd
      natalPlanets = optTransitedPlanets

  Just julian <- toJulianDay optBirthday
  utcDays <- sequence $ fromJulianDay <$> days
  let allDays = zip days utcDays
  transitedEphe <- readEphemerisEasy False julian
  transitingEphe S.:> _ <- ephe & planetEphemeris optTransitingPlanets

  case transitedEphe of
    Left err -> fail err
    Right trEphe -> do
      let transited = fromMaybe [] $ traverse (forPlanet trEphe) natalPlanets
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
    <*> option (allPlanets <|> planetListReader) (long "transited" <> help "space-separated list of transited planets")
    <*> option (allPlanets <|> planetListReader) (long "transiting" <> help "space-separated list of transiting planets")
    <*> option chartTypeReader (long "chart" <> short 'c')
    <*> switch (long "debug" <> help "print debug information")

allPlanets :: ReadM [Planet]
allPlanets = eitherReader $ \arg ->
  if arg == "All" then
    Right defaultPlanets
  else
    Left "invalid option"

chartTypeReader :: ReadM QueryType
chartTypeReader = eitherReader $ \arg ->
  case readMaybe arg of
    Nothing -> Left "Invalid chart type"
    Just q -> Right q
