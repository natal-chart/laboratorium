{-# LANGUAGE NamedFieldPuns #-}
module PrecalculatedEphemeris where

import Data.Time
import Options.Applicative
import SwissEphemeris.Precalculated
import Text.Read (readMaybe)
import SwissEphemeris
import Control.Monad (forM_)
import OptionParser (datetimeReader)

newtype Options = Options SubCommand

data SubCommand
  = Generate EphemerisBlockNumber Int
  | Query UTCTime Bool


main :: Options -> IO ()
main (Options subcommand) = do
  case subcommand of
    Generate block files -> generateEphemeris block files
    Query date withFallback -> queryEphemeris date withFallback

generateEphemeris :: EphemerisBlockNumber -> Int -> IO ()
generateEphemeris block fileCount = do
  forM_ (take fileCount [block..]) $ \currentBlock -> do
    generated <- writeEphemeris currentBlock
    case generated of
      Left err -> putStrLn $ "Error generating block: " <> show currentBlock <> err
      Right ebn -> putStrLn $ "Generated block: " <> show ebn

queryEphemeris :: UTCTime -> Bool -> IO ()
queryEphemeris queryDate allowFallback = do
  Just julian <- toJulianDay queryDate
  ephe <- readEphemerisEasy allowFallback julian
  case ephe of
    Left err -> putStrLn $ "Error querying date " <> show queryDate <> ": " <> err
    Right e@Ephemeris{epheEcliptic, epheNutation} -> do
      putStrLn $ "For JD (ET): " <> show (getJulianDay julian)
      putStrLn $ "Ecliptic: " <> showDegrees epheEcliptic
      putStrLn $ "Nutation: " <> showDegrees epheNutation
      forM_ ([Sun .. Pluto] <> [MeanNode, TrueNode, MeanApog, Chiron]) $ \currentPlanet -> do
        let position = currentPlanet `forPlanet` e
        case position of
          Nothing -> putStrLn $ show currentPlanet <> " N/A"
          Just EphemerisPosition{epheLongitude, epheSpeed} -> 
            putStrLn $ show currentPlanet 
              <> " pos: " <> showDegrees epheLongitude
              <> " spd: " <> showDegrees epheSpeed

      
showDegrees :: Double -> String
showDegrees deg =
  concat [
    show longitudeDegrees <> "° "
  , show longitudeMinutes <> "' "
  , show longitudeSeconds <> "\""
  ]
  where
    LongitudeComponents {longitudeDegrees, longitudeMinutes, longitudeSeconds} =
      splitDegrees (defaultSplitDegreesOptions <> [RoundSeconds]) deg


blockNumberReader :: ReadM EphemerisBlockNumber
blockNumberReader = eitherReader $ \arg ->
  let max' = extractEphemerisBlockNumber maxBound
      min' = extractEphemerisBlockNumber minBound
  in case readMaybe arg >>= mkEphemerisBlockNumber of
    Nothing -> Left $ "Invalid block number: " <> arg <> " (must be between " <> show min' <> " and " <> show max' <> " )"
    Just bn -> Right bn

generateParser :: Parser SubCommand
generateParser =
  Generate
    <$> option blockNumberReader (long "starting-file" <> short 'f')
    <*> option auto (long "number-of-files" <> short 'n')

queryParser :: Parser SubCommand
queryParser =
  Query
    <$> option datetimeReader (long "date" <> short 'd')
    <*> option auto (long "allow-fallback" <> short 'a')

generateCommand :: Mod CommandFields SubCommand
generateCommand =
  command "generate" (info generateParser (progDesc "Generate precalculated ephemeris files"))

queryCommand :: Mod CommandFields SubCommand
queryCommand =
  command "query"  (info queryParser (progDesc "Query an existing precalculated ephemeris file"))

mainOptions :: Parser Options
mainOptions =
  Options
    <$> hsubparser (generateCommand <> queryCommand)

optsParser :: ParserInfo Options
optsParser =
  info
    (helper <*> mainOptions)
    (fullDesc <> progDesc "Generate or query precalculated ephe4-style ephemeris.")
