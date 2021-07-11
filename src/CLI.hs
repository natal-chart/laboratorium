{-# LANGUAGE NamedFieldPuns #-}
module CLI where

import qualified TransitCharts as TC
import qualified PrecalculatedEphemeris as PE
import qualified Query.Main as Q
import Options.Applicative
import System.Directory (makeAbsolute)
import SwissEphemeris.Precalculated (setEphe4Path)
import SwissEphemeris (withEphemerides)

main :: IO ()
main = do
  Options {ephePath, subcommand} <- execParser optsParser
  absoluteEphePath  <- makeAbsolute ephePath  
  setEphe4Path absoluteEphePath
  withEphemerides absoluteEphePath $
    case subcommand of
      Ephemeris opts -> PE.main opts
      Charts opts -> TC.main opts
      Query opts -> Q.main opts

data Options = Options
  { ephePath  :: FilePath
  , subcommand :: SubCommand
  }

data SubCommand
  = Ephemeris PE.Options
  | Charts TC.Options
  | Query Q.Options

mainOptions :: Parser Options
mainOptions =
  Options
    <$> strOption (long "ephe-path" <> help "location of ephemeris files (both data files and precalculated ephemeris)")
    <*> hsubparser (ephemerisCommand <> chartsCommand <> queryCommand)
  where
    ephemerisCommand = command "ephemeris" (info (Ephemeris <$> PE.mainOptions) (progDesc "Work with pre-calculated ephemeris"))
    chartsCommand = command "charts" (info (Charts <$> TC.mainOptions) (progDesc "Work with transit charts"))
    queryCommand = command "query" (info (Query <$> Q.mainOptions) (progDesc "Query events for an interval of time"))
    
optsParser :: ParserInfo Options
optsParser =
  info
    (helper <*> mainOptions)
    (fullDesc <> progDesc "Variegate tools for ephemerides")
