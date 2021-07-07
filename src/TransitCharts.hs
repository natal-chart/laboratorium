{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TransitCharts (main) where

import Data.Foldable (forM_, traverse_)
import Data.Maybe (mapMaybe, fromMaybe, fromJust, isJust)
import Data.Time
  ( Day,
    toGregorian,
    UTCTime,
  )
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import Graphics.Rendering.Chart.Easy hiding (days)
import Options.Applicative
  ( Parser,
    ParserInfo,
    ReadM,
    eitherReader,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    option,
    progDesc,
    short,
  )
import SwissEphemeris
  ( JulianDayTT,
    Planet (..),
    ZodiacSignName (..), ToJulianDay (toJulianDay), gregorianToFakeJulianDayTT, FromJulianDay (fromJulianDay)
  )
import Text.Read (readMaybe)
import SwissEphemeris.Precalculated (readEphemerisEasy, forPlanet, EphemerisPosition (EphemerisPosition, epheLongitude), Ephemeris(..))
import Control.Monad (forM, guard)


data Line = Solid | Dotted

deriving instance Read Planet

-- | Default planets to consider for transits.
defaultPlanets :: [Planet]
defaultPlanets = [Sun .. Pluto] <> [MeanNode, MeanApog, Chiron]

data Options = Options
  { optBirthday :: !UTCTime,
    optRangeStart :: !Day,
    optRangeEnd :: !Day,
    optNatalPlanets :: [Planet]
  }

main :: IO ()
main = do
  Options {optBirthday, optRangeStart, optRangeEnd, optNatalPlanets} <-
    execParser optsParser
  let days = julianDays optRangeStart optRangeEnd
      natalPlanets = optNatalPlanets
  Just julian <- toJulianDay optBirthday
  utcDays <- (sequence $ fromJulianDay <$> days) :: IO [UTCTime]
  let allDays = zip days utcDays
  transitedEphe <- readEphemerisEasy False julian
  transitingEphe <- forM allDays $ \(dtt, dut) -> do
    transit <- readEphemerisEasy False dtt
    pure (dut, either (const Nothing) Just transit)
  print $ length transitingEphe
  case transitedEphe of
    Left err -> fail err
    Right ephe -> do
      let transited = fromMaybe [] $ traverse (`forPlanet` ephe) natalPlanets
      traverse_ (transitChart allDays transitingEphe) transited

transitChart :: [(JulianDayTT, UTCTime)] -> [(UTCTime, Maybe (Ephemeris Double))] -> EphemerisPosition Double -> IO ()
transitChart transitRange transits natalEphe@(EphemerisPosition transited natalPos _spd) = do
  toFile def ("charts/" <> show transited <> "_transits.svg") $ do
    layoutlr_title .= "Transits to " <> show transited
    -- hide axis guidelines -- too much noise
    layoutlr_left_axis . laxis_override .= axisGridHide
    layoutlr_right_axis . laxis_override .= axisGridHide

    -- plot the aspect "bands"
    plotLeft $ aspectLine "Sextile" (opaque darkorange) $ sextiles natalEphe
    plotLeft $ aspectLine "Square" (opaque darkblue) $ squares natalEphe
    plotLeft $ aspectLine "Trine" (opaque darkgreen) $ trines natalEphe
    plotLeft $ aspectLine "Opposition" (opaque darkred) [opposition natalEphe]

    -- plot the zodiac bands
    let zodiacBands = take 12 $ iterate (bimap (+ 30) (+ 30)) (0, 30)
        alternatingColors = concat $ replicate 6 [lightgray, white]
    forM_ (zip3 [Aries .. Pisces] alternatingColors zodiacBands) $ \(sign, color, band) -> do
      plotRight (fbetween (show sign) color [(ut, band) | (_tt, ut) <- transitRange])

    -- plot the positions of all planets for all year
    forM_ defaultPlanets $ \transitingP -> do
      plotLeft $
        planetLine
          (show transitingP)
          (opaque $ planetColor transitingP)
          (planetLineStyle transitingP)
          (planetTransits transits transitingP)

    -- plot the original natal position line
    plotLeft $
      natalLine
        ("Natal " <> show transited)
        (opaque green)
        [natalPos]

planetTransits :: [(UTCTime, Maybe (Ephemeris Double))] -> Planet -> [(UTCTime, Double)]
planetTransits ephe planet = do
  (t, dayEphe) <- ephe
  guard $ isJust dayEphe
  let planetEphe = forPlanet planet =<< dayEphe
  guard $ isJust planetEphe
  pure (t, epheLongitude $ fromJust planetEphe)

fbetween ::
  String ->
  Colour Double ->
  [(UTCTime, (Double, Double))] ->
  EC l2 (PlotFillBetween UTCTime Double)
fbetween title color vals = liftEC $ do
  plot_fillbetween_style .= solidFillStyle (withOpacity color 0.4)
  plot_fillbetween_values .= vals
  plot_fillbetween_title .= title

planetLine ::
  String ->
  AlphaColour Double ->
  Line ->
  [(UTCTime, Double)] ->
  EC l2 (PlotLines UTCTime Double)
planetLine title color lineStyle values = liftEC $ do
  plot_lines_title .= title
  -- split the curve if a segment makes an abrupt jump in two consecutive days
  -- (no planet moves 100 degrees in a day, so we consider it spurious -- a "loop around")
  plot_lines_values .= groupWhen (\(_t1, x) (_t2, y) -> abs (y - x) <= 30) values
  plot_lines_style . line_color .= color
  plot_lines_style . line_dashes .= dashes lineStyle
  where
    dashes Solid = []
    dashes Dotted = [1.0, 1.0]

hLine ::
  [Double] ->
  String ->
  AlphaColour Double ->
  [Double] ->
  EC l2 (PlotLines x Double)
hLine dashesPattern title color ys = liftEC $ do
  plot_lines_title .= title
  -- inspired by the horizontal line helper:
  -- https://github.com/timbod7/haskell-chart/blob/8c5a823652ea1b4ec2adbced4a92a8161065ead6/chart/Graphics/Rendering/Chart/Plot/Lines.hs#L86-L92
  plot_lines_limit_values .= [[(LMin, LValue a), (LMax, LValue a)] | a <- ys]
  plot_lines_style . line_color .= color
  plot_lines_style . line_dashes .= dashesPattern

aspectLine,
  natalLine ::
    String ->
    AlphaColour Double ->
    [Double] ->
    EC l2 (PlotLines x Double)
aspectLine = hLine [2.0, 3.0]
natalLine = hLine [1.0, 1.0]

planetLineStyle :: Planet -> Line
planetLineStyle Moon = Dotted
planetLineStyle MeanNode = Dotted
planetLineStyle _ = Solid

-- | Somewhat characteristic colors for transiting planets.
-- more ideas at:
-- https://hackage.haskell.org/package/colour-2.3.5/docs/Data-Colour-Names.html
planetColor :: Planet -> Colour Double
planetColor Sun = orange
planetColor Moon = dimgray
planetColor Mercury = purple
planetColor Venus = salmon
planetColor Mars = red
planetColor Jupiter = darkviolet
planetColor Saturn = darkred
planetColor Uranus = darkolivegreen
planetColor Neptune = darkslateblue
planetColor Pluto = deeppink
planetColor MeanNode = dimgray
planetColor MeanApog = darkorchid
planetColor Chiron = firebrick
planetColor _ = mempty

-- | Get all days in the given range, as @JulianDayTT@s
julianDays :: Day -> Day -> [JulianDayTT]
julianDays startDay endDay =
  [start .. end]
  where
    (startY, startM, startD) = toGregorian startDay
    (endY, endM, endD) = toGregorian endDay
    start = gregorianToFakeJulianDayTT startY startM startD 0
    end = gregorianToFakeJulianDayTT endY endM endD 0

-- | Generate all crossings where an aspect can occur. Note that there's multiple,
-- however many can fit in an ecliptic!
aspectBands :: Double -> EphemerisPosition Double-> [Double]
aspectBands aspectAngle (EphemerisPosition _planet position _spd) =
  take n $ tail $ iterate angleInEcliptic position
  where
    -- how many times this aspect can occur in the ecliptic
    n = floor $ 360 / aspectAngle
    angleInEcliptic = toLongitude . (+ aspectAngle)

sextiles,
  squares,
  trines ::
    EphemerisPosition Double-> [Double]
sextiles = aspectBands 60.0
squares = aspectBands 90.0
trines = aspectBands 120.0

-- there's only one opposition: the "other one" is
-- just the conjunction.
opposition :: EphemerisPosition Double -> Double
opposition = head . aspectBands 180.0

toLongitude :: Double -> Double
toLongitude e
  | e > 360 = abs $ 360 - e
  | e == 360 = 0
  | e < 0 = abs $ 360 + e
  | otherwise = e

-- from: https://gitlab.haskell.org/ghc/ghc/-/issues/1408
groupWhen :: (a -> a -> Bool) -> [a] -> [[a]]
groupWhen _ [] = []
groupWhen _ [a] = [[a]]
groupWhen f (a : l) =
  if f a (head c)
    then (a : c) : r
    else [a] : c : r
  where
    (c : r) = groupWhen f l

---
--- OPT UTILS
---

dayReader :: ReadM Day
dayReader = eitherReader $ \arg ->
  case iso8601ParseM arg of
    Nothing -> Left $ "Invalid date: " <> arg
    Just day -> Right day

datetimeReader :: ReadM UTCTime
datetimeReader = eitherReader $ \arg ->
  case iso8601ParseM arg of
    Nothing -> Left $ "Invalid UTC timestamp: " <> arg
    Just ts -> Right ts

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
