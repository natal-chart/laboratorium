{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
module TransitCharts (main) where

import Data.Foldable (forM_, traverse_)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Time
  ( Day,
    UTCTime (UTCTime),
    diffTimeToPicoseconds,
    fromGregorian,
    picosecondsToDiffTime,
    toGregorian,
  )
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import Graphics.Rendering.Chart.Easy hiding (days)
import SwissEphemeris
  ( EclipticPosition (EclipticPosition, lng),
    JulianTime (..),
    ZodiacSignName(..),
    Planet
      (..),
    calculateEclipticPosition,
    gregorianDateTime,
    julianDay,
  )
import Options.Applicative
import Text.Read (readMaybe)

type EclipticPoint = (UTCTime, Double)

type Ephemeris = (Planet, [EclipticPoint])

data Line = Solid | Dotted

deriving instance Read Planet

-- | Default planets to consider for transits.
defaultPlanets :: [Planet]
defaultPlanets = [Sun .. Pluto] <> [MeanNode, MeanApog, Chiron]

data Options = Options
  { optBirthday :: !UTCTime
  , optRangeStart :: !Day
  , optRangeEnd :: !Day
  , optNatalPlanets :: [Planet]
  }

main :: IO ()
main = do
  Options{optBirthday,optRangeStart,optRangeEnd,optNatalPlanets} <-
    execParser optsParser
  let days = julianDays optRangeStart optRangeEnd
      natalPlanets = optNatalPlanets
      julian = utcToJulian optBirthday 

  transited <- traverse (natalPosition julian) natalPlanets
  traverse_ (transitChart days) (catMaybes transited)

transitChart :: [JulianTime] -> (Planet, EclipticPoint) -> IO ()
transitChart transitRange (transited, natalEphe@(_t, natalPos)) = do
  transits <- traverse (transitingPositions transitRange) defaultPlanets

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
    let zodiacBands = take 12 $ iterate (bimap (+30) (+30)) (0,30)
        alternatingColors = concat $ replicate 6 [lightgray, white]
    forM_ (zip3 [Aries .. Pisces] alternatingColors zodiacBands) $ \(sign, color, band) -> do
      plotRight (fbetween (show sign) color [(julianToUTC t, band) | t <- transitRange])

    -- plot the positions of all planets for all year
    forM_ transits $ \(transiting, ephemeris) -> do
      plotLeft $
        planetLine
          (show transiting)
          (opaque $ planetColor transiting)
          (planetLineStyle transiting)
          ephemeris

    -- plot the original natal position line
    plotLeft $
      natalLine
        ("Natal " <> show transited)
        (opaque green)
        [natalPos]



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

aspectLine, natalLine :: String
 -> AlphaColour Double -> [Double] -> EC l2 (PlotLines x Double)
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

-- | Get all days in the given range, as @JulianTime@s
julianDays :: Day -> Day -> [JulianTime]
julianDays startDay endDay =
  map JulianTime [start, (start + 1.0) .. end]
  where
    start = unJulianTime . utcToJulian $ UTCTime startDay 0
    end = unJulianTime . utcToJulian $ UTCTime endDay 0

-- | Get all positions across the given time range for
-- the given planet.
transitingPositions :: [JulianTime] -> Planet -> IO Ephemeris
transitingPositions days p = do
  ephe <- mapM (`eclipticEphemeris` p) days
  pure (p, catMaybes ephe)

natalPosition :: JulianTime -> Planet -> IO (Maybe (Planet, EclipticPoint))
natalPosition bday p = do
  ephe <- eclipticEphemeris bday p
  pure $ fmap (p,) ephe

-- | Get the single ecliptic "point" (time + longitude)
-- for the given planet at the given UTC time.
eclipticEphemeris :: JulianTime -> Planet -> IO (Maybe EclipticPoint)
eclipticEphemeris t p = do
  pos <- calculateEclipticPosition t p
  case pos of
    Left _ -> pure Nothing
    Right EclipticPosition {lng} -> pure $ Just (julianToUTC t, lng)

-- | Generate all crossings where an aspect can occur. Note that there's multiple,
-- however many can fit in an ecliptic! 
aspectBands :: Double -> EclipticPoint -> [Double]
aspectBands aspectAngle (_planet, position) =
  take n $ tail $ iterate angleInEcliptic position
  where
    -- how many times this aspect can occur in the ecliptic
    n = floor $ 360/aspectAngle
    angleInEcliptic = toLongitude . (+ aspectAngle)

sextiles, squares, trines
  :: EclipticPoint -> [Double]

sextiles = aspectBands 60.0
squares = aspectBands 90.0
trines = aspectBands 120.0

-- there's only one opposition: the "other one" is 
-- just the conjunction.
opposition :: EclipticPoint -> Double
opposition = head . aspectBands 180.0


picosecondsInHour :: Double
picosecondsInHour = 3600 * 1e12

-- | Convert between a UTC timestamp and the low-level JulianTime that SwissEphemeris requires.
utcToJulian :: UTCTime -> JulianTime
utcToJulian (UTCTime day time) =
  julianDay (fromIntegral y) m d h
  where
    (y, m, d) = toGregorian day
    h = (1 / picosecondsInHour) * fromIntegral (diffTimeToPicoseconds time)

julianToUTC :: JulianTime -> UTCTime
julianToUTC jd =
  UTCTime day dt
  where
    (y, m, d, h) = gregorianDateTime jd
    day = fromGregorian (fromIntegral y) m d
    dt = picosecondsToDiffTime $ round $ h * picosecondsInHour

toLongitude :: Double -> Double
toLongitude e
  | e > 360 = abs $ 360 - e
  | e == 360 = 0
  | e < 0 = abs $ 360 + e
  | otherwise = e

-- from: https://gitlab.haskell.org/ghc/ghc/-/issues/1408
groupWhen :: (a -> a -> Bool) -> [a] -> [[a]]
groupWhen _ []    = []
groupWhen _ [a]   = [[a]]
groupWhen f (a:l) =
  if f a (head c) then (a:c):r
  else [a]:c:r
  where (c:r) = groupWhen f l

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
    <$> option datetimeReader   (long "date" <> short 'd')
    <*> option dayReader        (long "start" <> short 's')
    <*> option dayReader        (long "end" <> short 'e')
    <*> option planetListReader (long "planets" <> short 'p' <> help "space-separated list of planets")
