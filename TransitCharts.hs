{-# LANGUAGE NamedFieldPuns #-}

{-# LANGUAGE TupleSections #-}
module TransitCharts (main) where

import Control.Lens ((.=))
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (bimap)
import Data.Either (fromRight)
import Data.Foldable (forM_, traverse_)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Time
  ( Day,
    UTCTime (UTCTime),
    diffTimeToPicoseconds,
    fromGregorian,
    picosecondsToDiffTime,
    toGregorian,
  )
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Traversable (forM)
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import Graphics.Rendering.Chart.Easy
import SwissEphemeris
  ( EclipticPosition (EclipticPosition, lng),
    JulianTime (..),
    ZodiacSignName(..),
    Planet
      ( Chiron,
        Jupiter,
        Mars,
        MeanApog,
        MeanNode,
        Mercury,
        Moon,
        Neptune,
        Pluto,
        Saturn,
        Sun,
        Uranus,
        Venus
      ),
    calculateEclipticPosition,
    gregorianDateTime,
    julianDay,
  )

type EclipticPoint = (UTCTime, Double)

type Ephemeris = (Planet, [EclipticPoint])

data AspectPhase = Applying | Separating

data Line = Solid | Dotted

-- | Default planets to consider for transits.
defaultPlanets :: [Planet]
defaultPlanets = [Sun .. Pluto] <> [MeanNode, MeanApog, Chiron]

main :: IO ()
main = do
  bday <- iso8601ParseM "1989-01-07T05:30:00Z"
  let days = julianDays (fromGregorian 2021 1 1) (fromGregorian 2022 1 1)
      natalPlanets = [Mars]--defaultPlanets
      julianDay = utcToJulian bday

  transited <- traverse (natalPosition julianDay) natalPlanets
  traverse_ (transitChart days) (catMaybes transited)

transitChart :: [JulianTime] -> (Planet, EclipticPoint) -> IO ()
transitChart transitRange (transited, natalEphe@(_t, natalPosition)) = do
  transits <- traverse (transitingPositions transitRange) defaultPlanets

  toFile def ("charts/" <> show transited <> "_transits.svg") $ do
    layoutlr_title .= "Transits to " <> show transited
    -- hide axis guidelines -- too much noise
    layoutlr_left_axis . laxis_override .= axisGridHide
    layoutlr_right_axis . laxis_override .= axisGridHide
    -- plot the positions of all planets for all year
    forM_ transits $ \(transiting, ephemeris) -> do
      plotLeft $
        planetLine
          (show transiting)
          (opaque $ planetColor transiting)
          (planetLineStyle transiting)
          [ephemeris]

    -- plot the original natal position line
    plotLeft $
      natalLine
        (show transited)
        (opaque green)
        [natalPosition]

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



fbetween ::
  String ->
  Colour Double ->
  [(UTCTime, (Double, Double))] ->
  EC l2 (PlotFillBetween UTCTime Double)
fbetween label color vals = liftEC $ do
  plot_fillbetween_style .= solidFillStyle (withOpacity color 0.4)
  plot_fillbetween_values .= vals
  plot_fillbetween_title .= label

planetLine ::
  String ->
  AlphaColour Double ->
  Line ->
  [[(x, y)]] ->
  EC l2 (PlotLines x y)
planetLine title color lineStyle values = liftEC $ do
  plot_lines_title .= title
  plot_lines_values .= values
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
    Left e -> pure Nothing
    Right EclipticPosition {lng} -> pure $ Just (julianToUTC t, lng)

-- | Generate all crossings where an aspect can occur. Note that there's multiple,
-- however many can fit in an ecliptic! 
aspectBands :: Double -> EclipticPoint -> [Double]
aspectBands aspectAngle (planet, position) =
  take n $ tail $ iterate angleInEcliptic position
  where
    -- how many times this aspect can occur in the ecliptic
    n = floor $ 360/aspectAngle
    angleInEcliptic = toLongitude . (+ aspectAngle)

conjunctions, sextiles, squares, trines 
  :: EclipticPoint -> [Double]

conjunctions = aspectBands 0.0
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
