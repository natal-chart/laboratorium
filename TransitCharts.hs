{-# LANGUAGE NamedFieldPuns #-}
module TransitCharts (main) where
import SwissEphemeris
    ( calculateEclipticPosition,
      gregorianDateTime,
      julianDay,
      EclipticPosition(EclipticPosition, lng),
      JulianTime(..),
      Planet(Chiron, Moon, Mercury, Venus, Mars, Jupiter, Saturn, Uranus,
             Neptune, Sun, Pluto, MeanNode, MeanApog) )
import Data.Time
    ( Day,
      UTCTime(UTCTime),
      fromGregorian,
      toGregorian,
      diffTimeToPicoseconds,
      picosecondsToDiffTime )
import Data.Bifunctor (bimap)
import Control.Lens ( (.=) )
import Data.Foldable ( forM_, traverse_ )
import Data.Maybe (catMaybes, fromJust, isJust)
import Graphics.Rendering.Chart.Easy
    ( line_color,
      line_dashes,
      solidFillStyle,
      layout_title,
      plot_fillbetween_style,
      plot_fillbetween_title,
      plot_fillbetween_values,
      plot_lines_style,
      plot_lines_title,
      plot_lines_values,
      Layout,
      PlotFillBetween,
      PlotLines,
      Default(def),
      AlphaColour,
      Colour,
      liftEC,
      plot,
      opaque,
      withOpacity,
      darkblue,
      darkgreen,
      darkolivegreen,
      darkorange,
      darkorchid,
      darkred,
      darkslateblue,
      darkviolet,
      deeppink,
      dimgray,
      firebrick,
      green,
      orange,
      purple,
      red,
      salmon,
      EC )
import Graphics.Rendering.Chart.Backend.Diagrams ( toFile )
import Control.Monad.IO.Class (liftIO)
import Data.Traversable (forM)
import Data.Either (fromRight)
import Data.Time.Format.ISO8601 (iso8601ParseM)

type EclipticPoint = (UTCTime, Double)
type Ephemeris = (Planet, [EclipticPoint])
data AspectPhase = Applying | Separating
data Line = Solid | Dotted

-- | Default orb before and after a given aspect. By default,
-- only 1 degree (yielding a 2 degree "band" around the aspect
-- angle.)
defaultOrb :: Double
defaultOrb = 1.0


main :: IO ()
main = do
  let days = julianDays (fromGregorian 2021 1 1) (fromGregorian 2022 1 1)
  bday <- iso8601ParseM "1989-06-07T05:30:00Z"
  transited <- mapM (\p -> natalPositions p bday days) defaultPlanets
  traverse_ (transitChart days) transited

transitChart :: [JulianTime] -> Ephemeris -> IO ()
transitChart transitRange natalEphe@(transited, natalPositions) =  do
  transitingPositions <-
    forM defaultPlanets $ \transiting -> do
      transitingPositions transiting transitRange

  toFile def ("charts/" <> show transited <> "_transits.svg") $ do
    layout_title .= "Transits to " <> show transited
    -- plot the positions of all planets for all year
    forM_ transitingPositions $ \(transiting, ephemeris) -> do
      plot $
        planetLine
          (show transiting)
          (opaque $ planetColor transiting)
          (planetLineStyle transiting)
          [ephemeris]
    -- plot the original natal position line
    plot $
      planetLine
        (show transited)
        (opaque green)
        Dotted
        [natalPositions]
    --
    -- plot the aspect "bands"
    plot (fbetween "Conjunction" darkviolet (conjunctions Separating natalEphe))
    plot (fbetween "SextileA" darkorange  (sextiles Applying natalEphe))
    plot (fbetween "SextileS" darkorange (sextiles Separating natalEphe))
    plot (fbetween "SquareA" darkblue (squares Applying natalEphe))
    plot (fbetween "SquareS" darkblue (squares Separating natalEphe))
    plot (fbetween "TrineA" darkgreen (trines Applying natalEphe))
    plot (fbetween "TrineS" darkgreen (trines Separating natalEphe))
    plot (fbetween "OppositionA" darkred (oppositions Applying natalEphe))
    plot (fbetween "OppositionS" darkred (oppositions Separating natalEphe))

fbetween :: String
  -> Colour Double
  -> [(UTCTime, (Double, Double))]
  -> EC (Layout UTCTime Double) (PlotFillBetween UTCTime Double)
fbetween label color vals = liftEC $ do
  plot_fillbetween_style .= solidFillStyle (withOpacity color 0.4)
  plot_fillbetween_values .= vals
  plot_fillbetween_title  .= label


planetLine :: String
  -> AlphaColour Double
  -> Line
  -> [[(x, y)]]
  -> EC l2 (PlotLines x y)
planetLine title color lineStyle values = liftEC $ do
  plot_lines_title .= title
  plot_lines_values .= values
  plot_lines_style . line_color .= color
  plot_lines_style . line_dashes .= dashes lineStyle
  where
    dashes Solid = []
    dashes Dotted = [1.0, 1.0]

planetLineStyle :: Planet -> Line
planetLineStyle Moon = Dotted
planetLineStyle MeanNode = Dotted
planetLineStyle _    = Solid

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

-- | Get all days in the current year, as @JulianTime@s
julianDays :: Day -> Day -> [JulianTime]
julianDays startDay endDay =
  map JulianTime [start, (start + 1.0) .. end]
  where
    start = unJulianTime . utcToJulian $ UTCTime startDay 0
    end = unJulianTime . utcToJulian $ UTCTime endDay 0

transitingPositions :: Planet -> [JulianTime] -> IO Ephemeris
transitingPositions p days = do
  ephe <- mapM (eclipticEphemeris p) days
  pure (p, catMaybes ephe)
  
natalPositions :: Planet -> UTCTime -> [JulianTime] -> IO Ephemeris
natalPositions p bday days = do
  ephe <- eclipticEphemeris p (utcToJulian bday)
  case ephe of
    Nothing -> pure (p, [])
    Just (_t, pos) -> pure (p, [(julianToUTC d, pos) | d <- days])


eclipticEphemeris :: Planet -> JulianTime -> IO (Maybe (UTCTime, Double))
eclipticEphemeris p t = do
  pos <- calculateEclipticPosition t p
  case pos of
    Left e -> pure Nothing
    Right EclipticPosition{lng} -> pure $ Just (julianToUTC t,lng)

defaultPlanets :: [Planet]
defaultPlanets = [Sun .. Pluto] <> [MeanNode, MeanApog, Chiron]


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

aspectBand ::  Double -> AspectPhase -> Ephemeris -> [(UTCTime, (Double, Double))]
aspectBand aspectAngle aspectPhase (planet, positions) =
  map mkBand positions
  where
    orb = defaultOrb
    angle Separating a = toLongitude . (+ aspectAngle) $ a
    angle Applying a = toLongitude . (-) aspectAngle $ a
    mkBand (t,p) =
      let
        before = subtract orb $ angle aspectPhase p
        after  = (+ orb) $ angle aspectPhase p
      in (t, (before, after))


conjunctions = aspectBand 0.0
sextiles = aspectBand 60.0
squares = aspectBand 90.0
trines = aspectBand 120.0
oppositions = aspectBand 180.0


toLongitude :: Double -> Double
toLongitude e
  | e > 360   = abs $ 360 - e
  | e == 360  = 0
  | e < 0     = abs $ 360 + e
  | otherwise = e
