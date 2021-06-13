{-# LANGUAGE NamedFieldPuns #-}
module TransitCharts (main) where
import SwissEphemeris
import Data.Time
import Data.Bifunctor (bimap)
import Control.Lens
import Data.Foldable
import Data.Maybe (catMaybes)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Control.Monad.IO.Class (liftIO)
import Data.Traversable (forM)

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
main =
  traverse_ transitChart transited
  where
    transited = natalPlanets --filter ((==) Uranus . fst) natalPlanets

transitChart :: Ephemeris -> IO ()
transitChart natalEphe@(transited, natalPositions) =  do
  transitingPositions <-
    forM (map fst natalPlanets) $ \transiting -> do
      transitingPositions transiting

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
currentYearDays :: [JulianTime]
currentYearDays =
  map JulianTime [start, (start + 1.0) .. end]
  where
    start = unJulianTime . utcToJulian $ UTCTime (fromGregorian 2021 1 1) 0
    end = unJulianTime . utcToJulian $ UTCTime (fromGregorian 2021 12 31) 0

transitingPositions :: Planet -> IO Ephemeris
transitingPositions p = do
  ephe <- mapM mkEphemeris currentYearDays
  pure (p, catMaybes ephe)

  where
    mkEphemeris :: JulianTime -> IO (Maybe (UTCTime, Double))
    mkEphemeris t = do
      pos <- calculateEclipticPosition t p
      case pos of
        Left e -> pure Nothing
        Right EclipticPosition{lng} -> pure $ Just (julianToUTC t,lng)

-- | Natal positions for a single person
natalPlanets :: [Ephemeris]
natalPlanets =
  [
    (Sun, allYear 286.90)
  , (Moon, allYear 279.38)
  , (Mercury, allYear 305.83)
  , (Venus, allYear 265.59)
  , (Mars, allYear 23.43)
  , (Jupiter, allYear 56.38)
  , (Saturn, allYear 276.32)
  , (Uranus, allYear 272.12)
  , (Neptune, allYear 280.16)
  , (Pluto, allYear 224.71)
  , (MeanNode, allYear 337.46)
  , (MeanApog, allYear 176.41)
  , (Chiron, allYear 93.46)
  ]
  where
    allYear pos = [(julianToUTC d, pos) | d <- currentYearDays]
---
--- HELPERS
---

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
