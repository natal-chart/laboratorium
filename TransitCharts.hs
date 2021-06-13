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

main :: IO ()
main =
  traverse_ transitChart transited
  where
    transited = filter ((==) Uranus . fst) natalPlanets

transitChart :: Ephemeris -> IO ()
transitChart natalEphe@(transited, natalPositions) =  do
  transitingPositions <-
    forM (map fst natalPlanets) $ \transiting -> do
      transitingPositions transiting

  toFile def ("charts/" <> show transited <> "_transits.svg") $ do
    layout_title .= "Transits to " <> show transited
    -- plot the positions of all planets for all year
    forM_ transitingPositions $ \(transiting, ephemeris) -> do
      plot (line (show transiting) [ephemeris])
    -- plot the original natal position line
    plot (line (show transited) [natalPositions])
    -- plot the aspect "bands"
    plot (fbetween "Conjunction" purple (conjunctions Separating natalEphe))
    plot (fbetween "SextileA" yellow (sextiles Applying natalEphe))
    plot (fbetween "SextileS" yellow (sextiles Separating natalEphe))
    plot (fbetween "SquareA" blue (squares Applying natalEphe))
    plot (fbetween "SquareS" blue (squares Separating natalEphe))
    plot (fbetween "TrineA" green (trines Applying natalEphe))
    plot (fbetween "TrineS" green (trines Separating natalEphe))
    plot (fbetween "OppositionA" red (oppositions Applying natalEphe))
    plot (fbetween "OppositionS" red (oppositions Separating natalEphe))

fbetween :: String
  -> Colour Double
  -> [(UTCTime, (Double, Double))]
  -> EC (Layout UTCTime Double) (PlotFillBetween UTCTime Double)
fbetween label color vals = liftEC $ do
  plot_fillbetween_style .= solidFillStyle (withOpacity color 0.2)
  plot_fillbetween_values .= vals
  plot_fillbetween_title  .= label


-- | handy type alias to signify the positions of a
-- body through universal time
type EclipticPoint = (UTCTime, Double)
type Ephemeris = (Planet, [EclipticPoint])
data AspectPhase = Applying | Separating

defaultOrb :: Double
defaultOrb = 1.0

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
