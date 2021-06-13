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

main :: IO ()
main = 
  transitChart Mercury

transitChart :: Planet -> IO ()
transitChart planet =  do
  (_p, ephemeris) <- transitingPositions planet
  toFile def ("charts/" <> show planet <> "_transits.svg") $ do
    layout_title .= "Transiting " <> show planet
    -- plot the positions of the given planet for all year
    plot (line (show planet) [ephemeris])
    -- plot the original natal position line
    forM_ natalPositions $ \(natalPlanet, positions) -> do
      plot (line (show natalPlanet) [positions])
    -- TODO: plot the different aspect "bands"


-- | handy type alias to signify the positions of a
-- body through universal time
type EclipticPoint = (UTCTime, Double)
type Ephemeris = (Planet, [EclipticPoint])

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
natalPositions :: [Ephemeris]
natalPositions =
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

aspectBand :: Double -> Ephemeris -> (Ephemeris, Ephemeris)
aspectBand aspectAngle (planet, angles) =
  (
    (planet, before),
    (planet, after)
  )
  where
    after = angles & traverse . _2 %~ toLongitude . (+ aspectAngle)
    before = angles & traverse . _2 %~ toLongitude . (-) aspectAngle

conjunctionBand = aspectBand 0.0
sextileBand = aspectBand 60.0
squareBand = aspectBand 90.0
trineBand = aspectBand 120.0
oppositionBand = aspectBand 180.0


toLongitude :: Double -> Double
toLongitude e
  | e > 360   = abs $ 360 - e
  | e == 360  = 0
  | e < 0     = abs $ 360 + e
  | otherwise = e
