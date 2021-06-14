{-# LANGUAGE NamedFieldPuns #-}

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
  ( AlphaColour,
    Colour,
    Default (def),
    EC,
    Layout,
    PlotFillBetween,
    PlotLines,
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
    layout_title,
    liftEC,
    line_color,
    line_dashes,
    opaque,
    orange,
    plot,
    plot_fillbetween_style,
    plot_fillbetween_title,
    plot_fillbetween_values,
    plot_lines_style,
    plot_lines_title,
    plot_lines_values,
    purple,
    red,
    salmon,
    solidFillStyle,
    withOpacity, plotLeft, layoutlr_title, plotRight, lightgray, darkgray, white
  )
import SwissEphemeris
  ( EclipticPosition (EclipticPosition, lng),
    JulianTime (..),
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

-- | Default orb before and after a given aspect. By default,
-- only 1 degree (yielding a 2 degree "band" around the aspect
-- angle.)
defaultOrb :: Double
defaultOrb = 1.0

-- | Default planets to consider for transits.
defaultPlanets :: [Planet]
defaultPlanets = [Sun .. Pluto] <> [MeanNode, MeanApog, Chiron]

main :: IO ()
main = do
  let days = julianDays (fromGregorian 2021 1 1) (fromGregorian 2022 1 1)
      natalPlanets = [Mars]--defaultPlanets
  bday <- iso8601ParseM "1989-01-07T05:30:00Z"
  transited <- traverse (natalPositions bday days) natalPlanets 
  traverse_ (transitChart days) transited

transitChart :: [JulianTime] -> Ephemeris -> IO ()
transitChart transitRange natalEphe@(transited, natalPositions) = do
  transits <- traverse (transitingPositions transitRange) defaultPlanets

  toFile def ("charts/" <> show transited <> "_transits.svg") $ do
    layoutlr_title .= "Transits to " <> show transited
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
      planetLine
        (show transited)
        (opaque green)
        Dotted
        [natalPositions]
    --
    -- plot the aspect "bands"
    plotLeft (fbetween "Conjunction" darkviolet (conjunctions Separating natalEphe))
    plotLeft (fbetween "SextileA" darkorange (sextiles Applying natalEphe))
    plotLeft (fbetween "SextileS" darkorange (sextiles Separating natalEphe))
    plotLeft (fbetween "SquareA" darkblue (squares Applying natalEphe))
    plotLeft (fbetween "SquareS" darkblue (squares Separating natalEphe))
    plotLeft (fbetween "TrineA" darkgreen (trines Applying natalEphe))
    plotLeft (fbetween "TrineS" darkgreen (trines Separating natalEphe))
    plotLeft (fbetween "OppositionA" darkred (oppositions Applying natalEphe))
    plotLeft (fbetween "OppositionS" darkred (oppositions Separating natalEphe))
    
    -- plot the zodiac bands
    plotRight (fbetween "Aries" lightgray [(julianToUTC t, (0.0, 30.0)) | t <- transitRange])
    plotRight (fbetween "Taurus" white [(julianToUTC t, (30.0, 60.0)) | t <- transitRange])
    plotRight (fbetween "Gemini" lightgray [(julianToUTC t, (60.0, 90.0)) | t <- transitRange])
    plotRight (fbetween "Cancer" white [(julianToUTC t, (90.0, 120.0)) | t <- transitRange])
    plotRight (fbetween "Leo" lightgray [(julianToUTC t, (120.0, 150.0)) | t <- transitRange])
    plotRight (fbetween "Virgo" white [(julianToUTC t, (150.0, 180.0)) | t <- transitRange])
    plotRight (fbetween "Libra" lightgray [(julianToUTC t, (180.0, 210.0)) | t <- transitRange])
    plotRight (fbetween "Scorpio" white [(julianToUTC t, (210.0, 240.0)) | t <- transitRange])
    plotRight (fbetween "Sagittarius" lightgray [(julianToUTC t, (240.0, 270.0)) | t <- transitRange])
    plotRight (fbetween "Capricorn" white [(julianToUTC t, (270.0, 300.0)) | t <- transitRange])
    plotRight (fbetween "Aquarius" lightgray [(julianToUTC t, (300.0, 330.0)) | t <- transitRange])
    plotRight (fbetween "Pisces" white [(julianToUTC t, (330.0, 360.0)) | t <- transitRange])

  

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

transitingPositions :: [JulianTime] -> Planet -> IO Ephemeris
transitingPositions days p = do
  ephe <- mapM (eclipticEphemeris p) days
  pure (p, catMaybes ephe)

natalPositions :: UTCTime -> [JulianTime] -> Planet -> IO Ephemeris
natalPositions bday days p = do
  ephe <- eclipticEphemeris p (utcToJulian bday)
  case ephe of
    Nothing -> pure (p, [])
    Just (_t, pos) -> pure (p, [(julianToUTC d, pos) | d <- days])

eclipticEphemeris :: Planet -> JulianTime -> IO (Maybe (UTCTime, Double))
eclipticEphemeris p t = do
  pos <- calculateEclipticPosition t p
  case pos of
    Left e -> pure Nothing
    Right EclipticPosition {lng} -> pure $ Just (julianToUTC t, lng)

-- TODO: return multiple aspect bands.
aspectBand :: Double -> AspectPhase -> Ephemeris -> [(UTCTime, (Double, Double))]
aspectBand aspectAngle aspectPhase (planet, positions) =
  concatMap mkBand positions
  where
    orb = defaultOrb
    angle Separating a = toLongitude . (+ aspectAngle) $ a
    angle Applying a = toLongitude . (-) aspectAngle $ a
    mkBand (t, p) =
      let before = subtract orb $ angle aspectPhase p
          after = (+ orb) $ angle aspectPhase p
       in [(t, (before, after))]

conjunctions, sextiles, squares, trines, oppositions 
  :: AspectPhase -> Ephemeris -> [(UTCTime, (Double, Double))]

conjunctions = aspectBand 0.0
sextiles = aspectBand 60.0
squares = aspectBand 90.0
trines = aspectBand 120.0
oppositions = aspectBand 180.0


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
