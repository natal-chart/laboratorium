{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Chart.TransitOverview where

import Data.Foldable (forM_, Foldable (toList))
import Data.Time
  ( UTCTime,
  )
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import Graphics.Rendering.Chart.Easy hiding (days)
import SwissEphemeris
  ( JulianDayTT,
    Planet (..),
    ZodiacSignName (..)
  )
import SwissEphemeris.Precalculated (EphemerisPosition (EphemerisPosition, epheLongitude))
import qualified Data.Map as M
import Query.PlanetEphe
import Query.Aggregate
import qualified Data.Bifunctor as B


data Line = Solid | Dotted


-- | Default planets to consider for transits.
defaultPlanets :: [Planet]
defaultPlanets = [Sun .. Pluto] <> [MeanNode, MeanApog, Chiron]

transitChart :: [(JulianDayTT, UTCTime)] -> PlanetEphe -> EphemerisPosition Double -> IO ()
transitChart transitRange (Aggregate transits) natalEphe@(EphemerisPosition transited natalPos _spd) = do
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
      
    -- plot the positions of all chosen planets for the selected period
    forM_ (M.toAscList transits) $ \(transitingP, transits') -> do
       plotLeft $
        planetLine
          (show transitingP)
          (opaque $ planetColor transitingP)
          (planetLineStyle transitingP)
          (extractPositions transits')

    -- plot the original natal position line
    plotLeft $
      natalLine
        ("Natal " <> show transited)
        (opaque green)
        [natalPos]

extractPositions :: PlanetPositionSeq -> [(UTCTime, Double)]
extractPositions s =
  fmap (B.second epheLongitude) s
  & toList

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
-- and: https://mail.haskell.org/pipermail/libraries/2009-November/012794.html
groupWhen :: (a -> a -> Bool) -> [a] -> [[a]]
groupWhen _ [ ] = []
groupWhen _ [x] = [[x]]
groupWhen p (c:cs) = r : groupWhen p xs'
  where
  (r,xs') = run c cs
  cons' x (xs,y) = (x:xs,y)
  run y [] = ([y],[])
  run y l@(x:xs) | p y x     = cons' y $ run x xs
                 | otherwise = ([y],l)
