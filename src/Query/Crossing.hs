{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
module Query.Crossing where

import SwissEphemeris
import Data.Sequence (Seq(..))
import SwissEphemeris.Precalculated
import qualified Data.Map as M
import Data.Foldable (Foldable(toList))
import Data.Maybe (mapMaybe)
import Query.Aggregate
import Streaming (Stream, Of)
import qualified Streaming.Prelude as St
import Control.Category ((>>>))
import Query.Streaming
import Query.Common (concatForEach)
import Query.EventTypes

type CrossingMap a = Grouped Planet (Crossing a)

crossings :: (Monad m, HasEclipticLongitude a) => [a] -> Stream (Of (Ephemeris Double)) m b -> m (Of (CrossingMap a) b)
crossings degs =
  ephemerisWindows 2 >>> St.foldMap (mapCrossings degs)

mapCrossings :: HasEclipticLongitude a => [a] -> Seq (Ephemeris Double) -> CrossingMap a
mapCrossings degreesToCross (pos1 :<| pos2 :<| _) =
  concatForEach (zip (toList $ ephePositions pos1) (toList $ ephePositions pos2)) $ \(p1, p2) ->
    -- TODO: allow selecting which planets to consider -- we _do_ want to know
    -- Moon crossings over house cusps.
    -- the Moon crosses most of the ecliptic every month, so it's not
    -- really significant. The nodes do have a slower behavior, so maybe
    -- I'll reinstate them.
    if ephePlanet p1 `elem` [Moon, TrueNode, MeanNode] then
      mempty
    else
      Aggregate
        $ M.fromList
        $ map (\c -> (ephePlanet p1, singleton c))
        $ mapMaybe (mkCrossing (epheDate pos1, p1) (epheDate pos2, p2))
        degreesToCross

mapCrossings _ _ = mempty

getCrossings' :: HasEclipticLongitude a => (Crossing a -> Event ) -> [Planet] -> [a] -> Seq (Ephemeris Double) -> Grouped Planet Event
getCrossings' mkEvent selectedPlanets degreesToCross (pos1 :<| pos2 :<| _) =
  concatForEach (zip (toList $ ephePositions pos1) (toList $ ephePositions pos2)) $ \(p1, p2) ->
    if ephePlanet p1 `notElem` selectedPlanets then
      mempty
    else
      Aggregate
        $ M.fromList
        $ map (\c -> (ephePlanet p1, singleton $ mkEvent c))
        $ mapMaybe (mkCrossing (epheDate pos1, p1) (epheDate pos2, p2))
        degreesToCross

getCrossings' _ _ _ _ = mempty

getZodiacCrossings :: [Planet]-> [Zodiac] -> Seq (Ephemeris Double) -> Grouped Planet Event
getZodiacCrossings = getCrossings' ZodiacIngress

getHouseCrossings :: [Planet] -> [House] -> Seq (Ephemeris Double) -> Grouped Planet Event
getHouseCrossings = getCrossings' HouseIngress 

mkCrossing :: HasEclipticLongitude a => (JulianDayTT, EphemerisPosition Double) -> (JulianDayTT, EphemerisPosition Double) -> a -> Maybe (Crossing a)
mkCrossing (d1, pos1) (d2, pos2) toCross
  | crossesDirect (epheLongitude pos1) (epheLongitude pos2) (getEclipticLongitude toCross) =
     Just $ Crossing {
        crossingStarts = d1,
        crossingEnds = d2,
        crossingCrosses = toCross,
        crossingPlanet = ephePlanet pos1,
        crossingDirection = if epheSpeed pos2 < 0 then RetrogradeMotion  else DirectMotion
      }
  | otherwise = Nothing

crossesDirect :: Double -> Double -> Double -> Bool
crossesDirect p1 p2 toCross =
  if abs (p1 - p2) >= 100 && toCross == 0 then
    p1 <= (toCross + 360) && p2 >= toCross
  else
    p1 <= toCross && p2 > toCross

crossesRetrograde :: Double -> Double -> Double -> Bool
crossesRetrograde p1 p2 toCross =
  if abs (p1 - p2) >= 100 then
    p1 >= toCross && p2 < (toCross + 360)
  else
    p1 >= toCross && p2 < toCross
