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

-- TODO: remove this?
data SimplePlanetStation = Retrograde | Direct
  deriving (Eq, Show)

data Crossing a = Crossing
  { crossingEnters :: !JulianDayTT
  , crossingExits :: !JulianDayTT
  , crossingSubject :: !a
  , crossingPlanetEntered :: !(Maybe SimplePlanetStation)
  , crossingPlanetExited :: !(Maybe SimplePlanetStation)
  } deriving (Show)

instance Eq a => Merge (Crossing a) where
  x `merge` y =
    if crossingSubject x == crossingSubject y then
      Merge merged
    else
      ReplaceL updated
    where
      merged = x {
        crossingExits = crossingExits y,
        crossingPlanetExited = crossingPlanetExited y
      }
      updated = x {
        crossingExits = crossingEnters y,
        crossingPlanetExited = crossingPlanetEntered y
      }
      
instance Temporal (Crossing a) where
  type TemporalIndex (Crossing a) = JulianDayTT
  startTime = crossingEnters
  endTime = crossingExits

type CrossingMap a = Grouped Planet (Crossing a)

data Zodiac = Zodiac
  { signName :: ZodiacSignName, signLng :: Double}
  deriving (Eq, Show)

instance HasEclipticLongitude Zodiac where
  getEclipticLongitude (Zodiac _ l) = l


crossings :: (Monad m, HasEclipticLongitude a) => [a] -> Stream (Of (Ephemeris Double)) m b -> m (Of (CrossingMap a) b)
crossings degs =
  ephemerisWindows 2 >>> St.foldMap (mapCrossings degs)

mapCrossings :: HasEclipticLongitude a => [a] -> Seq (Ephemeris Double) -> CrossingMap a
mapCrossings degreesToCross (pos1 :<| pos2 :<| _) =
  concatForEach (zip (toList $ ephePositions pos1) (toList $ ephePositions pos2)) $ \(p1, p2) ->
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

mkCrossing :: HasEclipticLongitude a => (JulianDayTT, EphemerisPosition Double) -> (JulianDayTT, EphemerisPosition Double) -> a -> Maybe (Crossing a)
mkCrossing (_d1, pos1) (d2, pos2) toCross
  | crossesDirect (epheLongitude pos1) (epheLongitude pos2) (getEclipticLongitude toCross) =
     Just $ Crossing {
        crossingEnters = d2,
        crossingExits = d2,
        crossingSubject = toCross,
        crossingPlanetEntered = Just Direct,
        crossingPlanetExited = Nothing
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
