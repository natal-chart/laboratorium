{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Query.Crossing where

import SwissEphemeris
import Data.Sequence ((><), Seq(..), (|>), (<|), ViewL(..), ViewR(..))
import qualified Data.Sequence as S
import SwissEphemeris.Precalculated
import qualified Data.Map as M
import Data.Foldable (Foldable(foldMap', toList))
import Data.Maybe (mapMaybe)

data SimplePlanetStation = Retrograde | Direct
  deriving (Eq, Show)

data Crossing a = Crossing
  { crossingEnters :: !(Maybe JulianDayTT)
  , crossingExits :: !(Maybe JulianDayTT)
  , crossingSubject :: !a
  , crossingPlanetEntered :: !(Maybe SimplePlanetStation)
  , crossingPlanetExited :: !(Maybe SimplePlanetStation)
  } deriving (Show)

newtype CrossingSeq a =
  CrossingSeq { getCrossings :: S.Seq (Crossing a)}
  deriving (Show)

singleton :: Crossing a -> CrossingSeq a
singleton = CrossingSeq . S.singleton

instance HasEclipticLongitude  a => Semigroup (CrossingSeq a) where
  (CrossingSeq s1) <> (CrossingSeq s2) =
    let s1Last = S.viewr s1
        s2First = S.viewl s2
    in CrossingSeq $ mergeCrossingEvents s1Last s2First

instance HasEclipticLongitude  a => Monoid (CrossingSeq a) where
  mempty = CrossingSeq S.empty

newtype CrossingMap a =
  CrossingMap { getCrossingMap :: M.Map Planet (CrossingSeq a)}
  deriving (Show)

instance HasEclipticLongitude  a => Semigroup (CrossingMap a) where
  (CrossingMap c1) <> (CrossingMap c2) =
    CrossingMap $ M.unionWith (<>) c1 c2

instance HasEclipticLongitude  a => Monoid (CrossingMap a) where
  mempty = CrossingMap M.empty

foldCrossings :: HasEclipticLongitude  a => [a] -> [[Either String (Ephemeris Double)]] -> CrossingMap a
foldCrossings degreesToCross = foldMap' $ \case
  (Right pos1 : Right pos2 : _) ->
    mconcat $ flip map (zip (toList $ ephePositions pos1) (toList $ ephePositions pos2)) $ \(p1, p2) ->
      if ephePlanet p1 `elem` [Moon, TrueNode, MeanNode] then
        CrossingMap M.empty
      else
        CrossingMap
          $ M.fromList
          $ map (\c -> (ephePlanet p1, singleton c))
          $ mapMaybe (mkCrossing (epheDate pos1, p1) (epheDate pos2, p2))
          degreesToCross
  _ ->
    CrossingMap M.empty


mergeCrossingEvents :: HasEclipticLongitude  a => ViewR (Crossing a) -> ViewL (Crossing a) -> Seq (Crossing a)
mergeCrossingEvents EmptyR EmptyL = S.empty
mergeCrossingEvents EmptyR (x :< xs) = x <| xs
mergeCrossingEvents (xs :> x) EmptyL = xs |> x
mergeCrossingEvents (xs :> x) (y :< ys) =
  if crossingSubject x == crossingSubject y then
    (xs |> merged) >< ys
  else
    (xs |> updated) >< ( y <| ys)
  where
    merged = x {
      crossingExits   = crossingExits y,
      crossingPlanetExited = crossingPlanetExited y
    }
    updated = x {
      crossingExits = crossingEnters y,
      crossingPlanetExited = crossingPlanetEntered y
    }

mkCrossing :: HasEclipticLongitude a => (JulianDayTT, EphemerisPosition Double) -> (JulianDayTT, EphemerisPosition Double) -> a -> Maybe (Crossing a)
mkCrossing (d1, pos1) (_d2, pos2) toCross
  | crossesDirect (epheLongitude pos1) (epheLongitude pos2) (getEclipticLongitude toCross) =
     Just $ Crossing {
        crossingEnters = Just d1,
        crossingExits = Nothing,
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
