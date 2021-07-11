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

data Crossing = Crossing
  { crossingEnters :: !(Maybe JulianDayTT)
  , crossingExits :: !(Maybe JulianDayTT)
  , crossingDegree :: !Double
  , crossingPlanetEntered :: !(Maybe SimplePlanetStation)
  , crossingPlanetExited :: !(Maybe SimplePlanetStation)
  } deriving (Show)

newtype CrossingSeq =
  CrossingSeq { getCrossings :: S.Seq Crossing}
  deriving (Show)

singleton :: Crossing -> CrossingSeq
singleton = CrossingSeq . S.singleton

instance Semigroup CrossingSeq where
  (CrossingSeq s1) <> (CrossingSeq s2) =
    let s1Last = S.viewr s1
        s2First = S.viewl s2
    in CrossingSeq $ mergeCrossingEvents s1Last s2First

instance Monoid CrossingSeq where
  mempty = CrossingSeq S.empty

newtype CrossingMap =
  CrossingMap { getCrossingMap :: M.Map Planet CrossingSeq}
  deriving (Show)

instance Semigroup CrossingMap where
  (CrossingMap c1) <> (CrossingMap c2) =
    CrossingMap $ M.unionWith (<>) c1 c2

instance Monoid CrossingMap where
  mempty = CrossingMap M.empty

foldCrossings :: [Double] -> [[Either String (Ephemeris Double)]] -> CrossingMap
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


mergeCrossingEvents :: ViewR Crossing -> ViewL Crossing -> Seq Crossing
mergeCrossingEvents EmptyR EmptyL = S.empty
mergeCrossingEvents EmptyR (x :< xs) = x <| xs
mergeCrossingEvents (xs :> x) EmptyL = xs |> x
mergeCrossingEvents (xs :> x) (y :< ys) =
  if crossingDegree x /= crossingDegree y then
    (xs |> merged) >< ys
  else
    -- if it's the same degree... discard the latest
    -- event as a weird "wobble"
    (xs |> x) >< ys
  where
    merged = x {
      crossingExits   = crossingEnters y,
      crossingPlanetExited = crossingPlanetEntered y
    }

mkCrossing :: (JulianDayTT, EphemerisPosition Double) -> (JulianDayTT, EphemerisPosition Double) -> Double -> Maybe Crossing
mkCrossing (d1, pos1) (_d2, pos2) toCross
  | epheLongitude pos1 <= toCross && epheLongitude pos2 > toCross =
    Just $ Crossing {
        crossingEnters = Just d1,
        crossingExits = Nothing,
        crossingDegree = toCross,
        crossingPlanetEntered = Just Direct,
        crossingPlanetExited = Nothing
      }
  | epheLongitude pos1 >= toCross && epheLongitude pos2 < toCross =
    Just $ Crossing {
        crossingEnters = Just d1,
        crossingExits = Nothing,
        crossingDegree = toCross,
        crossingPlanetEntered = Just Direct,
        crossingPlanetExited = Nothing
    }
  | otherwise = Nothing
