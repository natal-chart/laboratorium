{-# LANGUAGE NamedFieldPuns #-}
module Query.Transit where

import Data.Sequence ((><), Seq(..), (|>), (<|), ViewL(..), ViewR(..))
import qualified Data.Sequence as S
import qualified Data.Map as M
import SwissEphemeris
import SwissEphemeris.Precalculated
import Data.Foldable (toList, Foldable (foldMap'))
import Query.Common
import Control.Lens (over, Each (each))
import Data.Fixed (mod')
import Data.Function ( (&) )
import Data.Functor ((<&>))
import Control.Monad (guard)

type EphemerisPoint = (JulianDayTT, EphemerisPosition Double)

data Relation
  = Below
  | Crossed
  | Above
  deriving (Eq, Show)

data TransitPhase
  = ApplyingDirect
  | ApplyingRetrograde
  | TriggeredDirect
  | TriggeredRetrograde
  | SeparatingDirect
  | SeparatingRetrograde
  deriving (Eq, Show)

data AspectName
  = Sextile
  | Square
  | Trine
  | Opposition
  | Conjunction
  deriving (Eq, Enum, Show)

data Aspect = Aspect {
  aspectName :: !AspectName
, angle :: !Double
, orbApplying :: !Double
, orbSeparating :: !Double
} deriving (Eq, Show)

data Transit = Transit {
  aspect :: !AspectName
, phase :: !TransitPhase
, transitAngle :: !Double
, transitOrb :: !Double
, transitStarts :: !JulianDayTT
, transitEnds :: !JulianDayTT
}

sextile, square, trine, opposition, conjunction :: Aspect
conjunction = Aspect Conjunction 0 5 5
sextile = Aspect Sextile 60 5 5
square = Aspect Square 90 5 5
trine = Aspect Trine 120 5 5
opposition = Aspect Opposition 180 5 5

-- | Aspects to consider, arranged in a "cycle":
-- as soon as one is determined, no need to seek the others.
aspects :: [Aspect]
aspects = [conjunction, sextile, square, trine, opposition]


transit
  :: (EphemerisPoint, EphemerisPoint)
  -- ^ planet 1 at days 1->2
  -> (EphemerisPoint, EphemerisPoint)
  -- ^ planet 2 at days 1->2
  -> Maybe Transit
transit transiting@((t1, p11), (t2, p12)) transited@((_t1', _p21), (_t2', p22))
  | isTransiting transiting transited = Nothing
  | otherwise = do
    let points = (epheLongitude p11, epheLongitude p12, epheLongitude p22)
        (before, after, ref) = normalize points
        station = movement transiting
        rel = relation before after ref
        phase = transitPhase station rel
    (aspectName, angle', orb') <- determineAspect after ref
    pure $ Transit aspectName phase angle' orb' t1 t2


-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------

-- | Given two moments of movement for two separate planets/body,
-- determine which is to be considered the "fastest"
isTransiting :: (EphemerisPoint, EphemerisPoint) -> (EphemerisPoint, EphemerisPoint) -> Bool
isTransiting _p1@(_, (_,p12)) _p2@(_, (_,p22)) =
  epheSpeed p22 <= epheSpeed p12

-- | Given two moments of movement for a given planet/body,
-- determine the "character" thereof
movement :: (EphemerisPoint, EphemerisPoint) -> Station
movement (_d1@(_t1, p1), _d2@(_t2, p2))
  | isRelativelyStationary p2 =
    if epheSpeed p1 > epheSpeed p2 then
      StationaryRetrograde
    else
      StationaryDirect
  | epheSpeed p1 > epheSpeed p2 = Retrograde
  | otherwise = Direct

-- | Given two moments of a moving planet, and a reference point
-- determine if the moving planet remained above, below or crossed the
-- reference point; fails if the distance is infeasible
relation :: Double -> Double -> Double -> Relation
relation p1 p2 ref
  |  p1 <  ref &&  p2 <  ref = Below
  |  p1 <= ref &&  p2 >= ref = Crossed
  |  p1 >= ref &&  p2 <= ref = Crossed
  |  p1 >  ref &&  p2 >  ref = Above
  | otherwise = Crossed


transitPhase :: Station -> Relation -> TransitPhase
transitPhase Direct Below = ApplyingDirect
transitPhase StationaryDirect Below = ApplyingDirect
transitPhase Direct Crossed = TriggeredDirect
transitPhase StationaryDirect Crossed = TriggeredDirect
transitPhase Direct Above = SeparatingDirect
transitPhase StationaryDirect Above = SeparatingDirect
transitPhase Retrograde Above = ApplyingRetrograde
transitPhase StationaryRetrograde Above = ApplyingRetrograde
transitPhase Retrograde Crossed = TriggeredRetrograde
transitPhase StationaryRetrograde Crossed = TriggeredRetrograde
transitPhase Retrograde Below = SeparatingRetrograde
transitPhase StationaryRetrograde Below = SeparatingRetrograde

determineAspect :: Double -> Double -> Maybe (AspectName, Double, Double)
determineAspect p1 p2 =
  headMaybe $ do  
    asp <- aspects
    let dist = circleDistance p1 p2
        theta = angle asp
        orb = abs $ theta - dist
    guard $ dist <= (theta + maxOrb asp) 
    pure (aspectName asp, dist, orb) 

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe xs = Just . head $ xs


maxOrb :: Aspect -> Double
maxOrb Aspect{orbApplying, orbSeparating} = max orbApplying orbSeparating

-- | Given two points that relate to a third point, translate
-- by 90 degrees if the line jumps over the 360/0 point
normalize :: (Double, Double, Double) -> (Double, Double, Double)
normalize points@(p1, p2, _ref)
  | abs (p2 - p1) > 30 = over each (+90) points
  | otherwise = points

-- from: https://stackoverflow.com/questions/9505862/shortest-distance-between-two-degree-marks-on-a-circle/9505991#9505991
-- 180.0 - std::fabs(std::fmod(std::fabs(first - second), 360.0) - 180.0)
circleDistance :: Double -> Double -> Double
circleDistance a b =
  180 - abs(abs(a - b) `mod'` 360 - 180)
