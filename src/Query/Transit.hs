{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}

module Query.Transit where

import Data.Sequence (Seq ((:<|)))
import qualified Data.Sequence as S
import qualified Streaming.Prelude as St
import qualified Data.Map as M
import SwissEphemeris
import SwissEphemeris.Precalculated
import Query.Common
import Control.Lens (over, Each (each))
import Data.Fixed (mod')
import Control.Monad (guard, join)
import Data.List (tails)
import Control.Applicative (liftA2)
import Query.Aggregate
import Query.Streaming
import Control.Category ((>>>))

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
, transitProgress :: !(S.Seq (JulianDayTT, Double))
} deriving (Show)

instance Merge Transit where
  x `merge` y =
    if aspect x == aspect y && phase x == phase y then
      KeepL merged
    else
      KeepBoth x y
    where
      merged = x {
          transitEnds = transitEnds y,
          transitAngle = transitAngle y,
          transitOrb = transitOrb y,
          transitProgress = transitProgress x <> transitProgress y
        }

type TransitMap = Grouped (Planet, Planet) Transit

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

interplanetaryTransits :: Monad m => St.Stream (St.Of (Ephemeris Double)) m b -> m (St.Of TransitMap b)
interplanetaryTransits =
  ephemerisWindows 2
  >>> St.foldMap mapTransits

selectTransits :: Monad m => [(Planet, Planet)] -> St.Stream (St.Of (Ephemeris Double)) m b -> m (St.Of TransitMap b)
selectTransits selectedTransits =
  ephemerisWindows 2 >>> St.foldMap (mapTransits' selectedTransits)

selectNatalTransits :: Monad m => Ephemeris Double -> [(Planet, Planet)] -> St.Stream (St.Of (Ephemeris Double)) m b -> m (St.Of TransitMap b)
selectNatalTransits natalEphemeris selectedTransits =
  ephemerisWindows 2 >>> St.foldMap (mapNatalTransits natalEphemeris selectedTransits)

mapTransits' :: [(Planet, Planet)] -> Seq (Ephemeris Double) -> TransitMap
mapTransits' chosenPairs (day1Ephe :<| day2Ephe :<| _) =
  concatForEach chosenPairs $ \pair@(planet1, planet2) ->
      let planet1Ephe1 = (epheDate day1Ephe,) <$> forPlanet planet1 day1Ephe
          planet1Ephe2 = (epheDate day2Ephe,) <$> forPlanet planet1 day2Ephe
          planet2Ephe2 = (epheDate day2Ephe,) <$> forPlanet planet2 day2Ephe
          planet1Ephes = liftA2 (,) planet1Ephe1 planet1Ephe2
          transit' = join $ mkTransit <$> planet1Ephes <*> planet2Ephe2
      in case transit' of
        Nothing -> mempty
        Just transit -> Aggregate $ M.fromList [(pair, singleton transit)]

mapTransits' _ _ = mempty

mapTransits :: Seq (Ephemeris Double) -> TransitMap
mapTransits = mapTransits' uniquePairs

mapNatalTransits :: Ephemeris Double -> [(Planet, Planet)] -> Seq (Ephemeris Double) -> TransitMap
mapNatalTransits natalEphemeris chosenPairs (day1Ephe :<| day2Ephe :<| _) =
  concatForEach chosenPairs $ \pair@(planet1, planet2) ->
      let planet1Ephe1 = (epheDate day1Ephe,) <$> forPlanet planet1 day1Ephe
          planet1Ephe2 = (epheDate day2Ephe,) <$> forPlanet planet1 day2Ephe

          planet2Ephe2 = (epheDate day2Ephe,) <$> staticPosition (forPlanet planet2 natalEphemeris)
          planet1Ephes = liftA2 (,) planet1Ephe1 planet1Ephe2
          transit' = join $ mkTransit <$> planet1Ephes <*> planet2Ephe2
      in case transit' of
        Nothing -> mempty
        Just transit -> Aggregate $ M.fromList [(pair, singleton transit)]

mapNatalTransits _ _ _ = mempty

staticPosition :: Maybe (EphemerisPosition Double) -> Maybe (EphemerisPosition Double)
staticPosition (Just pos) = Just $ pos{epheSpeed = 0.0}
staticPosition Nothing = Nothing


-- | All distinct pairings of  planets, with the one that's faster
-- on average as the first of the pair, always.
uniquePairs :: [(Planet, Planet)]
uniquePairs =
  [(p1, p2) | (p1:ps) <- tails defaultPlanets, p2 <- ps]

defaultPlanets :: [Planet]
defaultPlanets =
      [ Moon
      , Mercury
      , Venus
      , Sun
      , Mars
      , Jupiter
      , MeanApog
      , MeanNode
      , Saturn
      , Chiron
      , Uranus
      , Neptune
      , Pluto
      ]

-- All pairs, including a planet with itself -- useful for natal transits
allPairs :: [(Planet, Planet)]
allPairs =
  uniquePairs <> selfPairs
  where
    selfPairs = zip defaultPlanets defaultPlanets


mkTransit
  :: (EphemerisPoint, EphemerisPoint)
  -- ^ planet 1 at days 1->2
  -> EphemerisPoint
  -- ^ planet 2 at day 2
  -> Maybe Transit
mkTransit transiting@((t1, p11), (t2, p12)) _transited@(_t2', p22)
  = do
    let (before, after, transitedPos) = (epheLongitude p11, epheLongitude p12, epheLongitude p22)
    (aspectName, angle', orb', meets) <- determineAspect after transitedPos
    let (before', after', ref) = normalize (before, after, meets)
        station = movement transiting
        rel = relation before' after' ref
        phase = transitPhase station rel
    pure $ Transit aspectName phase angle' orb' t1 t2 [(t2,orb')]


-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------

-- | Given two moments of movement for two separate planets/body,
-- determine which is to be considered the "fastest"
isTransiting :: EphemerisPoint -> EphemerisPoint -> Bool
isTransiting (_,p12) (_,p22) =
  absSpeed p22 <= absSpeed p12

absSpeed :: EphemerisPosition Double -> Double
absSpeed = abs . epheSpeed

-- | Given two moments of movement for a given planet/body,
-- determine the "character" thereof
movement :: (EphemerisPoint, EphemerisPoint) -> Station
movement (_d1@(_t1, _p1), _d2@(_t2, p2))
  | isRelativelyStationary p2 =
    if signum (epheSpeed p2) > 0 then
      StationaryDirect
    else
      StationaryRetrograde
  | signum (epheSpeed p2) > 0 = Direct
  | otherwise = Retrograde

-- | Given two moments of a moving planet, and a reference point
-- determine if the moving planet remained above, below or crossed the
-- reference point
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

determineAspect :: Double -> Double -> Maybe (AspectName, Double, Double, Double)
determineAspect p1 p2 =
  headMaybe $ do
    asp <- aspects
    let dist = circleDistance p1 p2
        theta = angle asp
        orb = abs $ theta - dist
        crossA = p2 + theta
        crossB = p2 - theta
        crossingPoint = clampCircle $
          if circleDistance p1 crossA <= orb then crossA else crossB
    guard $ abs (theta - dist) <= maxOrb asp
    pure (aspectName asp, dist, orb, crossingPoint)

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe xs = Just . head $ xs

clampCircle :: Double -> Double
clampCircle n =
  rectified `mod'` 360
  where
    rectified = if n < 0 then n + 360 else n

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
