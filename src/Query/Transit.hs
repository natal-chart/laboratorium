{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}

module Query.Transit where

import Data.Sequence (Seq ((:<|)), fromList)
import qualified Data.Sequence as S
import qualified Streaming.Prelude as St
import qualified Data.Map as M
import SwissEphemeris
import SwissEphemeris.Precalculated
import Query.Common
import Control.Lens (over, Each (each))
import Data.Fixed (mod')
import Control.Monad (guard, join)
import Data.List (tails, nub)
import Control.Applicative (liftA2)
import Query.Aggregate
import Query.Streaming
import Control.Category ((>>>))
import EclipticLongitude
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import Data.Foldable (foldMap')

type EphemerisPoint = (JulianDayTT, EphemerisPosition Double)

data Relation
  = Below
  | Crossed
  | Above
  deriving (Eq, Show)

data TransitPhaseName
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

data TransitPhase = TransitPhase {
  phaseName :: !TransitPhaseName
, phaseStarts :: !JulianDayTT
, phaseEnds :: !JulianDayTT
} deriving (Eq, Show)

instance Merge TransitPhase where
  x `merge` y =
    if phaseName x == phaseName y then
      Merge merged
    else
      KeepBoth
    where
      merged = x {
        phaseEnds = phaseEnds y
      }

data Transit = Transit {
  aspect :: !AspectName
, lastPhase :: !TransitPhaseName
, transitAngle :: !Double
, transitOrb :: !Double
, transitStarts :: !JulianDayTT
, transitEnds :: !JulianDayTT
, transitProgress :: !(S.Seq (JulianDayTT, Double))
, transitPhases :: !(MergeSeq TransitPhase)
, transitIsExact :: !(Maybe JulianDayTT)
} deriving (Show)

instance Merge Transit where
  x `merge` y =
    if aspect x == aspect y then
      Merge merged
    else
      KeepBoth
    where
      merged = x {
          lastPhase = lastPhase y,
          transitEnds = transitEnds y,
          transitAngle = transitAngle y,
          transitOrb = transitOrb y,
          transitProgress = transitProgress x <> transitProgress y,
          transitPhases = transitPhases x `union` transitPhases y
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
      let planet1Ephe1 = (epheDate day1Ephe,) <$> planetEphe planet1 day1Ephe
          planet1Ephe2 = (epheDate day2Ephe,) <$> planetEphe planet1 day2Ephe
          planet2Ephe2 = (epheDate day2Ephe,) <$> planetEphe planet2 day2Ephe
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
      let planet1Ephe1 = (epheDate day1Ephe,) <$> planetEphe planet1 day1Ephe
          planet1Ephe2 = (epheDate day2Ephe,) <$> planetEphe planet1 day2Ephe

          planet2Ephe2 = (epheDate day2Ephe,) <$> staticPosition (planetEphe planet2 natalEphemeris)
          planet1Ephes = liftA2 (,) planet1Ephe1 planet1Ephe2
          transit' = join $ mkTransit <$> planet1Ephes <*> planet2Ephe2
      in case transit' of
        Nothing -> mempty
        Just transit -> Aggregate $ M.fromList [(pair, singleton transit)]

mapNatalTransits _ _ _ = mempty

staticPosition :: Maybe (EphemerisPosition Double) -> Maybe (EphemerisPosition Double)
staticPosition (Just pos) = Just $ pos{epheSpeed = 0.0}
staticPosition Nothing = Nothing

selectLunarTransits :: JulianDayTT -> JulianDayTT -> Ephemeris Double -> IO TransitMap
selectLunarTransits start end natalEphemeris =
  foldMap' mkLunarTransit (ephePositions natalEphemeris)
  where
    mkLunarTransit :: EphemerisPosition Double -> IO TransitMap
    mkLunarTransit pos = do
      transit <- lunarAspects start end pos --(EclipticLongitude . epheLongitude $ pos)
      if null transit then
        mempty
      else
        pure $ Aggregate $ M.fromList [((Moon, ephePlanet pos), MergeSeq $ fromList transit)]

-- | All distinct pairings of  planets, with the one that's faster
-- on average as the first of the pair, always.
uniquePairs :: [(Planet, Planet)]
uniquePairs =
  [(p1, p2) | (p1:ps) <- tails defaultPlanets, p2 <- ps]

{-
The default sorting of planets here was obtained by looking at 100 years of average speeds:

cabal new-run laboratorium -- query -q "Averages" -s "1989-01-01" -e "2089-01-01" --ephe-path "./ephe"
Up to date
[(Moon,13.176522281580842),
(Mercury,1.2173611188617248),
(Venus,1.042309783743218),
(Sun,0.9856478045400626),
(Mars,0.5679595888524764),
(Jupiter,0.13204562470426282),
(MeanApog,0.11140269708380175),
(Saturn,6.881223337573507e-2),
(MeanNode,5.295424163793801e-2),
(Chiron,5.2216388904251725e-2),
(Uranus,3.229203526261477e-2),
(Neptune,2.112966146937543e-2),
(Pluto,2.060110471243601e-2)]

-}

defaultPlanets :: [Planet]
defaultPlanets =
      [ Moon
      , Mercury
      , Venus
      , Sun
      , Mars
      , Jupiter
      , MeanApog
      , Saturn
      , MeanNode
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
        phaseInfo = singleton $ TransitPhase phase t1 t2
    pure $ Transit aspectName phase angle' orb' t1 t2 [(t2,orb')] phaseInfo Nothing


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


transitPhase :: Station -> Relation -> TransitPhaseName
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
  headMaybe $ aspectCycle (EclipticLongitude p1) (EclipticLongitude p2)

aspectCycle :: EclipticLongitude -> EclipticLongitude -> [(AspectName, Double, Double, Double)]
aspectCycle p1 p2 = do
  asp <- aspects
  let dist = p1 <-> p2
      theta = angle asp
      orb = abs $ theta - dist
      crossA = p2 + EclipticLongitude theta
      crossB = p2 - EclipticLongitude theta
      crossesAt =
        if p1 <-> crossA <= orb then crossA else crossB
  guard $ abs (theta - dist) <= maxOrb asp
  pure (aspectName asp, dist, orb, getEclipticLongitude crossesAt)

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

-------------------------------------------------------------------------------
-- LUNAR ASPECTS
-------------------------------------------------------------------------------

-- | We use a different approach to lunar aspects: since the moon can move up to
-- 15 degrees per day, iterating over all ephemeris in an interval in day-wise steps
-- is too coarse: an entire applying/trigger/separating cycle can fall through!
-- Instead, we use the `moonCrossing` function provided by SwissEphemeris, which
-- does somewhat more clever interpolation with non-precalc ephemeris to find the
-- next time the moon will cross a longitude -- made possible by the fact that the Moon
-- never presents retrograde motion from a geocentric perspective. By the same token,
-- once we know when the aspect is triggered exactly, we can estimate a rough application/
-- separation interval without consulting any ephemeris. We incur many more IO actions here,
-- so lunar aspects should be reserved to daily/montly use cases, vs. heavier weight yearly
-- use cases that would not only do a lot of IO, also end up with a lot of results (since
-- in a given month, the Moon is likely to present almost every aspect.)
--
-- A similar function could be written for the Sun, with the appropriate mean solar speed.
lunarAspects
  :: HasEclipticLongitude a
  => JulianDayTT
  -> JulianDayTT
  -> a
  -> IO [Transit]
lunarAspects start end pos =
  mapM crossings aspects
  <&> catMaybes
  <&> concat
  <&> filter inRange
  <&> map toTransit
  where
    crossings :: Aspect -> IO (Maybe [(AspectName, Double, JulianDayTT)])
    crossings Aspect{aspectName, angle} = do
      let crossA = toEclipticLongitude pos + EclipticLongitude angle
          crossB = toEclipticLongitude pos - EclipticLongitude angle
      crossesA <- moonCrossing (getEclipticLongitude crossA) start
      crossesB <- moonCrossing (getEclipticLongitude crossB) start
      case (crossesA, crossesB) of
        (Left _, _) -> pure Nothing
        (_, Left _) -> pure Nothing
        (Right xA, Right xB) ->
          pure . Just $ nub [(aspectName, angle, xA), (aspectName, angle, xB)]

    inRange (_, _, aspectDate) =
      getJulianDay aspectDate >= getJulianDay start
      && getJulianDay aspectDate < getJulianDay end

    toTransit (aspname, angl, exactCrossingTime) =
      Transit {
        aspect = aspname,
        lastPhase = ApplyingDirect,
        transitAngle = angl,
        transitOrb = 0,
        transitStarts = estimateStart exactCrossingTime,
        transitIsExact = Just exactCrossingTime,
        transitEnds = estimateEnd exactCrossingTime,
        transitProgress = mempty,
        transitPhases = mempty
      }
    -- from:
    -- https://github.com/aloistr/swisseph/blob/40a0baa743a7c654f0ae3331c5d9170ca1da6a6a/sweph.c#L8494
    meanLunarSpeed = 360.0 / 27.32
    -- given the mean lunar speed, estimate when the Moon was 5 degrees before
    -- and 5 degrees after the moment of exact crossing
    estimateStart t' =
      mkJulianDay STT tBefore
      where
        tBefore = getJulianDay t' - 5/meanLunarSpeed
    estimateEnd t' =
      mkJulianDay STT tAfter
      where
        tAfter = getJulianDay t' + 5/meanLunarSpeed
