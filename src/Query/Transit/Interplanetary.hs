{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedLists #-}

module Query.Transit.Interplanetary where
import Query.Transit.Types
    ( TransitMap,
      Transit(Transit),
      TransitPhase(TransitPhase),
      Aspect(..),
      AspectName,
      TransitPhaseName(..),
      Relation(..),
      EphemerisPoint,
      aspects )

import Data.Sequence (Seq ((:<|)))
import qualified Streaming.Prelude as St
import qualified Data.Map as M
import SwissEphemeris
    ( HasEclipticLongitude(getEclipticLongitude),
      Planet(Pluto, Moon, Mercury, Venus, Sun, Mars, Jupiter, MeanApog,
             Saturn, MeanNode, Chiron, Uranus, Neptune) )
import SwissEphemeris.Precalculated
    ( planetEphe,
      Ephemeris(epheDate),
      EphemerisPosition(epheLongitude, epheSpeed) )
import Query.Common
    ( Station(..), isRelativelyStationary, concatForEach )
import Control.Lens (over, Each (each))
import Data.Fixed (mod')
import Control.Monad (guard, join)
import Data.List (tails)
import Control.Applicative (liftA2)
import Query.Aggregate ( Aggregate(Aggregate), singleton )
import Query.Streaming ( ephemerisWindows )
import Control.Category ((>>>))
import EclipticLongitude ( EclipticLongitude(..), (<->) )

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
