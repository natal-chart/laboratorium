{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Query.Retrograde where

import Data.Sequence ((><), Seq(..), (|>), (<|), ViewL(..), ViewR(..))
import qualified Data.Sequence as S
import qualified Data.Map as M
import SwissEphemeris
import SwissEphemeris.Precalculated
import Data.Foldable (toList, Foldable (foldMap'))

data Station
  = StationaryRetrograde
  | StationaryDirect
  | Retrograde
  | Direct
  deriving (Eq, Show)

data PlanetStation = PlanetStation
  { stationStarts :: !JulianDayTT
  , stationEnds :: !JulianDayTT
  , stationType :: !Station
  }
  deriving (Show)

newtype PlanetStationSeq =
  PlanetStationSeq {getStations :: S.Seq PlanetStation}
  deriving (Show)

singleton :: PlanetStation -> PlanetStationSeq
singleton = PlanetStationSeq . S.singleton

instance Semigroup PlanetStationSeq where
  (PlanetStationSeq s1) <> (PlanetStationSeq s2) =
    let s1Last = S.viewr s1
        s2First = S.viewl s2
    in if isStationary s2First then
      PlanetStationSeq $ mergeStationary s1Last s2First
    else
      PlanetStationSeq $ s1 <> s2

instance Monoid PlanetStationSeq where
  mempty = PlanetStationSeq S.empty

newtype RetrogradeMap =
  RetrogradeMap {getRetrogradeMap :: M.Map Planet PlanetStationSeq}
  deriving (Show)

instance Semigroup RetrogradeMap where
  (RetrogradeMap p1) <> (RetrogradeMap p2) =
    RetrogradeMap $ M.unionWith (<>) p1 p2

instance Monoid RetrogradeMap where
  mempty = RetrogradeMap M.empty

foldRetrograde :: [[Either String (Ephemeris Double)]] -> RetrogradeMap
foldRetrograde = foldMap' $ \case
    [] -> RetrogradeMap M.empty
    [_] -> RetrogradeMap M.empty
    (Left _:_) -> RetrogradeMap M.empty
    (_:Left _:_) -> RetrogradeMap M.empty
    (Right pos1:Right pos2:_) ->
      mconcat $ flip map (zip (toList $ ephePositions pos1) (toList $ ephePositions pos2)) $ \(p1, p2) ->
        case mkStation (epheDate pos1, p1) (epheDate pos2, p2) of
          Nothing -> RetrogradeMap M.empty
          Just st -> 
            -- the MeanNode /appears/ direct/retrograde sometimes,
            -- but that's not astrologically significant.
            if ephePlanet p1 `elem` [MeanNode, TrueNode] then
              RetrogradeMap M.empty
            else
              RetrogradeMap $ M.fromList [(ephePlanet p1, singleton st)]


isStationary :: ViewL PlanetStation -> Bool
isStationary EmptyL = False
isStationary (PlanetStation{stationType} :< _) =
  stationType `elem` [StationaryDirect, StationaryRetrograde]

mergeStationary :: ViewR PlanetStation -> ViewL PlanetStation -> Seq PlanetStation
mergeStationary EmptyR EmptyL = S.empty
mergeStationary EmptyR (x :< xs) = x <| xs
mergeStationary (xs :> x) EmptyL = xs |> x
mergeStationary (xs :> x) (y :< ys) =
  if stationType y == stationType x then
    (xs |> merged) ><  ys
  else
    (xs |> x) >< (y <| ys)
  where
    merged = PlanetStation {
      stationStarts = stationStarts x
    , stationEnds   = stationEnds y
    , stationType   = stationType y
    }

mkStation :: (JulianDayTT, EphemerisPosition Double) -> (JulianDayTT, EphemerisPosition Double) -> Maybe PlanetStation
mkStation (d1, pos1) (d2, pos2)
  | signum (epheSpeed pos1) /= signum (epheSpeed pos2) =
    Just $ PlanetStation {
      stationStarts = d1,
      stationEnds = d2,
      stationType = if epheSpeed pos1 > epheSpeed pos2 then Retrograde else Direct
     }
  | isRelativelyStationary pos1 =
    Just $ PlanetStation {
      stationStarts  = d1,
      stationEnds = d2,
      stationType = if epheSpeed pos1 > epheSpeed pos2 then StationaryRetrograde else StationaryDirect
      }
  | otherwise =
  Nothing

-- see: https://www.astro.com/astrowiki/en/Stationary_Phase
-- and: https://groups.io/g/swisseph/message/2975
{-
Mercury	5' or 300"/ day
Venus	3' or 180"/ day
Mars	90"/ day
Jupiter	60"/ day
Saturn	60"/ day
Chiron	20"/ day
Uranus	20"/ day
Neptune	10"/ day
Pluto	10"/ day
-}

isRelativelyStationary :: EphemerisPosition Double -> Bool
isRelativelyStationary EphemerisPosition{ephePlanet, epheSpeed} =
  case ephePlanet of
    Mercury -> degToSec epheSpeed <= 300
    Venus -> degToSec epheSpeed <= 180
    Mars -> degToSec epheSpeed <= 90
    Jupiter -> degToSec epheSpeed <= 60
    Saturn -> degToSec epheSpeed <= 60
    Chiron -> degToSec epheSpeed <= 20
    Uranus -> degToSec epheSpeed <= 20
    Neptune -> degToSec epheSpeed <= 10
    Pluto -> degToSec epheSpeed <= 10 
    _ -> degToSec epheSpeed <= 0

-- | Convert a degree unit to arcseconds
degToSec :: Double -> Double
degToSec = abs . (*3600)

-- from:
-- https://stackoverflow.com/a/27727244
windows :: Int -> [a] -> [[a]]
windows n0 = go 0 S.empty
  where
    go n s (a:as) | n' <  n0   =              go n' s'  as
                  | n' == n0   = toList s'  : go n' s'  as
                  | otherwise =  toList s'' : go n  s'' as
      where
        n'  = n + 1         -- O(1)
        s'  = s |> a        -- O(1)
        s'' = S.drop 1 s' -- O(1)
    go _ _ [] = []