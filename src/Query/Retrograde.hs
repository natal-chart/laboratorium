{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
module Query.Retrograde where

import Data.Sequence ((><), Seq(..), (|>), (<|), ViewL(..), ViewR(..))
import qualified Data.Sequence as S
import qualified Data.Map as M
import SwissEphemeris
import SwissEphemeris.Precalculated
import Data.Foldable (toList)
import Query.Common
    ( isRelativelyStationary, Station(..), concatForEach )
import Query.Aggregate ( Aggregate(Aggregate), HasUnion(..) )
import Streaming ( Of, Stream )
import qualified Streaming.Prelude as St
import Query.Streaming ( ephemerisWindows )
import Control.Arrow ((>>>))

data PlanetStation = PlanetStation
  { stationStarts :: !JulianDayTT
  , stationEnds :: !JulianDayTT
  , stationType :: !Station
  }
  deriving (Show)

newtype PlanetStationSeq =
  PlanetStationSeq {getStations :: S.Seq PlanetStation}
  deriving stock (Show)
  deriving (Semigroup , Monoid) via (S.Seq PlanetStation)

singleton :: PlanetStation -> PlanetStationSeq
singleton = PlanetStationSeq . S.singleton

instance HasUnion PlanetStationSeq where
  (PlanetStationSeq s1) `union` (PlanetStationSeq s2) =
    let s1Last = S.viewr s1
        s2First = S.viewl s2
    in if isStationary s2First then
      PlanetStationSeq $ mergeStationary s1Last s2First
    else
      PlanetStationSeq $ s1 <> s2

type RetrogradeMap = Aggregate Planet PlanetStationSeq

retrogrades :: Monad m => Stream (Of (Ephemeris Double)) m b -> m (Of RetrogradeMap b)
retrogrades =
  ephemerisWindows 2 >>> St.foldMap mapRetrogrades

mapRetrogrades :: Seq (Ephemeris Double) -> RetrogradeMap
mapRetrogrades (pos1 :<| pos2 :<| _) =
  concatForEach (zip (toList $ ephePositions pos1) (toList $ ephePositions pos2)) $ \(p1, p2) ->
    case mkStation (epheDate pos1, p1) (epheDate pos2, p2) of
     Nothing -> mempty
     Just st -> 
       -- the MeanNode /appears/ direct/retrograde sometimes,
       -- but that's not astrologically significant.
       if ephePlanet p1 `elem` [MeanNode, TrueNode] then
         mempty
       else
         Aggregate $ M.fromList [(ephePlanet p1, singleton st)]

mapRetrogrades _ = mempty

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
