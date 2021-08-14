{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
module Query.Retrograde where

import Data.Sequence (Seq(..), ViewL(..))
import qualified Data.Map as M
import SwissEphemeris
import SwissEphemeris.Precalculated
import Data.Foldable (toList)
import Query.Common
    ( isRelativelyStationary, Station(..), concatForEach )
import Query.Aggregate ( Aggregate(Aggregate), Merge (merge), MergeStrategy(..), Grouped, singleton )
import Streaming ( Of, Stream )
import qualified Streaming.Prelude as St
import Query.Streaming ( ephemerisWindows )
import Control.Arrow ((>>>))

data PlanetStation = PlanetStation
  { stationStarts :: !JulianDayTT
  , stationEnds :: !JulianDayTT
  , stationType :: !Station
  }
  deriving (Eq, Show)

instance Merge PlanetStation where
  x `merge` y =
    if stationType y == stationType x then
      Merge merged
    else
      KeepBoth
    where
      merged = PlanetStation {
        stationStarts = stationStarts x
      , stationEnds   = stationEnds y
      , stationType   = stationType y
      }

type RetrogradeMap = Grouped Planet PlanetStation

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
