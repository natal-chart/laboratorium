{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
module Query.Retrograde where

import Data.Sequence (Seq(..), ViewL(..))
import qualified Data.Map as M
import SwissEphemeris
import SwissEphemeris.Precalculated
import Data.Foldable (toList)
import Query.Common
    ( isRelativelyStationary, Station(..), concatForEach )
import Query.Aggregate ( Aggregate(Aggregate), Grouped, singleton )
import Streaming ( Of, Stream )
import qualified Streaming.Prelude as St
import Query.Streaming ( ephemerisWindows )
import Control.Arrow ((>>>))
import Query.EventTypes

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

-- | Given ephemeris for two consecutive days, determine if a planet has changed
-- direction, or entered a stationary phase.
getRetrogrades :: Seq (Ephemeris Double) -> Grouped Planet Event
getRetrogrades (pos1 :<| pos2 :<| _) =
  concatForEach (zip (toList $ ephePositions pos1) (toList $ ephePositions pos2)) $ \(p1, p2) ->
    case mkStation (epheDate pos1, p1) (epheDate pos2, p2) of
     Nothing -> mempty
     Just st -> 
       -- the MeanNode /appears/ direct/retrograde sometimes,
       -- but that's not astrologically significant.
       if ephePlanet p1 `elem` [MeanNode, TrueNode] then
         mempty
       else
         Aggregate $ M.fromList [(ephePlanet p1, singleton (DirectionChange st))]

getRetrogrades _ = mempty


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
      stationPlanet = ephePlanet pos1,
      stationType = if epheSpeed pos1 > epheSpeed pos2 then Retrograde else Direct
     }
  | isRelativelyStationary pos1 =
    Just $ PlanetStation {
      stationStarts  = d1,
      stationEnds = d2,
      stationPlanet = ephePlanet pos1,
      stationType = if epheSpeed pos1 > epheSpeed pos2 then StationaryRetrograde else StationaryDirect
      }
  | otherwise =
  Nothing
