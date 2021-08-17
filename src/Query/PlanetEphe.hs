{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
module Query.PlanetEphe where

import qualified Data.Map as M
import SwissEphemeris.Precalculated
import Data.Time
import SwissEphemeris
import qualified Data.Sequence as Sq
import Query.Aggregate
import qualified Streaming.Prelude as S
import Streaming (MonadIO (liftIO))
import Query.Common (concatForEach)
import Data.Foldable (toList)
import Control.Category ((>>>))

type EphemerisPosition' = EphemerisPosition Double
type PlanetPosition = (UTCTime, EphemerisPosition')
type PlanetPositionSeq = Sq.Seq PlanetPosition

type PlanetEphe = Aggregate Planet PlanetPositionSeq

type PlanetSpeedAggr = Aggregate Planet PlanetSpeed

data PlanetSpeed = PlanetSpeed !Double !Double

instance Semigroup PlanetSpeed where
  (PlanetSpeed spd1 count1) <> (PlanetSpeed spd2 count2) =
    PlanetSpeed (abs spd1 + abs spd2) (count1 + count2)

planetSpeedAverage :: PlanetSpeed -> Double
planetSpeedAverage (PlanetSpeed speedSum speedCount) =
  speedSum / speedCount

-- | a 'singleton' for a speed datum
acc :: Double -> PlanetSpeed
acc spd = PlanetSpeed spd 1

planetEphemeris :: MonadIO m => [Planet] -> S.Stream (S.Of (Ephemeris Double)) m () -> m (S.Of PlanetEphe ())
planetEphemeris selectedPlanets =
  S.mapM withUTC
  >>> S.foldMap (mapPlanets selectedPlanets)

withUTC :: MonadIO m => Ephemeris Double -> m (UTCTime, Ephemeris Double)
withUTC ephe = do
  ut <- liftIO . fromJulianDay $ epheDate ephe
  pure (ut, ephe)

mapPlanets :: [Planet] -> (UTCTime, Ephemeris Double) -> PlanetEphe
mapPlanets selectedPlanets (ut, ephe) =
  concatForEach (toList $ ephePositions ephe) $ \pos ->
    if ephePlanet pos `elem` selectedPlanets then
      Aggregate $ M.fromList [(ephePlanet pos, Sq.singleton (ut, pos))]
    else
      mempty

planetAverageSpeeds
  :: MonadIO m
  => [Planet]
  -> S.Stream (S.Of (Ephemeris Double)) m () -> m (S.Of PlanetSpeedAggr ())
planetAverageSpeeds selectedPlanets = S.foldMap (mapAverages selectedPlanets)

mapAverages :: [Planet] -> Ephemeris Double -> PlanetSpeedAggr
mapAverages selectedPlanets ephe =
  concatForEach selectedPlanets $ \planet ->
    case ephe `forPlanet` planet of
      Nothing -> mempty
      Just EphemerisPosition{epheSpeed} ->
        Aggregate $ M.fromList [(planet, acc epheSpeed)]

getAverages :: S.Of PlanetSpeedAggr () -> M.Map Planet Double
getAverages (allAggr S.:> _) =
  planetSpeedAverage <$> getAggregate allAggr
