{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
module Query.PlanetEphe where

import qualified Data.Map as M
import SwissEphemeris.Precalculated
import Data.Time
import SwissEphemeris
import qualified Data.Sequence as S
import Query.Aggregate
import qualified Streaming.Prelude as S
import Streaming (MonadIO (liftIO))
import Query.Common (concatForEach)
import Data.Foldable (toList)
import Control.Category ((>>>))

type EphemerisPosition' = EphemerisPosition Double

newtype PlanetPositionSeq =
  PlanetPositionSeq {getPlanetPositions :: S.Seq (UTCTime, EphemerisPosition')}
  deriving (Semigroup, Monoid) via (S.Seq (UTCTime, EphemerisPosition'))

singleton :: (UTCTime, EphemerisPosition') -> PlanetPositionSeq
singleton = PlanetPositionSeq . S.singleton

instance HasUnion PlanetPositionSeq where
  (PlanetPositionSeq s1) `union` (PlanetPositionSeq s2) =
    PlanetPositionSeq $ s1 <> s2

type PlanetEphe = Aggregate Planet PlanetPositionSeq

planetEphemeris :: MonadIO m => S.Stream (S.Of (Ephemeris Double)) m () -> m (S.Of PlanetEphe ())
planetEphemeris =
  S.mapM withUTC
  >>> S.foldMap mapPlanets

withUTC :: MonadIO m => Ephemeris Double -> m (UTCTime, Ephemeris Double)
withUTC ephe = do
  ut <- liftIO . fromJulianDay $ epheDate ephe
  pure (ut, ephe)

mapPlanets :: (UTCTime, Ephemeris Double) -> PlanetEphe
mapPlanets (ut, ephe) =
  concatForEach (toList $ ephePositions ephe) $ \pos ->
    Aggregate $ M.fromList [(ephePlanet pos, singleton (ut, pos))]
