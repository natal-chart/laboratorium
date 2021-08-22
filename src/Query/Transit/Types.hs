{-# LANGUAGE TypeFamilies #-}
module Query.Transit.Types where

import SwissEphemeris ( Planet, JulianDayTT )
import SwissEphemeris.Precalculated ( EphemerisPosition )
import Query.Aggregate
    ( Grouped,
      MergeSeq,
      MergeStrategy(KeepBoth, Merge),
      Merge(..), Temporal(..)
    )
import qualified Data.Sequence as S
import EclipticLongitude
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
, transitCrosses :: !EclipticLongitude
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
          transitPhases = transitPhases x <> transitPhases y
        }

instance Temporal Transit where
  type TemporalIndex Transit = JulianDayTT 
  startTime = transitStarts
  endTime = transitEnds

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
