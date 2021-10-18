module Query.EventTypes where

import SwissEphemeris 
import Query.Common
import Query.Aggregate
import SwissEphemeris.Precalculated
import EclipticLongitude
import qualified Data.Sequence as S

data Event
  = DirectionChange  PlanetStation
  | ZodiacIngress    (Crossing Zodiac)
  | HouseIngress     (Crossing House)
  | PlanetaryTransit (Transit Planet)
  | HouseTransit     (Transit House)
  | LunarPhaseChange LunarPhase
  | EclipseMaximum   Eclipse

instance Merge Event where
  merge (DirectionChange a) (DirectionChange b) = 
    DirectionChange <$> (a `merge` b)
  merge (ZodiacIngress a) (ZodiacIngress b) =
    ZodiacIngress <$> (a `merge` b)
  merge (HouseIngress a) (HouseIngress b) =
    HouseIngress <$> (a `merge` b)
  merge (PlanetaryTransit a) (PlanetaryTransit b) =
    PlanetaryTransit <$> (a `merge` b)
  merge (HouseTransit a) (HouseTransit b) =
    HouseTransit <$> (a `merge` b)
  merge (LunarPhaseChange a) (LunarPhaseChange b) =
    LunarPhaseChange <$> (a `merge` b)
  merge (EclipseMaximum a) (EclipseMaximum b) =
    EclipseMaximum <$> (a `merge` b)
  merge _ _ = KeepBoth 
-------------------------------------------------------------------------------
data PlanetStation = PlanetStation
  { stationStarts :: !JulianDayTT
  , stationEnds :: !JulianDayTT
  , stationType :: !Station
  , stationPlanet :: !Planet
  }
  deriving (Eq, Show)

instance Merge PlanetStation where
  x `merge` y =
    if stationType y == stationType x && stationPlanet x == stationPlanet y then
      Merge merged
    else
      KeepBoth
    where
      merged = x {
        stationStarts = stationStarts x
      , stationEnds   = stationEnds y
      , stationType   = stationType y
      }

-------------------------------------------------------------------------------

data Crossing crossed = Crossing
  { crossingStarts :: !JulianDayTT
  , crossingEnds :: !JulianDayTT
  , crossingCrosses :: !crossed
  , crossingPlanet :: !Planet
  , crossingDirection :: !PlanetMotion
  } deriving (Eq, Show)

instance Eq a => Merge (Crossing a) where
  x `merge` y =
    if crossingCrosses x == crossingCrosses y && crossingPlanet x == crossingPlanet y then
      Merge merged
    else
      KeepBoth
    where
      merged = x {
        crossingEnds = crossingEnds y,
        crossingDirection = crossingDirection y
      }

data Zodiac = Zodiac
  { signName :: ZodiacSignName, signLng :: Double}
  deriving (Eq, Show)

instance HasEclipticLongitude Zodiac where
  getEclipticLongitude (Zodiac _ l) = l

data HouseName
  = I
  | II
  | III
  | IV
  | V
  | VI
  | VII
  | VIII
  | IX
  | X
  | XI
  | XII
  deriving (Eq, Show, Enum, Ord)

data House = House
  { houseName :: HouseName, houseCusp :: Double }
  deriving (Eq, Show)
  
instance HasEclipticLongitude House where
  getEclipticLongitude = houseCusp

instance Ord House where
  a `compare` b = compare (houseCusp a) (houseCusp b)

-------------------------------------------------------------------------------

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

data Transit over = Transit {
  aspect :: !AspectName
, transiting :: !Planet
, transited :: !over
, lastPhase :: !TransitPhaseName
, transitAngle :: !Double
, transitOrb :: !Double
, transitStarts :: !JulianDayTT
, transitEnds :: !JulianDayTT
, transitProgress :: !(S.Seq (JulianDayTT, Double))
, transitPhases :: !(MergeSeq TransitPhase)
, transitIsExact :: ![JulianDayTT]
, transitCrosses :: !EclipticLongitude
} deriving (Show)

instance Eq a => Merge (Transit a) where
  x `merge` y =
    if aspect x == aspect y && transiting x == transiting y && transited x == transited y then
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

-------------------------------------------------------------------------------

data LunarPhase = LunarPhase
  { lunarPhaseName :: !LunarPhaseName
  , lunarPhaseStarts :: !JulianDayTT
  , lunarPhaseEnds :: !JulianDayTT
  } deriving (Eq, Show)

instance Merge LunarPhase where
  x `merge` y =
    if lunarPhaseName x == lunarPhaseName y then
      Merge merged
    else
      KeepBoth
    where
      merged = x {
        lunarPhaseEnds = lunarPhaseEnds y
      }

-------------------------------------------------------------------------------

data Eclipse 
  = SolarEclipse !SolarEclipseType !JulianDayUT1 
  | LunarEclipse !LunarEclipseType !JulianDayUT1
  deriving (Eq, Show)
 
instance Merge Eclipse where
  merge _ _= KeepBoth 
