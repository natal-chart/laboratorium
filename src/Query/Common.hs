{-# LANGUAGE NamedFieldPuns #-}

module Query.Common where

import SwissEphemeris.Precalculated
import SwissEphemeris

data Station
  = StationaryRetrograde
  | StationaryDirect
  | Retrograde
  | Direct
  deriving (Eq, Show)

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
