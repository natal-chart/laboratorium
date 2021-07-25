{-# LANGUAGE NamedFieldPuns #-}

module Query.Common where

import SwissEphemeris.Precalculated
import SwissEphemeris
import qualified Data.Sequence as S
import Data.Foldable (toList)
import Data.Sequence ((|>))

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

-- | Flipped 'map'. I've done too much Javascript.
forEach :: [a] -> (a -> b) -> [b]
forEach = flip map
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
