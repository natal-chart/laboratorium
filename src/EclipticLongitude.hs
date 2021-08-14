{-# LANGUAGE FlexibleInstances #-}
module EclipticLongitude (
  EclipticLongitude(..),
  (<->),
  toEclipticLongitude
) where

import SwissEphemeris ( HasEclipticLongitude(..) )
import Data.Fixed (mod')
import SwissEphemeris.Precalculated

newtype EclipticLongitude = 
  EclipticLongitude Double
  deriving (Eq, Show)
  
instance HasEclipticLongitude EclipticLongitude where
  getEclipticLongitude (EclipticLongitude l) = l
  
instance Num EclipticLongitude where
  (EclipticLongitude el1) + (EclipticLongitude el2) =
    clampLng $ el1 + el2  
  (EclipticLongitude el1) * (EclipticLongitude el2) =
    clampLng $ el1 * el2  
  (EclipticLongitude el1) - (EclipticLongitude el2) =
    clampLng $ el1 - el2
  abs = EclipticLongitude . abs . getEclipticLongitude
  signum = EclipticLongitude . signum . getEclipticLongitude
  fromInteger = EclipticLongitude . fromInteger 

clampLng :: Double -> EclipticLongitude
clampLng n =
  EclipticLongitude $ rectified `mod'` 360
  where
    rectified = if n < 0 then n + 360 else n
    
shortestDistance :: EclipticLongitude -> EclipticLongitude -> Double
shortestDistance (EclipticLongitude a) (EclipticLongitude b) =
  180 - abs(abs(a - b) `mod'` 360 - 180)
  
infixl 6 <->
(<->) :: EclipticLongitude -> EclipticLongitude -> Double 
(<->) = shortestDistance

toEclipticLongitude :: HasEclipticLongitude a => a -> EclipticLongitude
toEclipticLongitude = EclipticLongitude . getEclipticLongitude

-- TODO move to swiss ephemeris
instance (Real a, Eq a) => HasEclipticLongitude (EphemerisPosition a) where
  getEclipticLongitude = realToFrac . epheLongitude
