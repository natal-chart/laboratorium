module Util where

import Data.Time ( Day, toGregorian, getCurrentTime )
import SwissEphemeris ( JulianDayTT, gregorianToFakeJulianDayTT, ToJulianDay (toJulianDay) )

-- | Get all days in the given range, as @JulianDayTT@s
julianDays :: Day -> Day -> [JulianDayTT]
julianDays startDay endDay =
  [start .. end]
  where
    (startY, startM, startD) = toGregorian startDay
    (endY, endM, endD) = toGregorian endDay
    start = gregorianToFakeJulianDayTT startY startM startD 0
    end = gregorianToFakeJulianDayTT endY endM endD 0

julianToday :: IO JulianDayTT
julianToday = do
  todayUT <- getCurrentTime 
  Just jd <- toJulianDay todayUT
  pure jd

show' :: Show a => a -> String
show' = (<> " ") . show
