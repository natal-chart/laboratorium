module OptionParser where

import Options.Applicative ( eitherReader, ReadM )
import Data.Time ( Day, UTCTime )
import Data.Time.Format.ISO8601 ( iso8601ParseM )

dayReader :: ReadM Day
dayReader = eitherReader $ \arg ->
  case iso8601ParseM arg of
    Nothing -> Left $ "Invalid date: " <> arg
    Just day -> Right day

datetimeReader :: ReadM UTCTime
datetimeReader = eitherReader $ \arg ->
  case iso8601ParseM arg of
    Nothing -> Left $ "Invalid UTC timestamp: " <> arg
    Just ts -> Right ts
