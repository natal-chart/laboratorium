module Query.Streaming (
  streamEphe,
  streamEpheF,
  streamEpheJD,
  streamEpheJDF,
  ephemerisWindows,
  withUTC
) where


import Streaming (lift, MonadIO (liftIO))
import qualified Streaming.Prelude as S
import Data.Time
import SwissEphemeris.Precalculated
import Query.Common
import Data.Function
import qualified Data.Sequence as Sq
import SwissEphemeris (fromJulianDay, JulianDayTT)

-- | Given start and end dates, produce a stream of
-- 'Ephemeris'.
streamEphe :: MonadIO m => (String -> m x)
  -> Day
  -> Day
  -> S.Stream (S.Of (Ephemeris Double)) m ()
streamEphe onError start end =
  S.each (julianDayRange start end)
  & S.mapM (liftIO . readEphemerisEasy False)
  & S.partitionEithers
  -- thanks, ocharles:
  -- https://www.reddit.com/r/haskell/comments/5x2g0r/streaming_package_vs_pipes_conduit_question_on/def39od?utm_source=share&utm_medium=web2x&context=3
  & S.mapM_ (lift . onError)

streamEpheF :: (MonadIO m, MonadFail m) => Day -> Day -> S.Stream (S.Of (Ephemeris Double)) m ()
streamEpheF = streamEphe fail

streamEpheJD :: MonadIO m => (String -> m x)
  -> JulianDayTT
  -> JulianDayTT
  -> S.Stream (S.Of (Ephemeris Double)) m ()
streamEpheJD onError start end =
  S.each [start .. end]
  & S.mapM (liftIO . readEphemerisEasy False)
  & S.partitionEithers
  -- thanks, ocharles:
  -- https://www.reddit.com/r/haskell/comments/5x2g0r/streaming_package_vs_pipes_conduit_question_on/def39od?utm_source=share&utm_medium=web2x&context=3
  & S.mapM_ (lift . onError)

streamEpheJDF :: (MonadIO m, MonadFail m) => JulianDayTT -> JulianDayTT -> S.Stream (S.Of (Ephemeris Double)) m ()
streamEpheJDF = streamEpheJD fail


-- | Given a stream of ephemeris, produce "windowed"
-- steps
ephemerisWindows :: Monad m =>
  Int
  -> S.Stream (S.Of (Ephemeris Double)) m b
  -> S.Stream (S.Of (Sq.Seq (Ephemeris Double))) m b
ephemerisWindows = S.slidingWindow

withUTC :: MonadIO m => Ephemeris Double -> m (UTCTime, Ephemeris Double)
withUTC ephe = do
  ut <- liftIO . fromJulianDay $ epheDate ephe
  pure (ut, ephe)
