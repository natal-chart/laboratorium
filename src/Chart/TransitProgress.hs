{-# LANGUAGE NamedFieldPuns #-}
module Chart.TransitProgress where

import Data.Foldable (forM_, Foldable (toList))
import Data.Time
  ( UTCTime, Day, getCurrentTime
  )
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import Graphics.Rendering.Chart.Easy hiding (days)
import SwissEphemeris
  ( FromJulianDay (fromJulianDay), dayFromJulianDay, Planet
  )
import Query.Transit
import Query.Aggregate
import qualified Data.Map as M
import Control.Monad (when)
import Data.Time.Format.ISO8601


transitProgressChart :: TransitMap Planet -> Bool -> IO ()
transitProgressChart (Aggregate allTransits) verbose = do
  when verbose $ do
    forM_ (M.toAscList allTransits) $ \(bodies, transits) -> do
      print bodies
      putStrLn "-------------"
      forM_ (getMerged transits) $ \Transit{aspect,transitStarts, transitEnds, transitProgress, transitPhases} -> do
        startsUT <- fromJulianDay transitStarts :: IO UTCTime
        endsUT   <- fromJulianDay transitEnds   :: IO UTCTime
        print (startsUT, endsUT, aspect)
        putStrLn "======================="
        forM_ transitProgress $ \(jd, orb) -> do
          ut <- fromJulianDay jd :: IO UTCTime
          putStrLn $ show ut <> ": " <> show orb

        putStrLn "======================="
        forM_ (getMerged transitPhases) $ \TransitPhase{phaseName, phaseStarts, phaseEnds} -> do
          startsUT' <- fromJulianDay phaseStarts :: IO UTCTime
          endsUT'  <- fromJulianDay phaseEnds :: IO UTCTime
          print (phaseName, startsUT', endsUT')

  queryT <- getCurrentTime
  let fname = "charts/transit_progress_" <> iso8601Show  queryT <> ".svg"

  toFile def fname $ do
    layout_title .= "Transit Progress"
    layout_y_axis . laxis_reverse .= True
    forM_ (M.toAscList allTransits) $ \((transiting, transited), transits) ->
      forM_ (getMerged transits) $ \Transit{transitProgress, aspect}-> do
        plot (fillBetween (show transiting <> " " <> show aspect <> " " <> show transited)
          [(dayFromJulianDay jd, (o, 5.0)) | (jd, o) <- toList transitProgress])

  print fname

fillBetween :: String -> [(Day, (Double , Double))] -> EC l2 (PlotFillBetween Day Double)
fillBetween title vs = liftEC $ do
  plot_fillbetween_title .= title
  color <- takeColor
  plot_fillbetween_style .= solidFillStyle (0.4 `dissolve` color)
  plot_fillbetween_values .= vs
