{-# LANGUAGE NamedFieldPuns #-}
module Chart.TransitProgress where

import Data.Foldable (forM_, Foldable (toList, foldMap'))
import Data.Time
  ( UTCTime
  )
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import Graphics.Rendering.Chart.Easy hiding (days)
import SwissEphemeris
  ( FromJulianDay (fromJulianDay)
  )
import Query.Transit
import Query.Aggregate
import qualified Data.Map as M
import Data.List (groupBy)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (when)


transitProgressChart :: TransitMap -> Bool -> IO ()
transitProgressChart (Aggregate allTransits) verbose = do
  when verbose $ do
    forM_ (M.toAscList allTransits) $ \(bodies, transits) -> do
      print bodies
      putStrLn "-------------"
      forM_ (getTransits transits) $ \Transit{aspect,phase,transitStarts, transitEnds, transitProgress} -> do
        startsUT <- fromJulianDay transitStarts :: IO UTCTime
        endsUT   <- fromJulianDay transitEnds   :: IO UTCTime
        print (startsUT, endsUT, aspect, phase)
        putStrLn "======================="
        forM_ transitProgress $ \(jd, orb) -> do
          ut <- fromJulianDay jd :: IO UTCTime
          putStrLn $ show ut <> ": " <> show orb

  toFile def "transit_progress3.svg" $ do
    layout_title .= "Transit Progress"
    layout_y_axis . laxis_reverse .= True
    forM_ (M.toAscList allTransits) $ \((transiting, transited), transits) ->
      forM_ (groupBy (\t1 t2 -> aspect t1 == aspect t2) $ toList $ getTransits transits) $ \transitsByAspect -> do
        let allProgress = toList $ foldMap' transitProgress transitsByAspect
            aspectN = head transitsByAspect & aspect
        plot (fillBetween (show transiting <> " " <> show aspectN <> " " <> show transited)
          -- TODO: I probably want to go out and do this in an actual IO context
          [(unsafePerformIO (fromJulianDay jd), (o, 5.0)) | (jd, o) <- allProgress])

fillBetween :: String -> [(UTCTime, (Double , Double))] -> EC l2 (PlotFillBetween UTCTime Double)
fillBetween title vs = liftEC $ do
  plot_fillbetween_title .= title
  color <- takeColor
  plot_fillbetween_style .= solidFillStyle (0.4 `dissolve` color)
  plot_fillbetween_values .= vs
