module Chart.TransitProgress where

import Data.Foldable (forM_, Foldable (toList, foldMap'))
import Data.Time
  ( UTCTime
  )
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import Graphics.Rendering.Chart.Easy hiding (days)
import SwissEphemeris
  ( Planet (..), FromJulianDay (fromJulianDay)
  )
import Query.Transit
import Query.Aggregate
import qualified Data.Map as M
import Data.List (groupBy)
import System.IO.Unsafe (unsafePerformIO)


defaultTransits :: [(Planet, Planet)]
defaultTransits = [(Venus, Mars), (Mars, Chiron), (Mars, Jupiter), (Saturn, Chiron)]

transitProgressChart :: TransitMap -> IO ()
transitProgressChart (Aggregate allTransits) =
  toFile def "transit_progress.svg" $ do
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
