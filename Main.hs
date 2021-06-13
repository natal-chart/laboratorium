module Main where

import qualified TransitCharts as TC

main :: IO ()
main = do
  putStrLn "Generating Charts"
  TC.main
  putStrLn "Done!"
