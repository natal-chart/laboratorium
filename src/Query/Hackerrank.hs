{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Query.Hackerrank where

import Control.Monad
import Data.Bits
import Data.List
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe
import Data.Foldable
import Data.Bifunctor
import Text.Printf (printf)

--helloMany :: Int -> IO ()
helloMany n = replicateM_ n (putStrLn "Hello World")

--f :: Int -> [Int] -> [Int]
--f n arr = mconcat $ (replicate n) <$> arr

--f :: Int -> [Int] -> [Int]
--f n arr = foldl' (\l n' -> if n' < n then l <> [n'] else l) [] arr

f :: [Int] -> [Int]
f lst = foldl' (\l (n, isEven) -> if isEven then l <> [n] else l) [] $ zipWith (\n idx -> (n, even idx)) lst [0..]

main :: IO()
main = do
    n <- readLn :: IO Int
    helloMany n
    -- Print "Hello World" on a new line 'n' times.

len :: [a] -> Int
len = foldl' (\c _n -> c+1) 0


-- >>> eulerExpand 20
-- 2423600.1887125224
eulerExpand :: Double -> Double
eulerExpand n =
  sum $ zipWith expanded (replicate 10 n) [0..]
  where
    expanded n' i' = (n'^i')/product [1..(fromIntegral i')]

poly :: [(Double, Double)] -> Double -> Double
poly as x = sum [fst c * x ** snd c | c <- as]

solve' :: Double -> Double -> [(Double,Double)] -> (Double, Double)
solve' l r cs =
    ( sum [     (p x)     * step | x <- xs]
    , sum [pi * (p x) ^ 2 * step | x <- xs]
    )
  where
    p    = poly cs
    step = 0.001
    xs   = [l,l + step..r]

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b =
    [area, vol]
    where
        (area,vol) = solve' (fromIntegral l) (fromIntegral r) (zipWith (curry (bimap fromIntegral fromIntegral)) a b)

{-
-- this one I didn't get, had to look it up :( 
  https://codereview.stackexchange.com/questions/164452/area-under-curve

import Text.Printf (printf)
import Data.Bifunctor

poly :: [(Double, Double)] -> Double -> Double
poly as x = sum [fst c * x ** snd c | c <- as]

solve' :: Double -> Double -> [(Double,Double)] -> (Double, Double)
solve' l r cs =
    ( sum [     (p x)     * step | x <- xs]
    , sum [pi * (p x) ^ 2 * step | x <- xs]
    )
  where
    p    = poly cs
    step = 0.001
    xs   = [l,l + step..r]

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b =
    [area, vol]
    where
        (area,vol) = solve' (fromIntegral l) (fromIntegral r) (zipWith (curry (bimap fromIntegral fromIntegral)) a b)

--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
-}

-- ((λx.((λy.(x y))x))(λz.w))

-- >>> foo
-- 'w'
foo = ((\x -> ((\y -> (x y)) 'x')) (\z -> 'w'))



plusMinusCt :: [Int] -> (Double, Double, Double)
plusMinusCt =
  foldl' signumPart (0,0,0)
  where
    signumPart (m,z,p) n =
      case signum n of
        -1 -> (m+1,z,p)
        0 ->  (m,z+1,p)
        _ -> (m,z,p+1)

subLists :: [Int] -> [[Int]]
subLists l = map ((\(b,a) -> init b <> a) . flip splitAt l) [1 .. (length l)]


miniMaxSum arr = 
  printf "%d %d\n" minSum maxSum
  where
    subLists l = map ((\(b,a) -> init b <> a) . flip splitAt l) [1 .. (length l)]
    sums = map sum (subLists arr)
    minSum = minimum sums
    maxSum = maximum sums
    -- Write your code 


-- >>> head . mconcat . filter ((==1) . length) . group . sort $ [1,2,3,4,3,2,1]
-- 4

-- >>> map (\i -> at (i, i) m') [0..(length m' - 1)]
-- [1,5,9]
-- >>> map (\i -> at (i-1, length m' - i) m') [1..length m']
-- [3,5,9]
m' = [[1,2,3], 
     [4,5,6],
     [9,8,9]]
--at :: (Int, Int) -> [[Int]] -> Int
at (r,c) m = m !! r !! c  


-- >>>replicate 10 0
-- [0,0,0,0,0,0,0,0,0,0]
