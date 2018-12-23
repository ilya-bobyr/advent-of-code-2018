{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Array.IArray (accumArray, listArray, (!))
import Data.Array.Unboxed (UArray)
import Data.Ix (range)
import Data.List (foldl', maximumBy)
import Data.Ord (comparing)
import Data.Int (Int8)
import System.Environment (getArgs)
import Text.Printf (printf)

cellPowerLevel :: Int -> (Int, Int) -> Int8
cellPowerLevel gridSerial (x, y) =
  let !rackId = x + 10
      !v = (rackId * y + gridSerial) * rackId
  in fromIntegral $ (v `div` 100 `mod` 10) - 5

type Grid = UArray (Int, Int) Int8

getGrid :: Int -> Grid
getGrid gridSerial =
  let bounds = ((0, 0), (299, 299))
  in listArray bounds
     $ map (cellPowerLevel gridSerial)
     $ range bounds

-- (size [1..300], x, y)
type HStripes = UArray (Int, (Int, Int)) Int

getHStripes :: Grid -> HStripes
getHStripes grid =
  let firstForSize :: Int -> Int -> Int
      firstForSize s y =
        sum $ map (fromIntegral . (!) grid) $ zip [0 .. s - 1] $ repeat y
      forSizeAndX s y (!x, !prev) =
        (x + 1
        , prev - fromIntegral (grid ! (x, y))
          + fromIntegral (grid ! (x + s, y))
        )
      forSizeAndY :: (Int, Int) -> [((Int, (Int, Int)), Int)]
      forSizeAndY (s, y) =
        map (\(!x, !res) -> ((s, (x, y)), res))
        $ take (300 - s + 1)
        $ iterate (forSizeAndX s y) (0, firstForSize s y)

      bounds = ((1, (0, 0)), (300, (299, 299)))

  in accumArray (+) 0 bounds
     $ concatMap forSizeAndY
     [(s, y) | s <- [1..300]
             , y <- [0..(300 - s)]]

squarePower :: Grid -> Int -> (Int, Int) -> Int
squarePower grid size (x, y) =
  foldl' (\a p -> a + fromIntegral (grid ! p)) 0
  $ range ((x, y), (x + (size - 1), y + (size - 1)))

solution1 :: Grid -> (Int, Int)
solution1 grid =
  fst
  $ maximumBy (comparing snd)
  $ map (\p -> (p, squarePower grid 3 p))
  [(x, y) | x <- [0..297]
          , y <- [0..297]]

-- There is a more efferent algorithm that uses "Summed-area table" as
-- described here:
--
--   https://en.wikipedia.org/wiki/Summed-area_table
--
-- Here is an implementation:
--
--   https://www.reddit.com/r/adventofcode/comments/a53r6i/2018_day_11_solutions/ebjogd7
--
-- It runs much faster, but requires a lot of destructive updates.  So
-- I would need to switch to STUArray completely to implement it.
solution2 :: HStripes -> (Int, Int, Int)
solution2 hStripes =
  let firstForSize :: Int -> Int -> Int
      firstForSize s x =
        sum $ map (\y -> hStripes ! (s, (x, y))) [0 .. s - 1]

      forSizeAndY s x (!y, !prev) =
        (y + 1
        , prev - hStripes ! (s, (x, y))
          + hStripes ! (s, (x, y + s))
        )
      forSizeAndX (s, x) =
        map (\(!y, !res) -> ((x, y, s), res))
        $ take (300 - s + 1)
        $ iterate (forSizeAndY s x) (0, firstForSize s x)

  in fst
     $ maximumBy (comparing snd)
     $ concatMap forSizeAndX
     [(s, x) | s <- [1..300]
             , x <- [0..(300 - s)]]

  -- Slow:
  -- fst
  -- $ maximumBy (comparing snd)
  -- $ map (\(!x, !y, !s) -> ((x, y, s), squarePower grid s (x, y)))
  -- [(x, y, s) | s <- [1..50]
  --            , x <- [0..(300 - s)]
  --            , y <- [0..(300 - s)]]

main :: IO ()
main = do
  gridSerial <- read . head <$> getArgs :: IO Int

  let myGrid = getGrid gridSerial
      myGridHStripes = getHStripes myGrid

      -- grid18 = getGrid 18
      -- grid18HStripes = getHStripes grid18
      -- grid42 = getGrid 42
      -- grid42HStripes = getHStripes grid42

  -- printf "Actual:   %d\n" $ cellPowerLevel 8 (3, 5)
  -- putStrLn "Expected: 4"
  -- printf "Actual:   %d\n" $ cellPowerLevel 57 (122, 79)
  -- putStrLn "Expected: -5"
  -- printf "Actual:   %d\n" $ cellPowerLevel 39 (217, 196)
  -- putStrLn "Expected: 0"
  -- printf "Actual:   %d\n" $ cellPowerLevel 71 (101, 153)
  -- putStrLn "Expected: 4"
  -- putStrLn ""

  -- printf "Actual:   %d\n" $ squarePower grid18 3 (33, 45)
  -- putStrLn "Expected: 29"
  -- putStrLn ""

  -- printf "Actual:   %s\n" $ show $ solution1 grid18
  -- putStrLn "Expected: (33,45)"
  -- putStrLn ""

  -- printf "Actual:   %s\n" $ show $ solution2 grid18HStripes
  -- putStrLn "Expected: (90,269,16)"
  -- putStrLn ""
  -- printf "Actual:   %s\n" $ show $ solution2 grid42HStripes
  -- putStrLn "Expected: (232,251,12)"
  -- putStrLn ""

  let (s1x, s1y) = solution1 myGrid
      (s2x, s2y, s2s) = solution2 myGridHStripes
  printf "Solution1: %d,%d\n" s1x s1y
  printf "Solution2: %d,%d,%d\n" s2x s2y s2s
