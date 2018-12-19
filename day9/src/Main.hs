{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.List.PointedList as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map.Strict as Map
import Control.Lens (view)
import Text.Printf (printf)

solve1 :: Int -> Int -> Int
solve1 playerCount marbleCount =
  let
    go :: PL.PointedList Int
       -> Int
       -> Int
       -> Map.Map Int Int
       -> Int
    go board !player !marble scores
      | marble == marbleCount = maximum $ Map.elems scores
      | marble `mod` 23 == 0 =
          let board' = PLC.moveN (-7) board
              score = view PL.focus board' + marble
              (Just board'') = PLC.delete board'
              scores' = Map.insertWith (+) player score scores
              player' = (player + 1) `mod` playerCount
          in go board'' player' (marble + 1) scores'
      | otherwise =
          let board' = PL.insertRight marble $ PLC.moveN 1 board
              player' = (player + 1) `mod` playerCount
          in go board' player' (marble + 1) scores

    (Just initialBoard) = PL.fromList [0]
  in go initialBoard 0 1 Map.empty

solve1AndPrint :: Int -> Int -> Int -> IO ()
solve1AndPrint playerCount marbleCount expected = do
  let actual = solve1 playerCount marbleCount
  printf "Actual:   %d\n" actual
  printf "Expected: %d\n" expected

main :: IO ()
main = do
  solve1AndPrint 9 25 32
  solve1AndPrint 10 1618 8317
  solve1AndPrint 13 7999 146373
  putStrLn "          vvvv"
  solve1AndPrint 17 1104 2764
  putStrLn "          ^^^^"
  solve1AndPrint 21 6111 54718
  solve1AndPrint 30 5807 37305

  print $ solve1 465 71498

  print $ solve1 465 7149800
