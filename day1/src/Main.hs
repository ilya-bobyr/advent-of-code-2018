{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Set (Set)
import qualified Data.Set as S

parseOne :: String -> Int
parseOne ('+' : num) = read num
parseOne ('-' : num) = negate $ read num
parseOne text = error $ "Failed to parse: '" ++ text ++ "'"

findFirstMatch :: [Int] -> Int
findFirstMatch adjs =
  let loop !v (a:as) !seen =
        let v' = v + a
        in if S.member v' seen
           then v'
           else loop v' as (S.insert v' seen)
      loop _ [] _ = error "Ran out of adjustments!"
  in loop 0 (cycle adjs) S.empty

main :: IO ()
main = do
  input <- getContents
  let adjustments = map parseOne $ lines input :: [Int]
      part1 = sum adjustments

      part2 = findFirstMatch adjustments

  print part1
  print part2
