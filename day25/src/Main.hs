module Main where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl', partition)
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import Data.Vector (Vector)
import qualified Data.Vector as V

data Point = Point Int Int Int Int
  deriving (Eq, Show)

manhattanDistance :: Point -> Point -> Int
manhattanDistance (Point w1 x1 y1 z1) (Point w2 x2 y2 z2) =
  abs (w1 - w2) + abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

data Constellation = Constellation (Vector Point)
  deriving (Show, Eq)

constellationPoints :: Constellation -> Vector Point
constellationPoints (Constellation points) = points

type SpaceTime = [Constellation]

emptySpace :: SpaceTime
emptySpace = []

constellationCount :: SpaceTime -> Int
constellationCount = length

listToPoint :: [Int] -> Point
listToPoint [w, x, y, z] = Point w x y z
listToPoint _ = error "Invalid number of integers to form a point"

sameConstellation :: Int -> Point -> Constellation -> Bool
sameConstellation distance p (Constellation points) =
  isJust $ V.find (\p2 -> manhattanDistance p p2 <= distance) points

addPoint :: Int -> SpaceTime -> Point -> SpaceTime
addPoint distance st p =
  let (matching, separate) = partition (sameConstellation distance p) st
      new = Constellation $ p `V.cons` V.concat (map constellationPoints matching)
  in new : separate

main :: IO ()
main = do
  input <- map (listToPoint . map read . splitOn ",") . lines <$> getContents
  -- let point0 = head input
  --     point1 = input !! 1

  -- print $ manhattanDistance point0 point1
  -- print $ manhattanDistance point1 point0

  -- let spaceTime0 = addPoint 3 [] point0
  --     spaceTime1 = addPoint 3 spaceTime0 point1
  -- print spaceTime1

  let spaceTime = foldl' (addPoint 3) [] input
  print $ constellationCount spaceTime
