module Main where

import Data.Bool (bool)
import Data.Ix (range)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import Text.Parsec (parse)
import Text.Parsec.Char (char, digit, spaces, newline)
import Text.Parsec.Combinator (many1, eof, sepEndBy)
import Text.Parsec.String (Parser)

data TaggedPoint = Point Int Int Int
  deriving (Show, Eq, Ord)

lineParser :: Parser TaggedPoint
lineParser = do
  x <- read <$> many1 digit :: Parser Int
  spaces
  _ <- char ','
  spaces
  y <- read <$> many1 digit :: Parser Int
  return $ Point 0 x y

inputParser :: Parser [TaggedPoint]
inputParser = do
  allPoints <- lineParser `sepEndBy` newline
  eof
  return $ map (\(pId, Point _ x y) -> Point pId x y) $ zip [0..] allPoints

type Bounds = ((Int, Int), (Int, Int))

areaBounds :: [TaggedPoint] -> Bounds
areaBounds points =
  let Point _ firstX firstY = head points
      findBounds [] ((minX, minY), (maxX, maxY)) =
        ((minX - 1, minY - 1), (maxX + 1, maxY + 1))
      findBounds (Point _ x y : rest) ((minX, minY), (maxX, maxY)) =
        let res = ( ( if x < minX then x else minX
                    , if y < minY then y else minY)
                  , ( if x > maxX then x else maxX
                    , if y > maxY then y else maxY)
                  )
        in findBounds rest res
  in findBounds (tail points) ((firstX, firstY), (firstX, firstY))

atBound :: Bounds -> (Int, Int) -> Bool
atBound ((minX, minY), (maxX, maxY)) (x, y) =
  x == minX || x == maxX || y == minY || y == maxY
{-# INLINE atBound #-}

closestPoint :: [TaggedPoint] -> (Int, Int) -> Maybe Int
closestPoint points (x, y) =
  let distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
      distanceTo = distance (x, y)
      distances =
        sortOn snd
        $ map (\(Point pId pX pY) -> (pId, distanceTo (pX, pY)))
              points
      (bestId, bestDistance) = head distances
      (     _, nextDistance) = distances !! 1
  in if bestDistance < nextDistance
     then Just bestId
     else Nothing

allAreaSizes :: [TaggedPoint] -> Map Int Int
allAreaSizes points =
  Map.fromListWith (+)
  $ map (\pId -> (pId, 1))
  $ mapMaybe (closestPoint points)
  $ range
  $ areaBounds points

infiniteAreaPoints :: [TaggedPoint] -> Set Int
infiniteAreaPoints points =
  let ((minX, minY), (maxX, maxY)) = areaBounds points
      indices = range ((minX, minY), (maxX, minY))
                ++ range ((minX, maxY), (maxX, maxY))
                ++ range ((minX, minY), (minX, maxY))
                ++ range ((maxX, minY), (maxX, maxY))
  in Set.fromList $ mapMaybe (closestPoint points) indices

distanceToAllPoints :: (Int, Int) -> [TaggedPoint] -> Int
distanceToAllPoints (x, y) points =
  let distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
      distanceTo (Point _ px py) = distance (x, y) (px, py)
  in sum $ map distanceTo points

regionSize :: [TaggedPoint] -> Int -> Int
regionSize points maxDistance =
  sum
  $ map (\(x, y) -> bool 0 1 $
                    distanceToAllPoints (x, y) points < maxDistance)
  $ range $ areaBounds points

main :: IO ()
main = do
  input <- getContents
  case parse inputParser "<input>" input of
    Left errorText -> print errorText
    Right allPoints -> do
      let finiteAreaSizes =
            allAreaSizes allPoints `Map.withoutKeys` infiniteAreaPoints allPoints
          largestArea = maximum $ Map.elems finiteAreaSizes
      print largestArea

      print $ regionSize allPoints 10000
