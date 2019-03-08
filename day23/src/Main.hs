{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad.ST (ST)
import Data.Array (listArray, (!))
import qualified Data.Array.IArray as IA
import Data.Array.ST (STUArray, runSTUArray)
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Foldable (forM_)
import Data.List (maximumBy, groupBy, sortBy, nub)
import Data.List.Extra (nubSort)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Word (Word32)
import Numeric.Search.Range (searchFromTo)
import Text.Parsec (parse)
import Text.Parsec.Char (digit, char, string, spaces, newline)
import Text.Parsec.Combinator (eof, many1, option, sepEndBy)
import Text.Parsec.String (Parser)
import Text.Printf (perror)

type Pos = (Int, Int, Int)

data NanoBot = NanoBot {
    nanobotPos :: Pos
  , nanobotSignal :: Int
  } deriving (Show)

numberParser :: Parser Int
numberParser = do
  sign <- option '+' $ char '-'
  value <- read <$> many1 digit
  return $ if sign == '+'
           then value
           else -value

nanoBotParser :: Parser NanoBot
nanoBotParser = do
  _ <- string "pos=<"
  x <- numberParser
  _ <- char ','
  y <- numberParser
  _ <- char ','
  z <- numberParser
  _ <- string ">,"
  spaces
  _ <- string "r="
  r <- read <$> many1 digit
  return $ NanoBot (x, y, z) r

inputParser :: Parser [NanoBot]
inputParser = do
  bots <- nanoBotParser `sepEndBy` newline
  eof
  return bots

distance :: Pos -> Pos -> Int
distance (!x1, !y1, !z1) (!x2, !y2, !z2) =
  abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)
{-# INLINE distance #-}

countInRange :: [NanoBot] -> Pos -> Int -> Int
countInRange bots from range =
  length
  $ filter ((<= range)
             . distance from
             . nanobotPos)
  bots

solution1 :: [NanoBot] -> Int
solution1 bots =
  let strongestSignal = maximumBy (comparing nanobotSignal) bots
      from = nanobotPos strongestSignal
  in countInRange bots from (nanobotSignal strongestSignal)

countReaching :: [NanoBot] -> Pos -> Word32
countReaching bots target =
  let reaching (NanoBot from !range) = distance from target <= range
  in fromIntegral $ length $ filter reaching bots

allDirections :: Int -> Pos -> [Pos]
allDirections delta (x, y, z) =
  [ (x + delta, y, z)
  , (x - delta, y, z)
  , (x, y + delta, z)
  , (x, y - delta, z)
  , (x, y, z + delta)
  , (x, y, z - delta)]

distanceToZero :: Pos -> Int
distanceToZero = distance (0, 0, 0)

moveTowardsZero :: (Pos -> Bool) -> Pos -> Pos
moveTowardsZero confirms from =
  let isCloserToZero prev curr =
        distanceToZero prev < distanceToZero curr

      moveOne p =
        filter (isCloserToZero p)
        $ allDirections 1 p

      moveFront front =
        filter confirms
        $ nub
        $ concatMap moveOne front

      findBest (ps1:rest@(ps2:_))
        | null ps2 = head ps1
        | otherwise = findBest rest
      findBest err = perror "findBest: " err

  in findBest
     $ iterate moveFront [from]

{-

In 2D a bot has an area that it covers that looks like a rotated
square:

y
. . . . . . .
. . . o . . .
. . o o o . .
. o o o o o .
. . o o o . .
. . . o . . .
(0,0) . . . . x

In 3D these squares become cubes.  The idea of the solution is to
"rotate" the coordinate system 45 degrees, in which case those cubes
will run parallel to the coordinate grid.  Next we will go along each
axis and mark regions that are potentially covered by each bot.  For
example, if we have a 2D case like this:

y
. . . . . . . . . . . .
. . . o . . . . . . . .
. . o o o c . * . . . .
. o o 1 o o * * * . d .
. b o o o * * * * * . .
. . . o * * * * * * * .
. . . * * * * 2 * * * *
. . . . * * * * * * * .
. . . . a * * * * * . .
. . . . . . * * * . . .
(0,0) . . . . * . . . . x

There are two bots: one starting in position 1 and one starting in
position 2.  Rotating 45 degrees clockwise, we would have 5 regions:
one before the bots, one with a single slice produces by side a of
bot 2, one produced by side b of bot 1, side c of bot 1, side d of
bot 2, and finally one after side d of bot 2 with no bots.

Now for each of bot we would go through these regions and will
increment a counter for every region cover by a bot, if if projected
onto axis x.  In this example, the counters for bot 1 will be [0, 0,
1, 0, 0], and for bot 2 they will be [0, 1, 1, 1, 0].  First and the
last regions will always have counters of 0.  Adding the counters
together we get [0, 1, 2, 1, 0].

These counters provide us with the *maximum* possible number of
overlapping squares (cubes for 3D case) in this region.  We then need
some "low water" mark that would allow us to filter out regions that
could not possibly contain enough overlaps.

This is an estimation, of cause.  For the low water mark I use the
number I get from checking just all the furthest points of all bots.

So first I check all the furthest points - 8 per bot, and calculate
one with the best reachability.  Then I use this number to filter out
regions along all the projections (somehow there are 4 in 3D space).

Note that within an intersection of regions along 3 possible
projections the reachability does not change.  So it is enough to
calculate it only once on the intersection.

For the problem on the website this reduces the number of regions from
several thousand to 159, 172, 113 and 222 along the 4 projections.
This is not perfect as projections may not go along the integer
coordinate, meaning that I would have to check 159*172*222*8*8
points. First 8 is for the case when the intersection happens
in-between integer coordinates on all 3 projections as I would need to
shift every coordinate in two possible directions and check all of
them.  The second 8 is for the fact that I have 4 projections and by
choosing 3 out of 4 I get an intersection - and there are 8 ways to
choose 3 out of 4.

The above give me about 388m points to check.  And for each point I
will need to calculate distance to 1000 bots.  So about 400G
distances. For the problem at hand this number is actually ~56G
distances.  Which are checked in ~10 minutes.

This is not the fastest possible solution.  There are solutions that
solve this problem in microseconds, but they use Z3.

 Box equation:

 |x - x0| + |y - y0| + |z - z0| = r


  x - x0 + y - y0 + z - z0 = r
  x - x0 + y - y0 - z + z0 = r
  x - x0 - y + y0 + z - z0 = r
  x - x0 - y + y0 - z + z0 = r
 -x + x0 + y - y0 + z - z0 = r
 -x + x0 + y - y0 - z + z0 = r
 -x + x0 - y + y0 + z - z0 = r
 -x + x0 - y + y0 - z + z0 = r


  x + y + z = r + x0 + y0 + z0
  x + y - z = r + x0 + y0 - z0
  x - y + z = r + x0 - y0 + z0
  x - y - z = r + x0 - y0 - z0
 -x + y + z = r - x0 + y0 + z0
 -x + y - z = r - x0 + y0 - z0
 -x - y + z = r - x0 - y0 + z0
 -x - y - z = r - x0 - y0 - z0


  x + y + z =  r + x0 + y0 + z0
  x + y + z = -r + x0 + y0 + z0

  x - y + z =  r + x0 - y0 + z0
  x - y + z = -r + x0 - y0 + z0

  x + y - z =  r + x0 + y0 - z0
  x + y - z = -r + x0 + y0 - z0

  x - y - z =  r + x0 - y0 - z0
  x - y - z = -r + x0 - y0 - z0


  x + y + z = a
  x - y + z = b
  x + y - z = c

    x = (b + c) / 2
    y = (a - b) / 2
    z = (a - c) / 2

  x + y + z = a
  x - y + z = b
  x - y - z = d

    x = (a + d) / 2
    y = (a - b) / 2
    z = (b - d) / 2

  x + y + z = a
  x + y - z = c
  x - y - z = d

    x = (a + d) / 2
    y = (c - d) / 2
    z = (a - c) / 2

  x - y + z = b
  x + y - z = c
  x - y - z = d

    x = (b + c) / 2
    y = (c - d) / 2
    z = (b - d) / 2

-}

bestPoints :: [NanoBot]
           -> [(Int, Int, Int)]
           -> [(Int, Int, Int)]
bestPoints bots points =
  let pointRank :: (Int, Int, Int) -> Word32
      pointRank = countReaching bots

      pickTheBest _ _ res [] = res
      pickTheBest rank bestRank res (p:ps)
        | pRank < bestRank = pickTheBest rank bestRank res ps
        | pRank == bestRank = pickTheBest rank bestRank (p:res) ps
        | otherwise = pickTheBest rank pRank [p] ps
          where pRank = rank p

      firstPoint = head points
      firstRank = pointRank firstPoint

  in pickTheBest pointRank firstRank [firstPoint] $ tail points

-- Slow version of bestPoints. Allocates a lot of memory.
-- bestPoints :: [NanoBot]
--            -> [(Int, Int, Int)]
--            -> [(Int, Int, Int)]
-- bestPoints bots points =
--   let pointRank = countReaching bots
--   in map snd
--      $ head
--      $ groupBy (\a b -> fst a == fst b)
--      $ sortBy (comparing $ negate . fst)
--      $ map (\p -> (pointRank p, p))
--      points

bestEndpoints :: [NanoBot] -> [(Int, Int, Int)]
bestEndpoints bots =
  let botRangeEndpoints (NanoBot pos r) =
        allDirections r pos

      candidatePoints =
        nubSort
        $ concatMap botRangeEndpoints bots

  in bestPoints bots candidatePoints

-- bestEndpoint additional movement:
--
--       bestRank = pointRank $ head bestEdgePoints
--
--       keepMoving = (== bestRank) . pointRank
--
--   in minimum
--      $ map (distanceToZero
--             . moveTowardsZero keepMoving)
--      bestEdgePoints

bestEndpointRank :: [NanoBot] -> Word32
bestEndpointRank bots =
  countReaching bots $ head $ bestEndpoints bots

processSlice :: [NanoBot]
             -> Word32
             -> (NanoBot -> (Int, Int))
             -> [Int]
processSlice bots lowWater gen =
  let flat (a, b) = [a, b]
      cuts = nubSort
        $ concatMap (flat . gen) bots
      cutsArr = listArray (0, maxI) cuts

      maxI = fromIntegral $ length cuts - 1 :: Word32

      orderPair (x, y) =
        if x <= y
        then (x, y)
        else (y, x)

      markOverlap :: STUArray s Word32 Word32
                  -> (Int, Int)
                  -> ST s (STUArray s Word32 Word32)
      markOverlap overlap (from', to') = do
        let (from, to) = orderPair (from', to')
            startI =
              fromMaybe 0 $
              searchFromTo (\i -> cutsArr ! i >= from) 0 maxI
            endI =
              subtract 1
              $ fromMaybe (maxI + 1)
              $ searchFromTo (\i -> to < cutsArr ! i) startI maxI
        forM_ [startI..endI] $ \i -> do
          v <- readArray overlap $ fromIntegral i
          writeArray overlap (fromIntegral i) (v+1)
        return overlap

      minOverlapsArr = runSTUArray $ do
        overlap <- newArray (0, maxI) 0
        forM_ bots $ \bot -> markOverlap overlap (gen bot)
        return overlap

      filterAbove _ _ [] = []
      filterAbove mark prev ((m, x):ps)
        | m >= mark = x : filterAbove mark True ps
        | prev      = x : filterAbove mark False ps
        | otherwise =     filterAbove mark False ps

  in filterAbove lowWater False $ zip (IA.elems minOverlapsArr) cuts


solution2 :: [NanoBot] -> Int
solution2 bots =
  let lowWater = bestEndpointRank bots

      slices gen =
        processSlice bots lowWater (\(NanoBot (x, y, z) r) -> gen x y z r)
      (as, bs, cs, ds) = (
          slices $ \x y z r -> (r + x + y + z, -r + x + y + z)
        , slices $ \x y z r -> (r + x - y + z, -r + x - y + z)
        , slices $ \x y z r -> (r + x + y - z, -r + x + y - z)
        , slices $ \x y z r -> (r + x - y - z, -r - x - y - z)
        )

      extendX (!dx, !dy, !dz)
        | even dx   = extendY (dx `quot` 2, dy, dz)
        | otherwise = extendY (dx `quot` 2, dy, dz)
                      ++ extendY (dx `quot` 2 + 1, dy, dz)
      {-# INLINE extendX #-}

      extendY (!x, !dy, !dz)
        | even dy   = extendZ (x, dy `quot` 2, dz)
        | otherwise = extendZ (x, dy `quot` 2, dz)
                      ++ extendZ (x, dy `quot` 2 + 1, dz)
      {-# INLINE extendY #-}

      extendZ (!x, !y, !dz)
        | even dz   = [(x, y, dz `quot` 2)]
        | otherwise = [ (x, y, dz `quot` 2)
                      , (x, y, dz `quot` 2 + 1)
                      ]
      {-# INLINE extendZ #-}

      candidates =
        concatMap extendX
        -- x + y + z = a
        -- x - y + z = b
        -- x + y - z = c
        --   x = (b + c) / 2
        --   y = (a - b) / 2
        --   z = (a - c) / 2
        $ [(b + c, a - b, a - c) | a <- as, b <- bs, c <- cs]
        -- x + y + z = a
        -- x - y + z = b
        -- x - y - z = d
        --   x = (a + d) / 2
        --   y = (a - b) / 2
        --   z = (b - d) / 2
        ++ [(a + d, a - b, b - d) | a <- as, b <- bs, d <- ds]
        -- x + y + z = a
        -- x + y - z = c
        -- x - y - z = d
        --   x = (a + d) / 2
        --   y = (c - d) / 2
        --   z = (a - c) / 2
        ++ [(a + d, c - d, a - c) | a <- as, c <- cs, d <- ds]
        -- x - y + z = b
        -- x + y - z = c
        -- x - y - z = d
        --   x = (b + c) / 2
        --   y = (c - d) / 2
        --   z = (b - d) / 2
        ++ [(b + c, c - d, b - d) | b <- bs, c <- cs, d <- ds]

  in minimum
     $ map distanceToZero
     $ bestPoints bots candidates

main :: IO ()
main = do
  input <- getContents
  case parse inputParser "<input>" input of
    Left errorText -> print errorText
    Right bots -> do
      print $ solution1 bots
      putStrLn "This will take ~10 minutes... :("
      print $ solution2 bots
