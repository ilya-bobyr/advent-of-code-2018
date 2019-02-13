{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Control.Lens (set)
import Data.Array (Array, array, listArray, (!), (//))
import qualified Data.Array.IArray as IA
import Data.Ix (inRange)
import Data.List (foldl')
import qualified Data.PQueue.Prio.Min as PQ
import Data.Vector.Fixed (mk3, element)
import qualified Data.Vector.Fixed as FV
import Data.Vector.Fixed.Unboxed (Vec)
import Text.Parsec (parse)
import Text.Parsec.Char (digit, char, string, spaces, newline)
import Text.Parsec.Combinator (eof, many1)
import Text.Parsec.String (Parser)
import System.Environment (getArgs)
import System.IO (Handle, hGetContents, IOMode(ReadMode), stdin, withFile)

type CaveInfo = (Int, (Int, Int))

data Region = Rocky
            | Wet
            | Narrow

type Cave = Array (Int, Int) Region

inputParser :: Parser CaveInfo
inputParser = do
  _ <- string "depth:"
  spaces
  depth <- read <$> many1 digit
  _ <- newline
  _ <- string "target:"
  spaces
  tx <- read <$> many1 digit
  _ <- char ','
  ty <- read <$> many1 digit
  spaces
  eof
  return (depth, (tx, ty))

populateCave :: CaveInfo -> (Int, Int) -> Cave
populateCave (depth, (tx, ty)) (extraX, extraY) =
  let bounds = ((0, 0), (tx + extraX, ty + extraY))
      erosionLevel eArr (x, y)
        | x == 0 && y == depth `rem` 20183
          || x == tx && y == ty = depth `rem` 20183
        | y == 0 = (x * 16807 + depth) `rem` 20183
        | x == 0 = (y * 48271 + depth) `rem` 20183
        | otherwise = ((eArr ! (x - 1, y) * eArr ! (x, y - 1)) + depth) `rem` 20183
      erosionArray =
        array bounds [((x, y), erosionLevel erosionArray (x, y))
                     | x <- [0 .. tx + extraX]
                     , y <- [0 .. ty + extraY]]
      regionType elvl =
        case elvl `rem` 3 of
          0 -> Rocky
          1 -> Wet
          2 -> Narrow
          _ -> error "... `rem` 3 /= [0..2]"
  in listArray bounds $ map regionType $ IA.elems erosionArray

regionRisk :: Region -> Int
regionRisk = \case Rocky -> 0
                   Wet -> 1
                   Narrow -> 2

riskLevel :: Cave -> (Int, Int) -> Int
riskLevel cave (tx, ty) =
  sum [regionRisk $ cave ! (x, y) | x <- [0..tx]
                                  , y <- [0..ty]]

type RegionPaths = Vec 3 Int

type CavePaths = Array (Int, Int) RegionPaths

data Tool = ClimbingGear
          | Torch
          | Neither

toolIndex :: Tool -> Int
toolIndex = \case ClimbingGear -> 0
                  Torch -> 1
                  Neither -> 2

canSwitchTo :: Region -> Tool -> [Tool]
canSwitchTo Rocky ClimbingGear = [Torch]
canSwitchTo Rocky Torch = [ClimbingGear]
canSwitchTo Rocky Neither = [Torch, ClimbingGear]
canSwitchTo Wet ClimbingGear = [Neither]
canSwitchTo Wet Torch = [ClimbingGear, Neither]
canSwitchTo Wet Neither = [ClimbingGear]
canSwitchTo Narrow ClimbingGear = [Torch, Neither]
canSwitchTo Narrow Torch = [Neither]
canSwitchTo Narrow Neither = [Torch]

canEnter :: Region -> Tool -> Bool
canEnter Rocky ClimbingGear = True
canEnter Rocky Torch = True
canEnter Rocky Neither = False
canEnter Wet ClimbingGear = True
canEnter Wet Torch = False
canEnter Wet Neither = True
canEnter Narrow ClimbingGear = False
canEnter Narrow Torch = True
canEnter Narrow Neither = True

allMoves :: (Int, Int) -> [(Int, Int)]
allMoves (x, y) = [ (x - 1, y)
                  , (x + 1, y)
                  , (x, y - 1)
                  , (x, y + 1)
                  ]

minPathForTool :: CavePaths -> (Int, Int) -> Tool -> Int
minPathForTool cavePaths pos tool = (cavePaths ! pos) FV.! toolIndex tool

populatePaths :: Cave -> (Int, Int) -> CavePaths
populatePaths cave target =
  let bounds = IA.bounds cave

      finish pos res
        | pos /= target = False
        | otherwise = minPathForTool res pos Torch < maxBound

      go res todo =
        case PQ.getMin todo of
          Nothing -> res
          Just (_, (pos, _))
            | finish pos res -> res
          Just (dist, (pos, tool)) ->
            let todo' = PQ.deleteMin todo
                distances = res ! pos
                currToolDist = distances FV.! toolIndex tool
            in if dist >= currToolDist
               then go res todo'
               else
                 let region = cave ! pos
                     !distances' =
                       set (element $ toolIndex tool) dist distances
                     res' = res // [(pos, distances')]
                     sameRegionMoves =
                       map (\t -> (dist + 7, (pos, t)))
                       $ canSwitchTo region tool
                     !oneStepMoves =
                       map (\p -> (dist + 1, (p, tool)))
                       $ filter (\p -> canEnter (cave ! p) tool)
                       $ filter (inRange bounds)
                       $ allMoves pos
                     todo'' =
                       foldl' (\queue (d, pos_tool) -> PQ.insert d pos_tool queue)
                       todo' (sameRegionMoves ++ oneStepMoves)
                 in go res' todo''

      initialPaths = listArray bounds $ repeat $ mk3 maxBound maxBound maxBound

  in go initialPaths $ PQ.singleton 0 ((0, 0), Torch)

main :: IO ()
main = do
  args <- getArgs
  case args of
      [] -> run stdin
      file:_ ->
        withFile file ReadMode run

run :: Handle -> IO ()
run inputFile = do
  input <- hGetContents inputFile
  case parse inputParser "<input>" input of
    Left errorText -> print errorText
    Right (depth, target@(tx, ty)) -> do
      -- In practice, I guess, extra space could be way less than
      -- this.  It is probably contributing quite some extra time to
      -- the calculation.  The priority queue should help reduce
      -- unnecessary computations, but it is not perfect - all the
      -- additional paths are still checked, before we finally reach
      -- the target.
      let extra = tx + ty
          cave = populateCave (depth, target) (extra, extra)
      print $ riskLevel cave target

      let allPaths = populatePaths cave target
          minTourchPath = minPathForTool allPaths target Torch

      putStrLn "This will take ~2 minutes..."
      print minTourchPath
