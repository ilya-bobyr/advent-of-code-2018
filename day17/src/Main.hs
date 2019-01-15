{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Bool ((<&&>))
import Control.Monad (foldM, sequence)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (readArray, writeArray)
import qualified Data.Array.MArray as MA
import Data.Array (Array)
import Data.Array.ST (STArray)
import Data.List (foldl', intercalate)
import Data.Maybe (catMaybes)
import System.Environment (getArgs)
import System.IO (Handle, hGetContents, IOMode(ReadMode), stdin, withFile)
import Text.Parsec (parse)
import Text.Parsec.Char (digit, string, newline)
import Text.Parsec.Combinator (eof, many1, sepEndBy)
import Text.Parsec.Prim (try, (<|>))
import Text.Parsec.String (Parser)

-- import Debug.Trace (traceM)

data BoardCell = Sand
               | Clay
               | MovingWater
               | StillWater
               deriving (Eq)

instance Show BoardCell where
  show Sand = "."
  show Clay = "#"
  show MovingWater = "|"
  show StillWater = "~"

flowAbove :: BoardCell -> Bool
flowAbove = \case Clay -> True
                  StillWater -> True
                  _ -> False

type Board s = STArray s (Int, Int) BoardCell
type IBoard = Array (Int, Int) BoardCell

showBoard :: Board s -> ST s String
showBoard board = do
  ((minY, minX), (maxY, maxX)) <- MA.getBounds board
  let asChar = \case Sand -> '.'
                     Clay -> '#'
                     MovingWater -> '|'
                     StillWater -> '~'

  intercalate "\n" <$>
    sequence [
        sequence [
            asChar <$> readArray board (y, x) | x <- [minX..maxX]]
        | y <- [minY..maxY]]

data InputCommand = Horizontal Int (Int, Int)
                  | Vertical (Int, Int) Int
                  deriving (Eq, Show)

horizontalCommandParser :: Parser InputCommand
horizontalCommandParser = do
  _ <- string "y="
  y <- read <$> many1 digit
  _ <- string ", x="
  minX <- read <$> many1 digit
  _ <- string ".."
  maxX <- read <$> many1 digit
  return $ Horizontal y (minX, maxX)

verticalCommandParser :: Parser InputCommand
verticalCommandParser = do
  _ <- string "x="
  x <- read <$> many1 digit
  _ <- string ", y="
  minY <- read <$> many1 digit
  _ <- string ".."
  maxY <- read <$> many1 digit
  return $ Vertical (minY, maxY) x

commandParser :: Parser InputCommand
commandParser =
  try horizontalCommandParser
  <|> verticalCommandParser

inputParser :: Parser [InputCommand]
inputParser = do
  commands <- commandParser `sepEndBy` newline
  eof
  return commands

commandBounds :: InputCommand -> ((Int, Int), (Int, Int))
commandBounds (Horizontal y (minX, maxX)) = ((y, minX), (y, maxX))
commandBounds (Vertical (minY, maxY) x) = ((minY, x), (maxY, x))

buildBoard :: [InputCommand] -> ST s (Board s)
buildBoard commands =
  let buildBounds ((!minY, !minX), (!maxY, !maxX))
        ((cMinY, cMinX), (cMaxY, cMaxX)) =
        let minX' = if cMinX < minX then cMinX else minX
            maxX' = if cMaxX > maxX then cMaxX else maxX
            minY' = if cMinY < minY then cMinY else minY
            maxY' = if cMaxY > maxY then cMaxY else maxY
        in ((minY', minX'), (maxY', maxX'))

      bounds =
        foldl' buildBounds ((0, 500), (0, 500))
        $ map commandBounds commands

      extendXBy dx ((!minY, !minX), (!maxY, !maxX)) =
        ((minY, minX - dx), (maxY, maxX + dx))

      applyCommand :: Board s -> InputCommand -> ST s (Board s)
      applyCommand board (Horizontal y (minX, maxX)) = do
        mapM_ (\x -> writeArray board (y, x) Clay) [minX..maxX]
        return board
      applyCommand board (Vertical (minY, maxY) x) = do
        mapM_ (\y -> writeArray board (y, x) Clay) [minY..maxY]
        return board
  in do
    initial <- MA.newArray (extendXBy 1 bounds) Sand
    foldM applyCommand initial commands

findMinY :: [InputCommand] -> Int
findMinY commands =
  let getY c = case commandBounds c of
        ((minY, _), _) -> minY

      go !mY [] = mY
      go !mY (c:cs) =
        let mY' = if mY < getY c then mY else getY c
        in go mY' cs
  in go (getY $ head commands) $ tail commands

flowWaterOneTick :: Board s -> [(Int, Int)] -> ST s [(Int, Int)]
flowWaterOneTick _ [] = return []
flowWaterOneTick board (p:ps) = do
  (_, (maxY, _)) <- MA.getBounds board
  let extendDown (!y, !x) = do
        belowCell <- readArray board (y+1, x)
        case belowCell of
          Sand -> do writeArray board (y+1, x) MovingWater
                     return [(y+1, x)]
          Clay -> extendSideways (y, x)
          MovingWater -> return []
          StillWater -> extendSideways (y, x)

      boundedOn dir (!y, !x) = do
        belowCell <- readArray board (y+1, x)
        if flowAbove belowCell
          then do dirCell <- readArray board (y, dir x)
                  case dirCell of
                    Sand -> boundedOn dir (y, dir x)
                    Clay -> return True
                    StillWater -> return True
                    MovingWater -> boundedOn dir (y, dir x)
          else return False

      boundedOnBothSides (y, x) =
        boundedOn (subtract 1) (y, x) <&&> boundedOn (+1) (y, x)

      extendTo dir as (!y, !x) = do
        belowCell <- readArray board (y+1, x)
        let fillAndContinue =
              do writeArray board (y, x) as
                 dirCell <- readArray board (y, dir x)
                 case dirCell of
                    Sand -> extendTo dir as (y, dir x)
                    Clay -> return Nothing
                    MovingWater -> extendTo dir as (y, dir x)
                    StillWater -> return Nothing

            fillAndExtend =
              do writeArray board (y, x) as
                 return $ Just (y, x)

        case belowCell of
          Sand -> fillAndExtend
          Clay -> fillAndContinue
          MovingWater -> return Nothing
          StillWater -> fillAndContinue

      extendSideways (!y, !x) = do
        isBounded <- boundedOnBothSides (y, x)
        if isBounded
          then do _ <- extendTo (subtract 1) StillWater (y, x)
                  _ <- extendTo (+1) StillWater (y, x)
                  return [(y-1, x)]
          else do left <- extendTo (subtract 1) MovingWater (y, x)
                  right <- extendTo (+1) MovingWater (y, x)
                  return $ catMaybes [left, right]

  if fst p == maxY
    then return ps
    else do -- boardText <- showBoard board
            -- traceM "---------"
            -- traceM $ show (p:ps)
            -- bounds <- MA.getBounds board
            -- traceM $ show bounds
            -- traceM boardText
            ps' <- extendDown p
            return $ ps' ++ ps

flowWater :: Board s -> [(Int, Int)] -> ST s ()
flowWater _ [] = return ()
flowWater board ps = do
  ps' <- flowWaterOneTick board ps
  flowWater board ps'

countCells :: Board s -> Int -> (BoardCell -> Bool) -> ST s Int
countCells board minY target =
  length
    . filter target
    . map snd
    . filter ((>= minY) . fst . fst)
    <$> MA.getAssocs board

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
    Right commands -> do
      let isWater = \case MovingWater -> True
                          StillWater -> True
                          _ -> False
          isStillWater = \case StillWater -> True
                               _ -> False
      let (solution1, solution2) = runST $ do
            board <- buildBoard commands
            flowWater board [(0, 500)]
            res1 <- countCells board (findMinY commands) isWater
            res2 <- countCells board (findMinY commands) isStillWater
            return (res1, res2)
      print solution1
      print solution2
