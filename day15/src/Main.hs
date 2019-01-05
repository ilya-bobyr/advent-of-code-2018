{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Bool ((<|=>))
import Control.Monad (filterM, mapM, sequence, when, (<=<))
import Control.Monad.ST (ST, runST)
import Data.Array.IArray (listArray)
import Data.Array.MArray (newArray, readArray, writeArray)
import qualified Data.Array.MArray as MA
import Data.Array (Array)
import Data.Array.ST (STArray, STUArray)
import Data.Bits ((.|.))
import Data.Char (ord)
import qualified Data.Ix as Ix
import Data.Ord (comparing)
import Data.List (concatMap, foldl', intercalate, nub, sortBy, elemIndex)
import Data.Maybe (isJust, listToMaybe)
import System.Environment (getArgs)
import System.IO (Handle, hGetContents, IOMode(ReadMode), stdin, withFile)
import Text.Printf (printf, perror)

import Debug.Trace (traceM)

data BoardCell = Empty
               | Wall
               | Elf Int
               | Goblin Int
               deriving (Eq)

instance Show BoardCell where
  show Empty = "."
  show Wall = "#"
  show (Elf _) = "E"
  show (Goblin _) = "G"

type MBoard s = STArray s (Int, Int) BoardCell
type IBoard = Array (Int, Int) BoardCell

parseInput :: [String] -> Either String IBoard
parseInput input =
  let rows = length input
      columns = length $ head input

      go :: (Int, Int) -> [BoardCell] -> [String] -> Either String IBoard
      go _ cells [] =
        let bounds = ((0, 0), (rows - 1, columns - 1))
        in Right $ listArray bounds (reverse cells)
      go (!y, !x) cells ((c:cs):ls)
        | c == '.'  = go (y, x+1) (Empty      : cells) (cs:ls)
        | c == '#'  = go (y, x+1) (Wall       : cells) (cs:ls)
        | c == 'E'  = go (y, x+1) (Elf 200    : cells) (cs:ls)
        | c == 'G'  = go (y, x+1) (Goblin 200 : cells) (cs:ls)
        | otherwise = Left
          $ printf "Unexpected character '%c' (%d) at (%d, %d)" c (ord c) x y

      go (!y, !x) cells ([]:ls)
        | x /= columns = Left
          $ printf ("Unexpected row length in line %d\n"
                    ++ "Expected: %d, actual: %d") y columns x
        | otherwise =
          go (y+1, 0) cells ls

  in go (0, 0) [] input

showBoard :: MBoard s -> ST s String
showBoard board = do
  ((minY, minX), (maxY, maxX)) <- MA.getBounds board
  let asChar = \case Empty -> '.'
                     Wall -> '#'
                     Elf _ -> 'E'
                     Goblin _ -> 'G'
      unitWithHeath =
        \case Elf h -> printf "E(%d)" h
              Goblin h -> printf "G(%d)" h
              invalid ->
                perror "Unexpected unitWithHeath source cell: %s" invalid

      rowCells y = sequence [readArray board (y, x) | x <- [minX..maxX]]

      rowText y = do
        cells <- rowCells y
        return
          $ map asChar cells
          ++ if any isUnit cells
             then "   "
                  ++ intercalate ", " (map unitWithHeath
                                       $ filter isUnit cells)
             else ""

  intercalate "\n" <$> sequence [rowText y | y <- [minY..maxY]]

count :: (a -> Bool) -> [a] -> Int
count p = go 0
  where go !n [] = n
        go !n (x:xs) | p x       = go (n+1) xs
                     | otherwise = go n xs

traceBoard :: String -> MBoard s -> ST s ()
traceBoard prefix board = do
  boardText <- showBoard board
  traceM (prefix ++ boardText)

isUnit :: BoardCell -> Bool
isUnit = \case Elf _ -> True
               Goblin _ -> True
               _ -> False

isEmpty :: BoardCell -> Bool
isEmpty = \case Empty -> True
                _ -> False

isElf :: BoardCell -> Bool
isElf = \case Elf _ -> True
              _ -> False

unitHealth :: BoardCell -> Int
unitHealth =
  \case Elf h -> h
        Goblin h -> h
        invalid ->
          perror "Unexpected unitHealth source cell: %s" (show invalid)

adjustUnitHealth :: (Int -> Int) -> BoardCell -> BoardCell
adjustUnitHealth f =
  \case Elf h -> Elf (f h)
        Goblin h -> Goblin (f h)
        invalid -> perror "Unexpected adjustUnitHealth source cell: %s" invalid

allDirections :: (Int, Int) -> [(Int, Int)]
allDirections (!y, !x) =
  [(y-1, x), (y+1, x), (y, x-1), (y, x+1)]

adjacent :: MBoard s -> (Int, Int) -> ST s [((Int, Int), BoardCell)]
adjacent board pos = do
  bounds <- MA.getBounds board
  mapM (\i -> do cell <- readArray board i
                 return (i, cell))
    $ filter (Ix.inRange bounds)
    $ allDirections pos

isInRange :: MBoard s -> (BoardCell -> Bool) -> (Int, Int) -> ST s Bool
isInRange board isTarget pos =
  any (isTarget . snd) <$> adjacent board pos

markVisited :: STUArray s (Int, Int) Bool
            -> [(Int, Int)]
            -> ST s ()
markVisited visited =
  mapM_ (\p -> writeArray visited p True)

skipVisited :: STUArray s (Int, Int) Bool
            -> [(Int, Int)]
            -> ST s [(Int, Int)]
skipVisited visited =
  filterM (return . not <=< readArray visited)

onlyWalkable :: MBoard s
             -> (BoardCell -> Bool)
             -> [(Int, Int)]
             -> ST s [(Int, Int)]
onlyWalkable board walking =
  filterM (return . walking <=< readArray board)

closestMatching :: MBoard s
                -> (BoardCell -> Bool)
                -> STUArray s (Int, Int) Bool
                -> [(Int, Int)]
                -> ((Int, Int) -> ST s Bool)
                -> ST s [(Int, Int)]
closestMatching _ _ _ [] _ = return []
closestMatching board walking visited front matches = do
  bounds <- MA.getBounds board
  matched <- filterM matches front
  if null matched
    then do markVisited visited front
            let newFront =
                  filter (Ix.inRange bounds)
                  $ nub
                  $ concatMap allDirections front
            newFront' <-
              onlyWalkable board walking
              <=< skipVisited visited
              $ newFront
            closestMatching board walking visited newFront' matches
    else return matched

closestWalkable :: MBoard s
                -> [(Int, Int)]
                -> ((Int, Int) -> ST s Bool)
                -> ST s [(Int, Int)]
closestWalkable board front matches = do
  bounds <- MA.getBounds board
  initialVisited <- newArray bounds False
  closestMatching board isEmpty initialVisited front matches

isEnemy :: BoardCell -> BoardCell -> Bool
isEnemy =
  \case Elf _ -> \case Goblin _ -> True
                       _ -> False
        Goblin _ -> \case Elf _ -> True
                          _ -> False
        invalid -> perror "Unexpected isEnemy source cell: %s" invalid

chosenTarget :: MBoard s -> (Int, Int) -> ST s (Maybe (Int, Int))
chosenTarget board fromPos = do
  fromCell <- readArray board fromPos
  candidates <- closestWalkable board [fromPos]
    (isInRange board (isEnemy fromCell))

  return $ if null candidates
           then Nothing
           else Just $ minimum candidates

stepTowards :: MBoard s -> (Int, Int) -> (Int, Int) -> ST s (Int, Int)
stepTowards board from to = do
  bounds <- MA.getBounds board
  let moveOptions = filter (Ix.inRange bounds) $ allDirections from
      isTarget p = return $ isJust $ elemIndex p moveOptions

  candidates <- closestWalkable board [to] isTarget
  return $ if null candidates
           then perror "Could not find a path from %s to %s" from to
           else minimum candidates

attackTarget :: MBoard s -> (Int, Int) -> ST s (Maybe (Int, Int))
attackTarget board from = do
  fromCell <- readArray board from
  listToMaybe
    . map fst
    . sortBy (\a b -> comparing (unitHealth . snd) a b
                      <> comparing fst a b)
    . filter (isEnemy fromCell . snd)
    <$> adjacent board from

turnOrder :: MBoard s -> ST s [(Int, Int)]
turnOrder board =
  map fst . filter (isUnit . snd) <$> MA.getAssocs board

moveUnit :: MBoard s -> (Int, Int) -> ST s (Int, Int)
moveUnit board pos = do
  mTarget <- chosenTarget board pos
  case mTarget of
    Nothing -> return pos
    Just target -> do
      pos' <- stepTowards board pos target
      unit <- readArray board pos
      writeArray board pos Empty
      writeArray board pos' unit
      return pos'

unitAttack :: Int -> MBoard s -> (Int, Int) -> ST s ()
unitAttack power board pos = do
  mTarget <- attackTarget board pos
  case mTarget of
    Nothing -> return ()
    Just targetPos ->
      do target <- readArray board targetPos
         if unitHealth target > power
           then writeArray board targetPos
                (adjustUnitHealth (subtract power) target)
           else writeArray board targetPos Empty

unitTurn :: Int -> MBoard s -> (Int, Int) -> ST s ()
unitTurn power board pos = do
  cell <- readArray board pos
  when (isUnit cell) $
    do mTarget <- attackTarget board pos
       case mTarget of
         Nothing -> do pos' <- moveUnit board pos
                       unitAttack power board pos'
         Just _ -> unitAttack power board pos

combatEnded :: MBoard s -> ST s Bool
combatEnded board =
  let markSeen !seen = \case Elf _ -> seen .|. 1
                             Goblin _ -> seen .|. 2
                             _ -> seen
  in (< 3) . foldl' markSeen (0 :: Int) <$> MA.getElems board

runUntil :: (BoardCell -> Int)
         -> (MBoard s -> ST s Bool)
         -> (Int, MBoard s)
         -> ST s Int
runUntil unitPower cond (!i, board) = do
  -- traceBoard (show i ++ ":\n") board
  order <- turnOrder board
  let evalTurn [] = runUntil unitPower cond (i+1, board)
      evalTurn (pos:ps) = do
        shouldStop <- cond board
        cell <- readArray board pos
        unitTurn (unitPower cell) board pos
        if shouldStop
          then return i
          else evalTurn ps
  evalTurn order

elfCount :: MBoard s -> ST s Int
elfCount board =
  count isElf <$> MA.getElems board

elfCountNotEqual :: Int -> MBoard s -> ST s Bool
elfCountNotEqual expected board =
  (/= expected) <$> elfCount board

solution1 :: IBoard -> Int
solution1 iboard =
  runST $
  do board <- MA.thaw iboard
     fullTurnCount <- runUntil (const 3) combatEnded (0, board)
     totalHealth <-
       sum
       . map unitHealth
       . filter isUnit
       <$> MA.getElems board
     return $ fullTurnCount * totalHealth

solution2 :: IBoard -> Int
solution2 iboard =
  runST $
  do firstBoard <- MA.thaw iboard
     expectedElfs <- elfCount firstBoard
     let findMinElfPower :: MBoard s -> Int -> ST s Int
         findMinElfPower board !power = do
           let unitPower = \case Elf _ -> power
                                 _ -> 3
               stopCondition b =
                 combatEnded b <|=> elfCountNotEqual expectedElfs b
           fullTurnCount <-
             runUntil unitPower stopCondition (0, board)
           elfWon <- (== expectedElfs) <$> elfCount board
           if elfWon
             then do totalHealth <-
                       sum
                       . map unitHealth
                       . filter isUnit
                       <$> MA.getElems board
                     return $ fullTurnCount * totalHealth
             else do newBoard <- MA.thaw iboard
                     findMinElfPower newBoard (power+1)
     findMinElfPower firstBoard 4

main :: IO ()
main = do
  args <- getArgs
  case args of
      [] -> run stdin
      file:_ ->
        withFile file ReadMode run

run :: Handle -> IO ()
run inputFile = do
  input <- filter (not . null) . lines <$> hGetContents inputFile
  case parseInput input of
    Left errorText -> putStrLn errorText
    Right initialBoard -> do
      print $ solution1 initialBoard
      print $ solution2 initialBoard
