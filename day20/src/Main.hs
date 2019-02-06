{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Control.Monad.ST (ST)
import Data.Array.IArray (accumArray)
import qualified Data.Array.IArray as IA
import Data.Array.MArray (readArray, writeArray)
import qualified Data.Array.MArray as MA
import Data.Array (Array, (!))
import Data.Array.ST (STArray, STUArray, runSTArray, runSTUArray)
import Data.Array.Unboxed (UArray)
import Data.List (foldl')
import Data.List.Extra (trim)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Int (Int64)
import Text.Parsec (parse, try)
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (eof, sepBy)
import Text.Parsec.Prim (many)
import Text.Parsec.String (Parser)
import Text.Printf (perror)

type Path = [PathComponent]
data PathComponent = MoveN
                   | MoveS
                   | MoveE
                   | MoveW
                   | Branch [Path]
                   deriving (Show)

data Room = Room Bool Bool Bool Bool
type Board = Array (Int, Int) Room

type STBoard s = STArray s (Int, Int) Room

type Pos = (Int, Int)

pathParser :: Parser Path
pathParser = many oneStepParser

oneStepParser :: Parser PathComponent
oneStepParser =
      try (char 'N' >> return MoveN)
  <|> try (char 'S' >> return MoveS)
  <|> try (char 'E' >> return MoveE)
  <|> try (char 'W' >> return MoveW)
  <|> do _ <- char '('
         alts <- pathParser `sepBy` char '|'
         _ <- char ')'
         return $ Branch alts

inputParser :: Parser Path
inputParser = do
  _ <- char '^'
  path <- many oneStepParser
  _ <- char '$'
  eof
  return path

posShift :: PathComponent -> Pos -> Pos
posShift MoveN (x, y) = (x, y + 1)
posShift MoveS (x, y) = (x, y - 1)
posShift MoveE (x, y) = (x + 1, y)
posShift MoveW (x, y) = (x - 1, y)
posShift (Branch _) _ =
  perror "Branch movement provided to posShift"

reverseMove :: PathComponent -> PathComponent
reverseMove MoveN = MoveS
reverseMove MoveS = MoveN
reverseMove MoveE = MoveW
reverseMove MoveW = MoveE
reverseMove (Branch _) = perror "Branch movement provided to reverseMove"

roomAddDoor :: Room -> PathComponent -> Room
roomAddDoor (Room _ s e w) MoveN = Room True s e w
roomAddDoor (Room n _ e w) MoveS = Room n True e w
roomAddDoor (Room n s _ w) MoveE = Room n s True w
roomAddDoor (Room n s e _) MoveW = Room n s e True
roomAddDoor _ (Branch _) = perror "Branch movement provided to addDoor"

boardAddDoor :: PathComponent -> STBoard s -> Pos -> ST s (STBoard s)
boardAddDoor move board from = do
  let to = posShift move from
  fromRoom <- readArray board from
  toRoom <- readArray board to
  writeArray board from $ roomAddDoor fromRoom move
  writeArray board to (roomAddDoor toRoom $ reverseMove move)
  return board

walkTheBoard :: (Pos, Path) -> Board -> Board
walkTheBoard initial board =
  let go :: (Set Pos, Path)
         -> STBoard s
         -> ST s (Set Pos, STBoard s)
      go (ps, []) mboard = return (ps, mboard)
      go (ps, Branch [] : rest) mboard =
        go (ps, rest) mboard
      go (ps, Branch alts : rest) mboard = do
        (ps', mboard') <- foldM (runBranch ps) (Set.empty, mboard) alts
        go (ps', rest) mboard'
      go (ps, move : rest) mboard =
        do mboard' <- foldM (boardAddDoor move) mboard $ Set.elems ps
           let ps' = Set.map (posShift move) ps
           go (ps', rest) mboard'

      runBranch :: Set Pos
                -> (Set Pos, STBoard s)
                -> Path
                -> ST s (Set Pos, STBoard s)
      runBranch from (ps, mboard) path = do
        (to, mboard') <- go (from, path) mboard
        return (ps `Set.union` to, mboard')

  in runSTArray $ do
    mboard <- MA.thaw board
    snd <$> go (Set.singleton $ fst initial, snd initial) mboard

allPosibleMoves :: Pos -> Room -> [Pos]
allPosibleMoves from (Room doorN doorS doorE doorW) =
     [posShift MoveN from | doorN]
  ++ [posShift MoveS from | doorS]
  ++ [posShift MoveE from | doorE]
  ++ [posShift MoveW from | doorW]

type DistBoard = UArray (Int, Int) Int64
type STDistBoard s = STUArray s (Int, Int) Int64

computeDistances :: Pos -> Board -> DistBoard
computeDistances from board =
  let bounds = IA.bounds board
      walk :: STDistBoard s
           -> [(Int64, Pos)]
           -> ST s (STDistBoard s)
      walk dists [] = return dists
      walk dists ((dist, pos):rest) = do
        posDist <- readArray dists pos
        if posDist >= 0
          then walk dists rest
          else do writeArray dists pos dist
                  let room = board ! pos
                      path = rest
                             ++ map (\p -> (dist + 1, p))
                             (allPosibleMoves pos room)
                  walk dists path
  in runSTUArray $ do
     dists <- MA.newArray bounds (-1)
     walk dists [(0, from)]

furthestRoomDistance :: Pos -> Board -> Int64
furthestRoomDistance from board =
  let dists = computeDistances from board
  in maximum $ IA.elems dists

countRoomsFurtherThan :: Int64 -> Pos -> Board -> Int
countRoomsFurtherThan minDist from board =
  let dists = computeDistances from board
  in length $ filter (>= minDist) $ IA.elems dists

pathCount :: Path -> Int
pathCount path =
  let go :: Int -> Path -> Int
      go !res [] = res
      go !res (Branch alts:ms) =
        let altsCount = foldl' (+) 0 $ map (go 1) alts
        in go (res * altsCount) ms
      go !res (_:ms) = go res ms
  in go 1 path

dumbFurthestRoomDistance :: Path -> Int
dumbFurthestRoomDistance =
  let go :: Int -> Path -> Int
      go !res [] = res
      go !res (Branch alts:ms) =
        let m = maximum $ map (go 0) alts
        in go (res + m) ms
      go !res (_:ms) = go (res + 1) ms
  in go 0

main :: IO ()
main = do
  input <- trim <$> getContents
  mapM_ processLine $ lines input

processLine :: String -> IO ()
processLine line =
  case parse inputParser "<input>" line of
    Left errorText -> do
      putStrLn "Failed for:"
      putStrLn line
      putStrLn "Error:"
      print errorText
    Right path -> do
      putStr "pathCount:                  "
      print $ pathCount path
      putStr "dumbFurthestRoomDistance:   "
      print $ dumbFurthestRoomDistance path
      let arraySize = 1000
          initialBoard =
            accumArray const (Room False False False False)
            ((-arraySize, -arraySize), (arraySize, arraySize)) []
          board = walkTheBoard ((0, 0), path) initialBoard
      putStr "furthestRoomDistance:       "
      print $ furthestRoomDistance (0, 0) board
      putStr "countRoomsFurtherThan 1000: "
      print $ countRoomsFurtherThan 1000 (0, 0) board
      putStrLn ""
