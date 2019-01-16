{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Data.Array.IArray (listArray, (!))
import qualified Data.Array.IArray as IA
import Data.Array (Array)
import Data.List (foldl', intercalate)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import System.Environment (getArgs)
import System.IO (Handle, hGetContents, IOMode(ReadMode), stdin, withFile)

data Acre = Open
          | Trees
          | Lumberyard
          deriving (Eq, Ord)

acreAsChar :: Acre -> Char
acreAsChar = \case Open -> '.'
                   Trees -> '|'
                   Lumberyard -> '#'

type Area = Array (Int, Int) Acre

showArea :: Area -> String
showArea area =
    let (_, (maxX, _)) = IA.bounds area
    in intercalate "\n"
       $ chunksOf (maxX + 1)
       $ map acreAsChar
       $ IA.elems area

parseInput :: [String] -> Either String Area
parseInput input =
  let rows = length input
      columns = length $ head input

      go :: (Int, Int) -> [Acre] -> [String] -> Either String Area
      go _ acres [] =
        Right $ listArray ((0, 0), (rows - 1, columns - 1)) (reverse acres)
      go (!y, !x) acres ((c:cs):ls)
        | c == '.' = go (y, x+1) (Open : acres) (cs:ls)
        | c == '|' = go (y, x+1) (Trees : acres) (cs:ls)
        | c == '#' = go (y, x+1) (Lumberyard : acres) (cs:ls)
        | otherwise = Left
          $ printf "Unexpected character '%s' at (%d, %d)" c x y
      go (!y, !x) acres ([]:ls)
        | x /= columns = Left
          $ printf ("Unexpected row length in line %d\n"
                    ++ "Expected: %d, actual: %d") y columns x
        | otherwise =
          go (y+1, 0) acres ls

  in go (0, 0) [] input

oneMinute :: Area -> Area
oneMinute area =
  let ((minY, minX), (maxY, maxX)) = IA.bounds area
      surroundingIndices (!y, !x) =
        filter (\(y, x) -> y >= minY && y <= maxY
                           && x >= minX && x <= maxX)
        [ (y-1, x-1), (y-1, x), (y-1, x+1)
        , (y, x-1), (y, x+1)
        , (y+1, x-1), (y+1, x), (y+1, x+1)
        ]
      countSurrounding target pos =
        length . filter (== target) . map (area !) $ surroundingIndices pos

      nextState pos =
        case area ! pos of
          Open | countSurrounding Trees pos >= 3 -> Trees
          Open -> Open
          Trees | countSurrounding Lumberyard pos >= 3 -> Lumberyard
          Trees -> Trees
          Lumberyard | countSurrounding Lumberyard pos >= 1
                       && countSurrounding Trees pos >= 1 -> Lumberyard
          Lumberyard -> Open
  in listArray (IA.bounds area) [nextState (y, x) | y <- [minY..maxY]
                                                  , x <- [minX..maxX]]

countType :: Acre -> Area -> Int
countType target =
  length . filter (== target) . IA.elems

solution1 :: Area -> Int
solution1 area =
  let area' = foldl' (\a _ -> oneMinute a) area [(1 :: Int)..10]
      wooded = countType Trees area'
      lumberyards = countType Lumberyard area'
  in wooded * lumberyards

findRepetition :: [Area] -> Int -> Maybe (Int, Int)
findRepetition evolution maxMinutes =
  let insertLookup = Map.insertLookupWithKey (\_ a _ -> a)
      go _ _ !i | i >= maxMinutes = Nothing
      go _ [] _ = Nothing
      go seen (area:as) !i =
        case insertLookup area i seen of
          (Just j, _) -> Just (j, i)
          (Nothing, seen') -> go seen' as (i+1)
  in go Map.empty evolution 0

solution2 :: Area -> Int
solution2 area =
  let evolution = iterate oneMinute area
      Just (repeatsAt, nextMatchAt) = findRepetition evolution 1000
      cycleLength = nextMatchAt - repeatsAt

      solutionAt =
        repeatsAt + (1_000_000_000 - repeatsAt) `mod` cycleLength

      area' = evolution !! solutionAt
      wooded = countType Trees area'
      lumberyards = countType Lumberyard area'
    in wooded * lumberyards

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
    Left errorText -> print errorText
    Right area -> do
      print $ solution1 area
      print $ solution2 area

      -- let evolution = iterate oneMinute area
      -- mapM_ (putStrLn . (\a -> showArea a ++ "\n"))
      --        $ take 30 evolution
