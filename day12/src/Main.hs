{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Data.Array.IArray (assocs, listArray, bounds, elems, (!))
import Data.Array.Unboxed (UArray)
import Data.Bits (shiftL, (.|.), (.&.))
import Data.Ix (range, inRange)
import Data.List (elemIndex, scanl', replicate, takeWhile)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Word (Word8)
import Text.Parsec (parse)
import Text.Parsec.Char (oneOf, string, spaces, newline)
import Text.Parsec.Combinator (eof, many1, sepEndBy)
import Text.Parsec.String (Parser)

newtype Pots = Pots (UArray Int Bool)
  deriving (Eq, Ord)

type Rules = Map Word8 Bool

instance Show Pots where
  show (Pots pots) =
    let (minI, maxI) = bounds pots
    in "[" ++ show minI ++ ", " ++ show maxI ++ "]: "
       ++ map (\case True -> '#'
                     False -> '.') (elems pots)

headerParser :: Parser Pots
headerParser = do
  _ <- string "initial state:"
  spaces
  pots <- map (== '#') <$> many1 (oneOf "#.")
  return $ Pots $ listArray (0, length pots - 1) pots

potsToBits :: [Bool] -> Word8
potsToBits = go . reverse
  where go [] = 0
        go (True:bs) = (go bs `shiftL` 1) .|. 1
        go (False:bs) = go bs `shiftL` 1

noteParser :: Parser (Word8, Bool)
noteParser = do
  spaces
  match <- potsToBits . map (== '#') <$> many1 (oneOf "#.")
  spaces
  _ <- string "=>"
  spaces
  outcome <- (== '#') <$> oneOf "#."
  return (match, outcome)

inputParser :: Parser (Pots, Rules)
inputParser = do
  pots <- headerParser
  _ <- newline
  _ <- newline
  matches <- Map.fromList <$> noteParser `sepEndBy` newline
  eof
  return (pots, matches)

ensureSpaceOnSides :: Pots -> Pots
ensureSpaceOnSides (Pots pots) =
  let atLeast = 5
      (minI, maxI) = bounds pots
      countEmptyPrefix upto =
        length . takeWhile (== False) . take upto
      startExtra =
        atLeast - countEmptyPrefix atLeast (elems pots)
      endExtra =
        atLeast - countEmptyPrefix atLeast (reverse $ elems pots)

      minI' = minI - startExtra
      maxI' = maxI + endExtra
  in Pots $ if startExtra == 0 && endExtra == 0
            then pots
            else listArray (minI', maxI')
                 (replicate startExtra False
                  ++ elems pots
                  ++ replicate endExtra False)

nextGeneration :: Rules -> Pots -> Pots
nextGeneration rules (Pots pots) =
  let (minI, maxI) = bounds pots
      first = potsToBits $ take 5 $ elems pots

      newMatch :: Word8 -> Bool -> Word8
      newMatch prev pot =
        (prev `shiftL` 1) .&. 31 .|. (if pot then 1 else 0)

      matchToState match =
        fromMaybe False $ match `Map.lookup` rules

      updated = map matchToState
        $ scanl' newMatch first
        $ drop 5
        $ elems pots

  in ensureSpaceOnSides
     $ Pots
     $ listArray (minI, maxI)
     $ [False, False] ++ updated ++ [False, False]

showPots :: (Int, Int) -> Pots -> String
showPots ix (Pots pots) =
  let asChar = \case True -> '#'
                     False -> '.'
      present = inRange (bounds pots)
      showOne i = if present i
                  then asChar $ pots ! i
                  else '.'
  in map showOne $ range ix

generationHash :: Pots -> Int
generationHash (Pots pots) =
  sum $ map fst $ filter snd $ assocs pots

shiftToZero :: Pots -> Pots
shiftToZero (Pots pots) =
  let (minI, maxI) = bounds pots
      firstSet = elemIndex True $ elems pots
      lastSet = elemIndex True $ reverse $ elems pots
  in Pots $ case (firstSet, lastSet) of
              (Nothing, _) -> listArray (0, -1) []
              (_, Nothing) -> listArray (0, -1) []
              (Just i, Just j) -> listArray (0, maxI - minI - j - i)
                                  $ drop i
                                  $ elems pots

findRepetition :: [Pots] -> Int -> Maybe Int
findRepetition pots maxSteps =
  let go _ _ 0 = Nothing
      go _ [] _ = Nothing
      go seen (p:ps) !i
        | p `Set.member` seen = Just (maxSteps - i)
        | otherwise = go (p `Set.insert` seen) ps (i-1)
  in go Set.empty (map shiftToZero pots) maxSteps

main :: IO ()
main = do
  input <- getContents
  case parse inputParser "<input>" input of
    Left errorText -> print errorText
    Right (pots, rules) -> do
      let pots' = ensureSpaceOnSides pots
          evolution = iterate (nextGeneration rules) pots'

      print $ generationHash $ evolution !! 20

      let Just repeatsAt = findRepetition evolution 4000
          repetitionHash = generationHash $ evolution !! repeatsAt
          nextHash = generationHash $ evolution !! (repeatsAt + 1)
          diff = nextHash - repetitionHash
          solution2 = repetitionHash + diff * (50_000_000_000 - repeatsAt)

      print solution2

      -- mapM_ (putStrLn . showPots (-3, 35)) $ take 21 evolution

      -- mapM_ (putStrLn . showPots (-3, 200))
      --   $ take 100 evolution
      -- putStrLn ""
      -- mapM_ (putStrLn . showPots (-3, 200))
      --   $ take 100 $ map shiftToZero $ evolution
