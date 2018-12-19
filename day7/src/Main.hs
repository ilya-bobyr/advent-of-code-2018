{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Bits (bit, (.|.), (.&.))
import Data.List (foldl', sort)
import Data.List.Extra (nubOrd)
import qualified Data.Map.Strict as Map
import Data.Word (Word8, Word64)
import Text.Parsec (parse)
import Text.Parsec.Char (upper, string, spaces, newline)
import Text.Parsec.Combinator (eof, sepEndBy)
import Text.Parsec.String (Parser)
import Text.Printf (perror)

type Step = Word8
type Dependency = (Step, Word64)

charToStep :: Char -> Step
charToStep c | fromEnum c >= fromEnum 'A'
             && fromEnum c <= fromEnum 'Z' =
                 fromIntegral $ fromEnum c - fromEnum 'A'
charToStep c = perror "Unexpected character: %s" c

stepToChar :: Step -> Char
stepToChar from =
  toEnum $ fromIntegral from + fromEnum 'A'

lineParser :: Parser (Dependency, [Step])
lineParser = do
  _ <- string "Step"
  spaces
  dep <- charToStep <$> upper :: Parser Step
  spaces
  _ <- string "must be finished before step"
  spaces
  target <- charToStep <$> upper :: Parser Step
  spaces
  _ <- string "can begin."
  return ((target, bit $ fromIntegral dep), [target, dep])

inputParser :: Parser [Dependency]
inputParser = do
  allDeps <- lineParser `sepEndBy` newline
  eof
  let allSteps = sort . nubOrd . concat $ map snd allDeps
      zeroDeps = Map.fromList $ map (\v -> (v, 0)) allSteps
      combinedDeps = Map.toList
        $ Map.fromListWith (.|.) (map fst allDeps) `Map.union` zeroDeps
  return combinedDeps

solution1 :: [Dependency] -> String
solution1 deps =
  let go _ [] res = reverse res
      go complete pending res =
        let allowed (_, dep) = dep .&. complete == dep
            (selected, _) = minimum $ filter allowed pending
            complete' = complete .|. (bit . fromIntegral $ selected)
            pending' = filter (\(step, _) -> step /= selected) pending
            res' = stepToChar selected : res
        in go complete' pending' res'
  in go 0 deps ""

count :: (a -> Bool) -> [a] -> Int
count p = go 0
  where go !n [] = n
        go !n (x:xs) | p x       = go (n+1) xs
                     | otherwise = go n xs

data Worker = Free
            | Busy Step Int
  deriving (Eq, Show)

solution2 :: Int -> (Step -> Int) -> [Dependency] -> Int
solution2 workerCount stepLength deps =
  let
    freeCount = count (== Free)
    nextComplete =
      minimum . map (\(Busy _ t) -> t) . filter (/= Free)
    makeBusy workers [] = workers
    makeBusy [] _ = error "Ran out of work"
    makeBusy (Free : workers) (step : steps) =
      Busy step (stepLength step) : makeBusy workers steps
    makeBusy (busy : workers) steps =
      busy : makeBusy workers steps

    forwardTime [] _ res !complete =
      (res, complete)
    forwardTime (Free : workers) by res complete =
      forwardTime workers by (Free : res) complete
    forwardTime (Busy step t : workers) by res complete
      | t == by =
        let stepAsBit = bit . fromIntegral $ step
        in forwardTime workers by (Free : res) (complete .|. stepAsBit)
      | otherwise =
        forwardTime workers by (Busy step (t - by) : res) complete

    go :: Word64 -> [Dependency] -> [Worker] -> Int -> Int
    go _ [] workers !second =
      second + foldl' max 0 (map (\(Busy _ t) -> t)
                             $ filter (/= Free) workers)
    go complete pending workers !second =
      let
        takeUpTo = freeCount workers
        allowed (_, dep) = dep .&. complete == dep
        selected = map fst $ take takeUpTo $ sort $ filter allowed pending
        workers' = makeBusy workers selected
        forwardBy = nextComplete workers'
        (workers'', complete') =
          forwardTime workers' forwardBy [] complete
        pending' = filter ((`notElem` selected) . fst) pending
      in go complete' pending' workers'' (second + forwardBy)

  in go 0 deps (replicate workerCount Free) 0

main :: IO ()
main = do
  input <- getContents
  case parse inputParser "<input>" input of
    Left errorText -> print errorText
    Right deps -> do
      print $ solution1 deps
      -- print $ solution2 2 (fromIntegral . (+1)) deps
      print $ solution2 5 (fromIntegral . (+61)) deps
