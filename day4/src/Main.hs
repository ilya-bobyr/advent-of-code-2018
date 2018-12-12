{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude hiding (log)

import Data.Bits (setBit, popCount, testBit)
import Data.List (foldl', maximumBy, elemIndex)
import Data.Ord (comparing)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sort (sortOn)
import Data.Word (Word64)
import Text.Parsec (parse)
import Text.Parsec.Prim (try, (<|>))
import Text.Parsec.Char (char, digit, space, spaces, string, newline)
import Text.Parsec.Combinator (many1, eof, sepEndBy)
import Text.Parsec.String (Parser)
import Text.Printf (printf, perror)

data LogEvent = BeginShift Int
              | FallAsleep
              | WakeUp
  deriving (Show)

data LogDate = LogDate Int Int Int
  deriving (Eq, Ord)

instance Show LogDate where
  show (LogDate year month day) = printf "%d-%02d-%02d" year month day

data LogTime = LogTime Int Int
  deriving (Eq, Ord)

instance Show LogTime where
  show (LogTime hour minute) = printf "%02d:%02d" hour minute

data LogDateTime = LogDateTime LogDate LogTime
  deriving (Eq, Ord)

instance Show LogDateTime where
  show (LogDateTime date time) = show date ++ " " ++ show time

data LogEntry = LogEntry {
  unLogDateTime :: LogDateTime,
  unLogEvent :: LogEvent
}
  deriving (Show)

logDateTime :: Int -> Int -> Int -> Int -> Int -> LogDateTime
logDateTime year month day hour minute =
  LogDateTime (LogDate year month day) (LogTime hour minute)

rowParser :: Parser LogEntry
rowParser = do
  _ <- char '['
  year <- read <$> many1 digit :: Parser Int
  _ <- char '-'
  month <- read <$> many1 digit :: Parser Int
  _ <- char '-'
  day <- read <$> many1 digit :: Parser Int
  _ <- space
  hour <- read <$> many1 digit :: Parser Int
  _ <- char ':'
  minute <- read <$> many1 digit :: Parser Int
  _ <- char ']'
  spaces
  event <-     try (do _ <- string "Guard #"
                       gId <- read <$> many1 digit :: Parser Int
                       _ <- string " begins shift"
                       return $ BeginShift gId)
           <|> try (do _ <- string "falls asleep"
                       return FallAsleep)
           <|> try (do _ <- string "wakes up"
                       return WakeUp)
  let datetime = logDateTime year month day hour minute
  return $ LogEntry datetime event

inputParser :: Parser [LogEntry]
inputParser = do
  allClaims <- rowParser `sepEndBy` newline
  eof
  return allClaims

data GuardState = Awake | Asleep

orderedEventsToStats :: [LogEntry] -> Map Int [Word64]
orderedEventsToStats (LogEntry _ (BeginShift firstGId) : events) =
  let addLog gId log =
        Map.alter (\case Nothing -> Just [log]
                         Just prev -> Just (log : prev)) gId

      extendLog from to Asleep log | from < to =
        extendLog (from + 1) to Asleep (log `setBit` from)
      extendLog _ _ _ log = log

      combine gId from state log [] res =
        let log' = extendLog from 59 state log
        in addLog gId log' res
      combine gId from state log
        (LogEntry _ (BeginShift newGId) : events') res =
        let log' = extendLog from 59 state log
        in combine newGId 0 Awake 0 events' (addLog gId log' res)
      combine gId from state log
        (LogEntry (LogDateTime _ (LogTime _ minute)) FallAsleep : events') res =
        let log' = extendLog from minute state log
        in combine gId minute Asleep log' events' res
      combine gId from state log
        (LogEntry (LogDateTime _ (LogTime _ minute)) WakeUp : events') res =
        let log' = extendLog from minute state log
        in combine gId minute Awake log' events' res

  in combine firstGId 0 Awake 0 events Map.empty

orderedEventsToStats unexpected =
  perror "Unexpected first log entry: %s" unexpected

perMinutStats :: [Word64] -> [Int]
perMinutStats logs =
  let addLog res log =
        map (\(i, r) -> if log `testBit` i then r + 1 else r)
        $ zip [0..] res
  in foldl' addLog (replicate 60 0) logs

strategy1 :: Map Int [Word64] -> Int
strategy1 stats =
  let sleepAmounts = Map.map (foldl' (\a v -> a + popCount v) 0) stats
      topSleeper = fst . maximumBy (comparing snd) $ Map.toList sleepAmounts
      topSleeperStats = perMinutStats $ stats Map.! topSleeper
      bestMinute =
        fst . maximumBy (comparing snd) $ zip [0..] topSleeperStats
  in topSleeper * bestMinute

strategy2 :: Map Int [Word64] -> Int
strategy2 stats =
  let perMinute = Map.map perMinutStats stats
      sameMinuteMax = Map.map maximum perMinute
      (topSleeper, maxMinute) =
        maximumBy (comparing snd) $ Map.toList sameMinuteMax
      (Just bestMinute) = elemIndex maxMinute $ perMinute Map.! topSleeper
  in topSleeper * bestMinute

main :: IO ()
main = do
  input <- getContents
  case parse inputParser "<input>" input of
    Left errorText -> print errorText
    Right events -> do
      let orderedEvents = sortOn unLogDateTime events
          stats = orderedEventsToStats orderedEvents
      print $ strategy1 stats
      print $ strategy2 stats
