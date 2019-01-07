{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Arrow (second)
import Data.Bits ((.|.), (.&.))
import Data.List (notElem, partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector.Fixed (mk4, (!), element, convert)
import Data.Vector.Fixed.Unboxed (Vec4)
import Control.Lens (set)
import Text.Parsec (parse)
import Text.Parsec.Char (digit, char, string, spaces, newline)
import Text.Parsec.Combinator (eof, many1, sepBy, sepEndBy)
import Text.Parsec.String (Parser)

type CpuState = Vec4 Int

type UnknownOp = Vec4 Int

data Op = Addr Int Int Int
        | Addi Int Int Int
        | Mulr Int Int Int
        | Muli Int Int Int
        | Banr Int Int Int
        | Bani Int Int Int
        | Borr Int Int Int
        | Bori Int Int Int
        | Setr Int Int Int
        | Seti Int Int Int
        | Gtir Int Int Int
        | Gtri Int Int Int
        | Gtrr Int Int Int
        | Eqir Int Int Int
        | Eqri Int Int Int
        | Eqrr Int Int Int
        deriving (Eq, Show)

type OpC = Int -> Int -> Int -> Op

instance Show OpC where
  show _ = "OpC"

allOpConstructors :: [(Int, OpC)]
allOpConstructors = [0..] `zip`
  [Addr, Addi, Mulr, Muli, Banr, Bani, Borr, Bori, Setr, Seti, Gtir,
   Gtri, Gtrr, Eqir, Eqri, Eqrr]

update :: Int -> Int -> CpuState -> CpuState
update i = set (element i)

evalOp :: Op -> CpuState -> CpuState
evalOp (Addr a b c) state = update c (state ! a + state ! b) state
evalOp (Addi a b c) state = update c (state ! a + b) state
evalOp (Mulr a b c) state = update c (state ! a * state ! b) state
evalOp (Muli a b c) state = update c (state ! a * b) state
evalOp (Banr a b c) state = update c (state ! a .&. state ! b) state
evalOp (Bani a b c) state = update c (state ! a .&. b) state
evalOp (Borr a b c) state = update c (state ! a .|. state ! b) state
evalOp (Bori a b c) state = update c (state ! a .|. b) state
evalOp (Setr a _ c) state = update c (state ! a) state
evalOp (Seti a _ c) state = update c a state
evalOp (Gtir a b c) state = update c (if a > state ! b then 1 else 0) state
evalOp (Gtri a b c) state = update c (if state ! a > b then 1 else 0) state
evalOp (Gtrr a b c) state = update c (if state ! a > state ! b then 1 else 0) state
evalOp (Eqir a b c) state = update c (if a == state ! b then 1 else 0) state
evalOp (Eqri a b c) state = update c (if state ! a == b then 1 else 0) state
evalOp (Eqrr a b c) state = update c (if state ! a == state ! b then 1 else 0) state

runOps :: CpuState -> [Op] -> CpuState
runOps = foldl (flip evalOp)

applyAll :: [(Int, OpC)] -> UnknownOp -> CpuState -> [((Int, OpC), CpuState)]
applyAll opCs unknownOp state =
  let [a, b, c] = map (unknownOp !) [1..3]
      allOps = [((i, opC), opC a b c) | (i, opC) <- opCs]
  in map (\((i, opC), op) -> ((i, opC), evalOp op state)) allOps

data Sample = Sample {
    sampleCpuBefore :: CpuState
  , sampleOp :: UnknownOp
  , sampleCpuAfter :: CpuState
  }
  deriving (Eq, Show)

cpuStateParser :: Parser CpuState
cpuStateParser = do
  _ <- char '['
  [reg0, reg1, reg2, reg3] <-
    map read <$> many1 digit `sepBy` string ", "
  _ <- char ']'
  return $ mk4 reg0 reg1 reg2 reg3

unknownOpParser :: Parser UnknownOp
unknownOpParser = do
  [opcode, a, b, c] <-
    map read <$> many1 digit `sepBy` char ' '
  return $ mk4 opcode a b c

sampleParser :: Parser Sample
sampleParser = do
  _ <- string "Before:"
  spaces
  before <- cpuStateParser
  _ <- newline

  uop <- unknownOpParser
  _ <- newline

  spaces
  _ <- string "After:"
  spaces
  after <- cpuStateParser
  _ <- newline

  return $ Sample before uop after

inputParser :: Parser ([Sample], [UnknownOp])
inputParser = do
  samples <- sampleParser `sepEndBy` newline
  _ <- newline >> newline
  program <- unknownOpParser `sepEndBy` newline
  eof
  return (samples, program)

findMatching :: [(Int, OpC)] -> Sample -> [(Int, OpC)]
findMatching opCs (Sample before uop after) =
  map fst
  $ filter ((== after) . snd)
  $ applyAll opCs uop before

matchingCount :: [(Int, OpC)] -> Sample -> Int
matchingCount opCs sample =
  length $ findMatching opCs sample

inferOps :: [Sample] -> Map Int [(Int, OpC)]
inferOps =
  let initial = Map.fromList [(i, allOpConstructors) | i <- [0..15]]
      reduce known (sample@(Sample _ uop _):samples') =
        let opcode = uop ! 0
            possibilities = known Map.! opcode
            possibilities' = findMatching possibilities sample
            known' = Map.insert opcode possibilities' known
        in reduce known' samples'
      reduce known [] = known
  in reduce initial

simplifyOps :: Map Int [(Int, OpC)] -> Maybe (Map Int OpC)
simplifyOps known =
  let simplify :: [(Int, (Int, OpC))]
               -> [(Int, [(Int, OpC)])]
               -> Maybe [(Int, (Int, OpC))]
      simplify simple [] = Just simple
      simplify simple next =
        let (singles, complex) = partition ((== 1) . length . snd) next
            singles' = map (second head) singles
            remove = map (fst . snd) singles'
            complex' = map (second (filter ((`notElem` remove) . fst))) complex
        in if null singles'
           then Nothing
           else simplify (simple ++ singles') complex'
  in (Map.fromList
      . map (\(i, (_, opc)) -> (i, opc)))
     <$> simplify [] (Map.assocs known)

selectOps :: Map Int OpC -> [UnknownOp] -> [Op]
selectOps _ [] = []
selectOps known ((convert -> (uopcode, a, b, c)):uops) =
  let opc = known Map.! uopcode
  in opc a b c : selectOps known uops

solution1 :: [Sample] -> Int
solution1 samples =
  length . filter (>= 3)
  $ map (matchingCount allOpConstructors) samples

solution2 :: ([Sample], [UnknownOp]) -> Maybe Int
solution2 (samples, program) = do
  mapping <- simplifyOps $ inferOps samples
  let program' = selectOps mapping program
  return $ (! 0) $ runOps (mk4 0 0 0 0) program'

main :: IO ()
main = do
  input <- getContents
  case parse inputParser "<input>" input of
    Left errorText -> print errorText
    Right (samples, program) -> do
      -- let sample = Sample {
      --       sampleCpuBefore = mk4 3 2 1 1
      --       , sampleOp = mk4 9 2 1 2
      --       , sampleCpuAfter = mk4 3 2 2 1
      --       }
      -- print sample
      -- print $ applyAll (sampleOp sample) (sampleCpuBefore sample)
      -- print $ matchingCount sample
      print $ solution1 samples
      print $ solution2 (samples, program)
