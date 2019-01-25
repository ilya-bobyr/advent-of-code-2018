{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Applicative ((<|>))
import Control.Lens (set)
import Data.Array.IArray (listArray)
import qualified Data.Array.IArray as IA
import Data.Array (Array)
import Data.Bits ((.|.), (.&.))
import Data.List (foldl1', intercalate)
import Data.Proxy (Proxy(..))
import Data.Vector.Fixed (mkN, (!), element)
import Data.Vector.Fixed.Unboxed (Vec)
import Text.Parsec (parse, try)
import Text.Parsec.Char (digit, char, string, space, spaces, newline)
import Text.Parsec.Combinator (eof, many1, sepBy, sepEndBy)
import Text.Parsec.String (Parser)
import Text.Printf (printf, perror)

type CpuState = Vec 6 Int

mk6 :: Int -> Int -> Int -> Int -> Int -> Int -> Vec 6 Int
mk6 = mkN (Proxy :: Proxy (Vec 6 Int))

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

data DeviceState = DeviceState {
    deviceIpReg :: Int
  , deviceCpuState :: CpuState
  , deviceOps :: Array Int Op
  }
  deriving (Eq, Show)

evalOneStep :: DeviceState -> Either DeviceState DeviceState
evalOneStep device@(DeviceState ipReg cpuState ops) =
  let ip = cpuState ! ipReg
      (minIp, maxIp) = IA.bounds ops
  in if ip < minIp || ip > maxIp
     then Left device
     else let op = ops IA.! ip
              cpuState' = evalOp op cpuState
              ip' = cpuState' ! ipReg + 1
              cpuState'' = update ipReg ip' cpuState'
          in Right $ DeviceState ipReg cpuState'' ops

evalDevice :: DeviceState -> DeviceState
evalDevice device =
  case evalOneStep device of
    Left res -> res
    Right device' -> evalDevice device'

individualOpParser :: (String, OpC) -> Parser Op
individualOpParser (name, opC) = do
  _ <- string name >> space
  [a, b, c] <- map read <$> many1 digit `sepBy` char ' '
  return $ opC a b c

opParser :: Parser Op
opParser =
  foldl1' (<|>)
  $ map (try . individualOpParser) [ ("addr", Addr)
                                   , ("addi", Addi)
                                   , ("mulr", Mulr)
                                   , ("muli", Muli)
                                   , ("banr", Banr)
                                   , ("bani", Bani)
                                   , ("borr", Borr)
                                   , ("bori", Bori)
                                   , ("setr", Setr)
                                   , ("seti", Seti)
                                   , ("gtir", Gtir)
                                   , ("gtri", Gtri)
                                   , ("gtrr", Gtrr)
                                   , ("eqir", Eqir)
                                   , ("eqri", Eqri)
                                   , ("eqrr", Eqrr)
                                   ]

runAndGet0 :: DeviceState -> Int
runAndGet0 device =
  let device' = evalDevice device
  in deviceCpuState device' ! 0

runUntil :: (DeviceState -> Bool) -> DeviceState -> DeviceState
runUntil cond device | cond device = device
runUntil cond device =
  case evalOneStep device of
    Left res ->
      perror "Device halted before the condition was satisfied. State: %s"
      res
    Right device' -> runUntil cond device'

showAsAlternativeSyntax :: Int -> Array Int Op -> String
showAsAlternativeSyntax ipReg ops =
  let (minIp, _) = IA.bounds ops

      rn reg =
        if reg == ipReg then "#ip"
        else case reg of
          0 -> "a"
          1 -> "b"
          2 -> "c"
          3 -> "d"
          4 -> "e"
          5 -> "f"
          _ -> error "Unexpected register index"

      showOp =
        \case (Addr a b c) ->
                printf "%s = %s + %s" (rn c) (rn a) (rn b)
              (Addi a b c) ->
                printf "%s = %s + %d" (rn c) (rn a) b
              (Mulr a b c) ->
                printf "%s = %s * %s" (rn c) (rn a) (rn b)
              (Muli a b c) ->
                printf "%s = %s * %d" (rn c) (rn a) b
              (Banr a b c) ->
                printf "%s = %s & %s" (rn c) (rn a) (rn b)
              (Bani a b c) ->
                printf "%s = %s & %d" (rn c) (rn a) b
              (Borr a b c) ->
                printf "%s = %s | %s" (rn c) (rn a) (rn b)
              (Bori a b c) ->
                printf "%s = %s | %d" (rn c) (rn a) b
              (Setr a _ c) ->
                printf "%s = %s" (rn c) (rn a)
              (Seti a _ c) ->
                printf "%s = %d" (rn c) a
              (Gtir a b c) ->
                printf "%s = %d > %s ? 1 : 0" (rn c) a (rn b)
              (Gtri a b c) ->
                printf "%s = %s > %d ? 1 : 0" (rn c) (rn a) b
              (Gtrr a b c) ->
                printf "%s = %s > %s ? 1 : 0" (rn c) (rn a) (rn b)
              (Eqir a b c) ->
                printf "%s = %d == %s ? 1 : 0" (rn c) a (rn b)
              (Eqri a b c) ->
                printf "%s = %s == %d ? 1 : 0" (rn c) (rn a) b
              (Eqrr a b c) ->
                printf "%s = %s == %s ? 1 : 0" (rn c) (rn a) (rn b)

  in intercalate "\n"
     $ zipWith (++) (map (printf "%d: ") [minIp..]) (map showOp $ IA.elems ops)

inputParser :: Parser (CpuState -> DeviceState)
inputParser = do
  _ <- string "#ip"
  spaces
  ip <- read <$> many1 digit
  _ <- newline
  ops <- opParser `sepEndBy` newline
  eof
  return $ \state -> DeviceState ip state
                     $ listArray (0, length ops - 1) ops

optimizedCalculation :: DeviceState -> Int
optimizedCalculation initialState =
  let ipIs expectedIp (DeviceState ipReg cpuState _) =
        cpuState ! ipReg == expectedIp
      c = (! 3) $ deviceCpuState $ runUntil (ipIs 1) initialState
  in sum [i | i <- [1..c]
            , c `mod` i == 0]

main :: IO ()
main = do
  input <- getContents
  case parse inputParser "<input>" input of
    Left errorText -> print errorText
    Right getDevice -> do
      let device = getDevice (mk6 0 0 0 0 0 0)
      print $ runAndGet0 device

      -- putStrLn ""
      -- putStrLn
      --   $ showAsAlternativeSyntax (deviceIpReg device)
      --   $ deviceOps device

      print $ optimizedCalculation device

      let device2 = getDevice (mk6 1 0 0 0 0 0)
      print $ optimizedCalculation device2
