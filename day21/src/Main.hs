{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative ((<|>))
import Control.Lens (set)
import Control.Monad.State.Strict (State, get, put, execState)
import Data.Array (Array)
import Data.Array.IArray (listArray)
import qualified Data.Array.IArray as IA
import Data.Bits ((.|.), (.&.))
import Data.Functor.Identity (runIdentity)
import Data.Int (Int64)
import Data.List (foldl1', intercalate)
import Data.Proxy (Proxy(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector.Fixed (mkN, element)
import qualified Data.Vector.Fixed as FV
import Data.Vector.Fixed.Unboxed (Vec)
import Text.Parsec (parse, try)
import Text.Parsec.Char (digit, char, string, space, spaces, newline)
import Text.Parsec.Combinator (eof, many1, sepBy, sepEndBy)
import Text.Parsec.String (Parser)
import Text.Printf (printf, perror)


type CpuState = Vec 6 Int64

mk6 :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Vec 6 Int64
mk6 = mkN (Proxy :: Proxy (Vec 6 Int64))

data Op = Addr Int64 Int64 Int64
        | Addi Int64 Int64 Int64
        | Mulr Int64 Int64 Int64
        | Muli Int64 Int64 Int64
        | Banr Int64 Int64 Int64
        | Bani Int64 Int64 Int64
        | Borr Int64 Int64 Int64
        | Bori Int64 Int64 Int64
        | Setr Int64 Int64 Int64
        | Seti Int64 Int64 Int64
        | Gtir Int64 Int64 Int64
        | Gtri Int64 Int64 Int64
        | Gtrr Int64 Int64 Int64
        | Eqir Int64 Int64 Int64
        | Eqri Int64 Int64 Int64
        | Eqrr Int64 Int64 Int64
        deriving (Eq, Show)

type OpC = Int64 -> Int64 -> Int64 -> Op

update :: Int64 -> Int64 -> CpuState -> CpuState
update i = set (element $ fromIntegral i)

(!) :: FV.Vector v a => v a -> Int64 -> a
{-# INLINE (!) #-}
(!) vec i = vec FV.! fromIntegral i

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
    deviceIpReg :: Int64
  , deviceCpuState :: CpuState
  , deviceOps :: Array Int64 Op
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

inputParser :: Parser (CpuState -> DeviceState)
inputParser = do
  _ <- string "#ip"
  spaces
  ip <- read <$> many1 digit
  _ <- newline
  ops <- opParser `sepEndBy` newline
  eof
  return $ \state -> DeviceState ip state
                     $ listArray (0, fromIntegral $ length ops - 1) ops

showAsAlternativeSyntax :: Int64 -> Array Int64 Op -> String
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

runUntil :: Monad m
         => (DeviceState -> m Bool)
         -> DeviceState
         -> m DeviceState
runUntil cond device = do
  matches <- cond device
  if matches
    then return device
    else case evalOneStep device of
           Left res ->
             perror "Device halted. State: %s" res
           Right device' -> runUntil cond device'

solution1 :: DeviceState -> Int64
solution1 initialState =
  let ipIs expectedIp (DeviceState ipReg cpuState _) =
        return $ cpuState ! ipReg == expectedIp
      f = (! 5)
        $ deviceCpuState
        $ runIdentity
        $ runUntil (ipIs 28) initialState
  in f

solution2 :: DeviceState -> Int64
solution2 initialState =
  let stopOnSameStateAt :: Int64
                        -> DeviceState
                        -> State (Int64, Set Int64) Bool
      stopOnSameStateAt ip (DeviceState ipReg cpuState _)
        | cpuState ! ipReg /= ip = return False
        | otherwise = do
            let f = cpuState ! 5
            (_, seen) <- get
            if f `Set.member` seen
              then return True
              else do put (f, f `Set.insert` seen)
                      return False

  in fst
     $ flip execState (0, Set.empty)
     $ runUntil (stopOnSameStateAt 28) initialState

main :: IO ()
main = do
  input <- getContents
  case parse inputParser "<input>" input of
    Left errorText -> print errorText
    Right getDevice -> do
      let device = getDevice (mk6 0 0 0 0 0 0)
      putStrLn
        $ showAsAlternativeSyntax (deviceIpReg device)
        $ deviceOps device
      putStr "solution1: "
      print $ solution1 device
      putStrLn ""
      putStrLn "This takes ~4 minutes..."
      putStr "solution2: "
      print $ solution2 device
