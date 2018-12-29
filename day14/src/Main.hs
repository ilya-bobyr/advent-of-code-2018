{-# LANGUAGE BangPatterns  #-}

module Main where

import Control.Monad.ST (runST, ST)
import Data.Vector.Unboxed.Mutable (STVector)
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Word (Word8)
import System.Environment (getArgs)

type Recepies s = STVector s Word8

type Lab s = (Recepies s, Int, (Int, Int))

oneTick :: Lab s -> ST s (Lab s)
oneTick (recepies, len, (e1, e2)) = do
  r1 <- recepies `MV.read` e1
  r2 <- recepies `MV.read` e2
  let combined = r1 + r2
      newPosition len' =
        ( (e1 + fromIntegral r1 + 1) `rem` len'
        , (e2 + fromIntegral r2 + 1) `rem` len')

  if combined < 10
    then do MV.write recepies len combined
            return (recepies, len + 1, newPosition (len + 1))
    else do MV.write recepies (len + 1) (combined `rem` 10)
            return (recepies, len + 2, newPosition (len + 2))

generateAtLeastST :: Int -> ST s (Lab s)
generateAtLeastST minLength =
  let go :: Lab s -> ST s (Lab s)
      go (recepies, len, elves)
        | len < minLength =
          do next <- oneTick (recepies, len, elves)
             go next
        | otherwise = return (recepies, len, elves)
  in do vec <- MV.replicate (minLength + 2) 1
        MV.write vec 0 3
        MV.write vec 1 7
        go (vec, 2, (0, 1))

generateAtLeast :: Int -> (Vector Word8, Int, (Int, Int))
generateAtLeast minLength =
  runST $ do (res, len, elves) <- generateAtLeastST minLength
             frozen <- V.freeze res
             return (frozen, len, elves)

solution1 :: Int -> String
solution1 skipRecepies =
  let minLength = skipRecepies + 10
      (allRecepeies, _, _) = generateAtLeast minLength
      targetScores = V.take 10 $ V.drop skipRecepies allRecepeies
  in map (toEnum . (fromEnum '0' +) . fromIntegral)
     $ V.toList targetScores

tailMatches :: [Word8] -> Recepies s -> Int -> ST s Bool
tailMatches expected recepies offset = do
  let rLen = MV.length recepies
      eLen = length expected
  actual <- mapM (MV.read recepies)
            [rLen - eLen - offset .. rLen - offset - 1]
  return $ actual == expected

vectorMatches :: [Word8] -> Recepies s -> Int -> ST s Bool
vectorMatches expected recepies i =
  let go !j (e:es) =
        do actual <- MV.read recepies j
           if actual == e
             then go (j+1) es
             else return False
      go _ [] = return True
  in go i expected

showReceipts :: (Recepies s, Int) -> ST s String
showReceipts (recepits, len) =
  mapM (\i -> do
           v <- MV.read recepits i
           return $ toEnum . (fromEnum '0' +) . fromIntegral $ v
       ) [0..len - 1]

solution2 :: Int -> String -> Maybe Int
solution2 searchSize expectedStr =
  let expected =
        map (fromIntegral . subtract (fromEnum '0') . fromEnum) expectedStr

      search !i !maxI recepies
        | i >= maxI = return Nothing
        | otherwise =
          do match <- vectorMatches expected recepies i
             if match
               then return $ Just i
               else search (i+1) maxI recepies

  in runST $ do
    (recepies, len, _) <- generateAtLeastST searchSize
    search 0 len recepies

main :: IO ()
main = do
  input <- head <$> getArgs :: IO String
  searchArea <- read . (!! 1) <$> getArgs :: IO Int

  -- putStr "Actual:   "
  -- putStrLn $ solution1 9
  -- putStrLn "Expected: 5158916779"

  -- putStr "Actual:   "
  -- putStrLn $ solution1 5
  -- putStrLn "Expected: 0124515891"

  -- putStr "Actual:   "
  -- putStrLn $ solution1 18
  -- putStrLn "Expected: 9251071085"

  -- putStr "Actual:   "
  -- putStrLn $ solution1 2018
  -- putStrLn "Expected: 5941429882"

  -- putStr "Actual:   "
  -- print $ solution2 100 "51589"
  -- putStrLn "Expected: 9"

  -- putStr "Actual:   "
  -- print $ solution2 100 "01245"
  -- putStrLn "Expected: 5"

  -- putStr "Actual:   "
  -- print $ solution2 100 "92510"
  -- putStrLn "Expected: 18"

  -- putStr "Actual:   "
  -- print $ solution2 10000 "59414"
  -- putStrLn "Expected: 2018"


  -- let recepiesText =
  --       runST $ do (recepies, len, _) <- generateAtLeastST 100
  --                  showReceipts (recepies, len)

  -- putStrLn recepiesText

  putStrLn $ solution1 $ read input

  case solution2 searchArea input of
    Nothing -> putStrLn "Nothing found"
    Just i -> print i
