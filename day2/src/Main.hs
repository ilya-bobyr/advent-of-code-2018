{-# LANGUAGE BangPatterns #-}

module Main where

import Prelude hiding (id)

import Control.Applicative ((<$>))
import qualified Data.Map.Strict as M
import Data.List (foldl', find, reverse)
import Text.Printf (perror)

countLetters :: String -> (Int, Int)
countLetters id =
  let counts = M.fromListWith (+) $ zip id (repeat (1 :: Int))
      combineCounts (0, v) 2 = (1, v)
      combineCounts (v, 0) 3 = (v, 1)
      combineCounts v      _ = v
  in M.foldl' combineCounts (0, 0) counts

isMatching :: String -> String -> Bool
isMatching id1 id2 =
  let loop :: String -> String -> Bool
      loop (c1:cs1) (c2:cs2) =
        if c1 /= c2
        then cs1 == cs2
        else loop cs1 cs2
      loop [] [] = False
      loop _  _  = perror "Ids not of the same length: '%s', '%s'" id1 id2
  in loop id1 id2

getMatching :: String -> String -> String
getMatching id1 id2 =
  let loop :: String -> String -> String -> String
      loop (c1:cs1) (c2:cs2) prefix =
        if c1 /= c2
        then reverse prefix ++ cs1
        else loop cs1 cs2 (c1:prefix)
      loop _ _ _ = perror "Unexpected ids in getMatching: '%s', '%s'" id1 id2
  in loop id1 id2 ""

main :: IO ()
main = do
  ids <- lines <$> getContents :: IO [String]
  let checksum = uncurry (*)
        $ foldl' (\(!x1, !y1) (!x2, !y2) -> (x1 + x2, y1 + y2)) (0, 0)
        $ map countLetters ids
  print checksum

  let findMatch (Nothing, seen) v1 =
        case find (isMatching v1) seen of
          Nothing -> (Nothing, v1 : seen)
          Just v2 -> (Just (v1, v2), [])
      findMatch (Just res, _) _ = (Just res, [])
      (Just (id1, id2), _) = foldl' findMatch (Nothing, []) ids
  print $ getMatching id1 id2
