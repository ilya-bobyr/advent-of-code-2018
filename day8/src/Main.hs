{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List (splitAt, foldl')
import Text.Printf (perror)


data Node = Node [Node] [Int]
  deriving (Show)

buildTree :: [Int] -> Node
buildTree input =
  let
    extractChildren :: Int -> [Int] -> [Node] -> ([Node], [Int])
    extractChildren  0 left res = (reverse res, left)
    extractChildren !n left res =
      let (child, rest) = takeChild left
      in extractChildren (n-1) rest (child : res)

    takeChild (childCount : metadataCount : rest) =
        let (children, rest') = extractChildren childCount rest []
            (metadata, rest'') = splitAt metadataCount rest'
        in (Node children metadata, rest'')
    takeChild invalid = perror "Input is too short: %s" invalid

    (root, leftovers) = takeChild input
  in case leftovers of
    [] -> root
    _ -> perror "Leftovers: %s" leftovers

sumMetadata :: Node -> Int
sumMetadata (Node children metadata) =
  sum (map sumMetadata children) + sum metadata

nodeValue :: Node -> Int
nodeValue (Node [] metadata) = sum metadata
nodeValue (Node children metadata) =
  let totalChildren = length children
      valueAt i | i < 1 || i > totalChildren = 0
                | otherwise = nodeValue $ children !! (i - 1)
  in foldl' (\s i -> s + valueAt i) 0 metadata

main :: IO ()
main = do
  input <- map read . words <$> getContents :: IO [Int]
  let tree = buildTree input
  print $ sumMetadata tree
  print $ nodeValue tree
