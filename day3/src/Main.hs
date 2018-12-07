{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Array.ST (STUArray)
import qualified Data.Array.ST as STUA
import qualified Data.Array.MArray as MA
import qualified Data.Ix as Ix
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word8, Word32)
import Control.Monad (foldM)
import Control.Monad.ST (ST, runST)
import Text.Parsec (parse)
import Text.Parsec.Char (char, digit, spaces, newline)
import Text.Parsec.Combinator (many1, eof, sepEndBy)
import Text.Parsec.String (Parser)

newtype Claim = Claim (Word32, Int, Int, Int, Int)
  deriving (Show)

claimId :: Claim -> Word32
claimId (Claim (cId, _, _, _, _)) = cId

rowParser :: Parser Claim
rowParser = do
  _ <- char '#'
  cId <- read <$> many1 digit :: Parser Word32
  spaces
  _ <- char '@'
  spaces
  leftPadding <- read <$> many1 digit :: Parser Int
  _ <- char ','
  topPadding <- read <$> many1 digit :: Parser Int
  _ <- char ':'
  spaces
  width <- read <$> many1 digit :: Parser Int
  _ <- char 'x'
  height <- read <$> many1 digit :: Parser Int
  return $ Claim (cId, leftPadding, topPadding, width, height)

inputParser :: Parser [Claim]
inputParser = do
  allClaims <- rowParser `sepEndBy` newline
  eof
  return allClaims

cutForOneClaim :: STUArray s (Int, Int) Word8
               -> Claim
               -> ST s (STUArray s (Int, Int) Word8)
cutForOneClaim fabric (Claim (_, left, top, width, height)) =
  foldM (\arr i -> do
            prev <- STUA.readArray arr i
            let new = if prev < 2 then prev + 1 else prev
            STUA.writeArray arr i new
            return arr
        )
    fabric
    (Ix.range ((left, top), (left+width-1, top+height-1)))

countOverlaps :: Int -> [Claim] -> Int
countOverlaps size claims =
  runST $ do
    empty <- MA.newArray ((0, 0), (size, size)) 0
    fabric <- foldM cutForOneClaim empty claims
    length . filter (> 1) <$> MA.getElems fabric

type OverlapFabric s = STUArray s (Int, Int) Word32
type OverlappingIds = Set Word32

findNonOverlapping :: Int -> [Claim] -> Word32
findNonOverlapping size claims =
  let overlayOneClaim :: OverlapFabric s
                      -> OverlappingIds
                      -> Claim
                      -> ST s OverlappingIds
      overlayOneClaim fabric overlapping
        (Claim (cId, left, top, width, height)) =
        foldM (\oSet i -> do
                  curr <- STUA.readArray fabric i
                  case curr of
                    0 -> do
                      STUA.writeArray fabric i cId
                      return oSet
                    existingId -> return
                                  $ Set.insert cId
                                  $ Set.insert existingId
                                  oSet
              )
          overlapping
         (Ix.range ((left, top), (left+width-1, top+height-1)))
  in runST $ do
    empty <- MA.newArray ((0, 0), (size, size)) 0
    overlapping <- foldM (overlayOneClaim empty) Set.empty claims
    return
      $ head
      $ filter (`Set.notMember` overlapping)
      $ map claimId claims

main :: IO ()
main = do
  input <- getContents
  case parse inputParser "<intput>" input of
    Left errorText -> print errorText
    Right claims -> do
      print $ countOverlaps 1000 claims
      print $ findNonOverlapping 1000 claims
