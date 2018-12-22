{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Ix (range)
import Data.Ord (comparing)
import Data.List (foldl', minimumBy, sortOn, intercalate)
import Data.List.Split (chunksOf)
import Data.Tuple (swap)
import Data.Tuple.Extra (thd3)
import Text.Parsec (parse)
import Text.Parsec.Char (char, digit, string, spaces, newline)
import Text.Parsec.Combinator (eof, many1, option, sepEndBy)
import Text.Parsec.String (Parser)

data Point = Point (Int, Int) (Int, Int)
  deriving (Show, Eq, Ord)

numberParser :: Parser Int
numberParser = do
  sign <- option '+' $ char '-'
  value <- read <$> many1 digit
  return $ if sign == '+'
           then value
           else -value

lineParser :: Parser Point
lineParser = do
  _ <- string "position=<"
  spaces
  posX <- numberParser
  _ <- char ','
  spaces
  posY <- numberParser
  _ <- string "> velocity=<"
  spaces
  velX <- numberParser
  _<- char ','
  spaces
  velY <- numberParser
  _ <- string ">"
  return $ Point (posX, posY) (velX, velY)

inputParser :: Parser [Point]
inputParser = do
  points <- lineParser `sepEndBy` newline
  eof
  return points

coords :: Point -> (Int, Int)
coords (Point pos _) = pos

boundingRectangle :: [Point] -> ((Int, Int), (Int, Int))
boundingRectangle [] = error "Expect at least one point"
boundingRectangle (Point (firstX, firstY) _ : points) =
  let initial = ((firstX, firstY), (firstX, firstY))
      extend ((!minX, !minY), (!maxX, !maxY)) (!x, !y) =
        ( ( if x < minX then x else minX
          , if y < minY then y else minY)
        , ( if x > maxX then x else maxX
          , if y > maxY then y else maxY)
        )
  in foldl' extend initial $ map coords points

rectangleArea :: ((Int, Int), (Int, Int)) -> Int
rectangleArea ((minX, minY), (maxX, maxY)) =
  (maxX - minX) * (maxY - minY)

moveOneSecond :: [Point] -> [Point]
moveOneSecond points =
  let move (Point (!x, !y) v@(!dx, !dy)) =
        Point (x + dx, y + dy) v
  in map move points

dropOverlapping :: [Point] -> [Point]
dropOverlapping (p1@(Point pos1 _) : ps@(Point pos2 _ : _))
  | pos1 == pos2  =      dropOverlapping ps
  | otherwise     = p1 : dropOverlapping ps
dropOverlapping ps = ps

showPicture :: [Point] -> String
showPicture unsortedPoints =
  let
    -- Iterate over y first, thus swap when sorting.
    points = dropOverlapping $ sortOn (swap . coords) unsortedPoints
    ((minX, minY), (maxX, maxY)) = boundingRectangle points
    shiftCoords (x, y) = (x - minX, y - minY)
    maxX' = maxX - minX
    maxY' = maxY - minY
    fromOrigin = map (shiftCoords . coords) points

    showPoint ([], res) _ = ([], '.' : res)
    showPoint (ps @ ((!pX, !pY) : ps'), res) (x, y)
      | pX == x && pY == y = (ps', '#' : res)
      | otherwise          = (ps, '.' : res)

  in intercalate "\n"
     $ chunksOf (maxX' + 1)
     $ reverse
     $ snd
     $ foldl' showPoint (fromOrigin, "")
     -- Iterate over y coordinate first, but still order then as (x, y)
     $ map swap
     $ range ((0, 0), (maxY', maxX'))

main :: IO ()
main = do
  input <- getContents
  case parse inputParser "<input>" input of
    Left errorText -> print errorText
    Right points -> do
      let maxIterations = 40000
          series = iterate moveOneSecond points
          bestSecond, bestArea :: Int
          (bestSecond, bestLayout, bestArea) =
            minimumBy (comparing thd3)
            $ take maxIterations
            $ zip3 [0..] series
            $ map (rectangleArea . boundingRectangle) series

      print bestSecond
      print bestArea
      print $ boundingRectangle bestLayout
      putStrLn $ showPicture bestLayout
