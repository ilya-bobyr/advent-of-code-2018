{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Array.IArray (listArray, (!))
import Data.Array (Array)
import Data.List (sortOn)
import Data.Tuple (swap)
import qualified Data.Set as Set
import Text.Printf (printf, perror)


data BoardCell = Empty
               | Vertical
               | Horizontal
               | BottomRight
               | BottomLeft
               | Intersection
               deriving (Eq, Show)

data CartDirection = FacingUp
                   | FacingDown
                   | FacingLeft
                   | FacingRight
                   deriving (Eq, Show)

data CartNextTurn = WillTurnLeft
                  | WillGoStraight
                  | WillTurnRight
                  deriving (Eq, Show)

data Cart = Cart {
    cartPosition :: (Int, Int)
  , cartDirection :: CartDirection
  , cartNextTurn :: CartNextTurn
  }
          deriving (Eq, Show)


type Board = Array (Int, Int) BoardCell

parseInput :: [String] -> Either String (Board, [Cart])
parseInput input =
  let rows = length input
      columns = length $ head input

      go :: (Int, Int)
         -> ([BoardCell], [Cart])
         -> [String]
         -> Either String (Board, [Cart])
      go _ (cells, carts) [] =
        Right
        (listArray ((0, 0), (rows - 1, columns - 1)) (reverse cells), carts)
      go (!y, !x) (cells, carts) ((c:cs):ls)
        | c == ' '  = go (y, x+1) (Empty : cells, carts) (cs:ls)
        | c == '|'  = go (y, x+1) (Vertical : cells, carts) (cs:ls)
        | c == '-'  = go (y, x+1) (Horizontal : cells, carts) (cs:ls)
        | c == '/'  = go (y, x+1) (BottomRight : cells, carts) (cs:ls)
        | c == '\\' = go (y, x+1) (BottomLeft : cells, carts) (cs:ls)
        | c == '+'  = go (y, x+1) (Intersection : cells, carts) (cs:ls)
        | c == '^'  = go (y, x+1) (Vertical : cells
                                  , Cart (y, x) FacingUp WillTurnLeft
                                    : carts) (cs:ls)
        | c == 'v'  = go (y, x+1) (Vertical : cells
                                  , Cart (y, x) FacingDown WillTurnLeft
                                    : carts) (cs:ls)
        | c == '>'  = go (y, x+1) (Horizontal : cells
                                  , Cart (y, x) FacingRight WillTurnLeft
                                    : carts) (cs:ls)
        | c == '<'  = go (y, x+1) (Horizontal : cells
                                  , Cart (y, x) FacingLeft WillTurnLeft
                                    : carts) (cs:ls)
        | otherwise = Left
          $ printf "Unexpected character '%s' at (%d, %d)" c x y

      go (!y, !x) (cells, carts) ([]:ls)
        | x /= columns = Left
          $ printf ("Unexpected row length in line %d\n"
                    ++ "Expected: %d, actual: %d") y columns x
        | otherwise =
          go (y+1, 0) (cells, carts) ls

  in go (0, 0) ([], []) input

cartNextState :: Board -> Cart -> Cart
cartNextState board (Cart (y, x) dir nextTurn) =
  let nextPos
        | dir == FacingUp = (y - 1, x)
        | dir == FacingDown = (y + 1, x)
        | dir == FacingLeft = (y, x - 1)
        | dir == FacingRight = (y, x + 1)
        | otherwise = perror "Unexpected direction: %d" $ show dir
      nextBoardCell = board ! nextPos

      facingOnIntersection
        | nextTurn == WillTurnLeft && dir == FacingUp = FacingLeft
        | nextTurn == WillTurnLeft && dir == FacingDown = FacingRight
        | nextTurn == WillTurnLeft && dir == FacingLeft = FacingDown
        | nextTurn == WillTurnLeft && dir == FacingRight = FacingUp
        | nextTurn == WillGoStraight = dir
        | nextTurn == WillTurnRight && dir == FacingUp = FacingRight
        | nextTurn == WillTurnRight && dir == FacingDown = FacingLeft
        | nextTurn == WillTurnRight && dir == FacingLeft = FacingUp
        | nextTurn == WillTurnRight && dir == FacingRight = FacingDown
        | otherwise = perror "Unexpected nextTurn: %d" $ show nextTurn

      nextTurnOnIntersection
        | nextTurn == WillTurnLeft = WillGoStraight
        | nextTurn == WillGoStraight = WillTurnRight
        | nextTurn == WillTurnRight = WillTurnLeft
        | otherwise = perror "Unexpected nextTurn: %d" $ show nextTurn

  in case nextBoardCell of
    Empty -> perror ("Cart ran off the track :(\n"
                     ++ "Pos: (%d, %d)") x y
    Vertical -> Cart nextPos dir nextTurn
    Horizontal -> Cart nextPos dir nextTurn
    BottomRight -> case dir of
                     FacingUp -> Cart nextPos FacingRight nextTurn
                     FacingDown -> Cart nextPos FacingLeft nextTurn
                     FacingLeft -> Cart nextPos FacingDown nextTurn
                     FacingRight -> Cart nextPos FacingUp nextTurn
    BottomLeft -> case dir of
                     FacingUp -> Cart nextPos FacingLeft nextTurn
                     FacingDown -> Cart nextPos FacingRight nextTurn
                     FacingLeft -> Cart nextPos FacingUp nextTurn
                     FacingRight -> Cart nextPos FacingDown nextTurn
    Intersection -> Cart nextPos facingOnIntersection nextTurnOnIntersection

oneTick :: Board -> [Cart] -> (Maybe (Int, Int), [Cart])
oneTick board carts =
  let initiallyOccupied = Set.fromList $ map cartPosition carts

      go (cart:oldCarts) occupied (firstCrash, newCarts) =
        let cart' = cartNextState board cart
            occupied' = cartPosition cart `Set.delete` occupied
            occupied'' = pos' `Set.insert` occupied'
            pos' = cartPosition cart'
            oldCartsNoCrash = filter ((/= pos') . cartPosition) oldCarts
            newCartsNoCrash = filter ((/= pos') . cartPosition) newCarts

        in if pos' `Set.member` occupied'
           then case firstCrash of
                  Nothing ->
                    go oldCartsNoCrash occupied' (Just pos', newCartsNoCrash)
                  _ ->
                    go oldCartsNoCrash occupied' (firstCrash, newCartsNoCrash)
           else go oldCarts occupied'' (firstCrash, cart':newCarts)

      go [] _ (firstCrash, newCarts) =
        (firstCrash, sortOn cartPosition newCarts)

  in go carts initiallyOccupied (Nothing, [])

findFirstCrash :: Board -> [Cart] -> (Int, Int)
findFirstCrash board carts =
  case oneTick board carts of
    (Just pos, _) -> pos
    (Nothing, carts') -> findFirstCrash board carts'

findLastSurvivor :: Board -> [Cart] -> Maybe (Int, Int)
findLastSurvivor board carts
  | null carts = Nothing
  | length carts == 1 = Just $ cartPosition $ head carts
  | otherwise = case oneTick board carts of
      (_, carts') -> findLastSurvivor board carts'

main :: IO ()
main = do
  input <- filter (not . null) . lines <$> getContents
  case parseInput input of
    Left errorText -> putStrLn errorText
    Right (board, carts) -> do
      uncurry (printf "%d,%d\n") $ swap
        $ findFirstCrash board carts

      case findLastSurvivor board carts of
        Nothing -> print "All carts crashed into each other :("
        Just (y, x) -> printf "Last survivor: %d,%d\n" x y
