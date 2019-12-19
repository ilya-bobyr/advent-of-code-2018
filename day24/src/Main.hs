{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Arrow ((&&&))
import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Control.Applicative ((<|>))
import Data.Array.IArray (Array, listArray, (!))
import qualified Data.Array.IArray as IA
import Data.Array.ST (STArray)
import Data.Array.MArray (readArray, writeArray)
import qualified Data.Array.MArray as MA
import Data.Foldable (find)
import Data.List (elem, intercalate, iterate, notElem, sort, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe, maybe)
import Data.Ord (Down(Down), comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec (parse, try)
import Text.Parsec.Char (digit, char, letter, string, spaces, newline)
import Text.Parsec.Combinator (eof, many1, option, sepBy, sepEndBy)
import Text.Parsec.String (Parser)
import Text.Printf (printf)

type Weakness = [String]
type Immunity = [String]
type AttackType = String

data Group = Group {
  groupUnits :: Int
  , groupUnitHitPoints :: Int
  , groupUnitAttack :: Int
  , groupAttackType :: AttackType
  , groupInitiative :: Int
  , groupWeaknesses :: Weakness
  , groupImmunities :: Immunity
  }
  deriving (Show, Eq)

type Army = Array Int Group

data WeakOrImmune = Weak
                  | Immune
                  deriving Eq

data ArmyType = ImmuneSystem
              | Infection
              deriving Eq

weakParser :: Parser (WeakOrImmune, Weakness)
weakParser = do
  _ <- string "weak to "
  res <- many1 letter `sepBy` (spaces >> char ',' >> spaces)
  return (Weak, res)

immuneParser :: Parser (WeakOrImmune, Immunity)
immuneParser = do
  _ <- string "immune to "
  res <- many1 letter `sepBy` (spaces >> char ',' >> spaces)
  return (Immune, res)

weakImmuneParser :: Parser (Weakness, Immunity)
weakImmuneParser = do
  _ <- string " ("
  traits <- (weakParser <|> immuneParser) `sepBy` (char ';' >> spaces)
  let weakness = maybe [] snd
        $ find ((== Weak) . fst) traits
  let immunities = maybe [] snd
        $ find ((== Immune) . fst) traits
  _ <- char ')'
  return (weakness, immunities)

groupParser :: Parser Group
groupParser = do
  units <- read <$> many1 digit
  _ <- string " units each with "
  unitHitPoints <- read <$> many1 digit
  _ <- string " hit points"
  (weakensses, immunities) <- option ([], []) $ try weakImmuneParser
  _ <- string " with an attack that does "
  unitAttack <- read <$> many1 digit
  _ <- char ' '
  attackType <- many1 letter
  _ <- string " damage at initiative "
  initiative <- read <$> many1 digit
  return $ Group units unitHitPoints unitAttack attackType initiative
                 weakensses immunities

inputParser :: Parser (Army, Army)
inputParser = do
  _ <- string "Immune System:"
  _ <- newline
  immuneSystem <- groupParser `sepEndBy` newline
  _ <- newline
  _ <- string "Infection:"
  _ <- newline
  infection <- groupParser `sepEndBy` newline
  eof
  return ( listArray (0, length immuneSystem - 1) immuneSystem
         , listArray (0, length infection - 1) infection
         )

groupEffectivePower :: Group -> Int
groupEffectivePower g =
  groupUnits g * groupUnitAttack g

selectTarget :: Group -> [(Int, Group)] -> Set Int -> Maybe Int
selectTarget attacker targets taken =
  let attackType = groupAttackType attacker
      candidates = filter (\(i, _) -> i `Set.notMember` taken) targets
      weak =
        [(i, t) | (i, t) <- candidates
                , attackType `elem` groupWeaknesses t
                ]
      normal =
        [(i, t) | (i, t) <- candidates
                , attackType `notElem` groupWeaknesses t
                , attackType `notElem` groupImmunities t
                ]
  in listToMaybe $ map fst $ weak ++ normal

type AttackPlan = Map Int Int

selectTargets :: (Army, Army) -> (AttackPlan, AttackPlan)
selectTargets (army1, army2) =
  let buildPlan attackers defenders =
        let go ((aI, attacker) : as) taken =
              case selectTarget attacker defenders taken of
                Nothing -> go as taken
                Just dI -> let taken' = dI `Set.insert` taken
                           in (aI, dI) : go as taken'
            go [] _ = []
        in go attackers Set.empty

      army1ByPower =
        sortBy (comparing ((Down . groupEffectivePower . snd) &&&
                            (Down . groupInitiative . snd)
                          ))
        $ IA.assocs army1
      army2ByPower =
        sortBy (comparing ((Down . groupEffectivePower . snd) &&&
                            (Down . groupInitiative . snd)
                          ))
        $ IA.assocs army2

  in ( M.fromList $ buildPlan army1ByPower army2ByPower
     , M.fromList $ buildPlan army2ByPower army1ByPower
     )

data TurnOrder = Army1Attack Int
               | Army2Attack Int
               deriving Show

initiativeOrder :: (Army, Army) -> [TurnOrder]
initiativeOrder (army1, army2) =
  let wrap side (i, g) = (groupInitiative g, side i)
  in map snd
     $ sortBy (comparing (Down . fst))
     $ map (wrap Army1Attack) (IA.assocs army1)
        ++ map (wrap Army2Attack) (IA.assocs army2)

oneFight :: (Army, Army) -> (Army, Army)
oneFight (army1, army2) =
  let (army1Plan, army2Plan) = selectTargets (army1, army2)

      oneSkirmish :: Int
                  -> STArray s Int Group
                  -> Int
                  -> STArray s Int Group
                  -> ST s ()
      oneSkirmish aI attackers dI defenders = do
        aGroup <- readArray attackers aI
        dGroup <- readArray defenders dI
        let attackType = groupAttackType aGroup
            effectivePower = groupUnits aGroup * groupUnitAttack aGroup
            damage
              | attackType `elem` groupImmunities dGroup = 0
              | attackType `elem` groupWeaknesses dGroup = effectivePower * 2
              | otherwise = effectivePower
            deadCount = damage `quot` groupUnitHitPoints dGroup
            dUnits' = max 0 $ groupUnits dGroup - deadCount
            dGroup' = dGroup { groupUnits = dUnits' }
        writeArray defenders dI dGroup'

  in runST $ do
    marmy1 <- MA.thaw army1
    marmy2 <- MA.thaw army2

    forM_ (initiativeOrder (army1, army2)) $ \case
      (Army1Attack aI) -> case M.lookup aI army1Plan of
        Nothing -> return ()
        Just dI -> oneSkirmish aI marmy1 dI marmy2
      (Army2Attack aI) -> case M.lookup aI army2Plan of
        Nothing -> return ()
        Just dI -> oneSkirmish aI marmy2 dI marmy1

    larmy1' <- filter ((/= 0) . groupUnits) <$> MA.getElems marmy1
    larmy2' <- filter ((/= 0) . groupUnits) <$> MA.getElems marmy2

    let army1' = listArray (0, length larmy1' - 1) larmy1'
        army2' = listArray (0, length larmy2' - 1) larmy2'

    return (army1', army2')

data FightResult = LeftWins Int
                 | RightWins Int
                 | Incomplete
                 | Draw
                 deriving (Show, Eq)

armyUnits :: Army -> Int
armyUnits = sum . map groupUnits . IA.elems

leftWins :: (Army, Army) -> Maybe Int
leftWins (winner, looser)
  | IA.bounds looser == (0, -1) =
    Just $ armyUnits winner
leftWins _ = Nothing

fightResult :: (Army, Army) -> (Army, Army) -> FightResult
fightResult (army1, army2) (parmy1, parmy2)
  | IA.bounds army1 == (0, -1) = RightWins $ armyUnits army2
  | IA.bounds army2 == (0, -1) = LeftWins $ armyUnits army1
  | army1 == parmy1 && army2 == parmy2 =
    let army1Units = armyUnits army1
        army2Units = armyUnits army2
        res | army1Units > army2Units = LeftWins army1Units
            | army2Units > army1Units = RightWins army2Units
            | otherwise = Draw
    in res
  | otherwise = Incomplete

fightIsOver :: FightResult -> Bool
fightIsOver (LeftWins _) = True
fightIsOver (RightWins  _) = True
fightIsOver Incomplete = False
fightIsOver Draw = False

printArmies :: (Army, Army) -> IO ()
printArmies (army1, army2) = do
  let (army1Plan, army2Plan) = selectTargets (army1, army2)

      printGroup side g =
        printf "%s %d: n: %d, hp: %d, dmg: %d, dtype: %s, immune: [%s], weak: [%s], target: %s\n"
          side (groupInitiative g) (groupUnits g) (groupUnitHitPoints g)
          (groupUnitAttack g) (groupAttackType g)
          (intercalate "," $ sort $ groupImmunities g)
          (intercalate "," $ sort $ groupWeaknesses g)

  putStrLn ""
  forM_ (initiativeOrder (army1, army2)) $ \case
    (Army1Attack aI) ->
      let g = army1 ! aI
          target = case M.lookup aI army1Plan of
            Nothing -> "-"
            Just dI -> "Infection_" ++ (show . groupInitiative $ army2 ! dI)
      in printGroup "System" g target
    (Army2Attack aI) ->
      let g = army2 ! aI
          target = case M.lookup aI army2Plan of
            Nothing -> "-"
            Just dI -> "System_" ++ (show . groupInitiative $ army1 ! dI)
      in printGroup "Infection" g target

boost :: Army -> Int -> Army
boost army by =
  IA.amap (\g -> g { groupUnitAttack = groupUnitAttack g + by }) army

main :: IO ()
main = do
  input <- getContents
  case parse inputParser "<input>" input of
    Left errorText -> print errorText
    Right (immuneSystem, infection) -> do
      let firstFight = iterate oneFight (immuneSystem, infection)
          score = head
            $ dropWhile (not . fightIsOver)
            $ zipWith fightResult firstFight (tail firstFight)
      print score

      let leftBoostedScore =
            head
            $ dropWhile (\case LeftWins _ -> False
                               _ -> True)
            $ map ((\is ->
                      let fight = iterate oneFight (is, infection)
                      in head $ dropWhile (not . fightIsOver)
                         $ zipWith fightResult fight (tail fight))
                    . boost immuneSystem)
            [1..]
      print leftBoostedScore
