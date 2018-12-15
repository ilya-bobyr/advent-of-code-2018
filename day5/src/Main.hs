module Main where

import Control.Applicative ((<$>))
import Data.Char (isSpace, toUpper, toLower, isLower)
import Data.List (iterate')
import qualified Data.Set as Set

findStable :: Ord a => [a] -> a
findStable (v1:v2:_) | v1 == v2 = v1
findStable (_:rest) = findStable rest
findStable [] = error "List exhausted"

removeTriggered :: String -> String
removeTriggered str =
  let onePass [] = []
      onePass (c1:c2:rest)
        | (isLower c1 && toUpper c1 == c2)
          || (isLower c2 && c1 == toUpper c2) = onePass rest
      onePass (c:rest) = c : onePass rest

  in findStable $ iterate' onePass str

allUnitTypes :: String -> String
allUnitTypes str =
  Set.toList $ Set.fromList $ map toLower str

main :: IO ()
main = do
  input <- filter (not . isSpace) <$> getContents
  let solution1 = length $ removeTriggered input

  let unitTypes = allUnitTypes input
      allVariants = [filter ((/= c) . toLower) input | c <- unitTypes]
      solution2 = minimum $ map (length . removeTriggered) allVariants

  print solution1
  print solution2
