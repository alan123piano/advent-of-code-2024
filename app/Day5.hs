module Day5 (part1, part2) where

import Common (splitList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

newtype OrderingRule = OrderingRule (Int, Int) deriving (Show)

newtype Update = Update [Int] deriving (Show)

parseOrderingRule :: String -> OrderingRule
parseOrderingRule s =
  case splitList "|" s of
    [s1, s2] -> OrderingRule (read s1, read s2)
    _ -> error s

parseUpdate :: String -> Update
parseUpdate s =
  Update (map (read :: String -> Int) splits)
  where
    splits = splitList "," s

parseInput :: String -> ([OrderingRule], [Update])
parseInput s =
  case sp of
    [a, b] -> (map parseOrderingRule a, map parseUpdate b)
    _ -> error $ show sp
  where
    sp = splitList [""] (lines s)

isCompliantWithRules :: [OrderingRule] -> Update -> Bool
isCompliantWithRules rules (Update pages) =
  not (any checkForViolation rules)
  where
    -- check if rule is being violated
    checkForViolation :: OrderingRule -> Bool
    checkForViolation (OrderingRule (a, b)) =
      case Map.lookup b succSet of
        Just s -> Set.member a s
        Nothing -> False

    -- maps each page number to set of pages succeeding it
    succSet :: Map Int (Set Int)
    succSet =
      Map.fromList
        ( zipWith
            (\page idx -> (page, Set.fromList (drop idx pages)))
            pages
            [1 ..]
        )

getMiddlePage :: Update -> Int
getMiddlePage (Update pages) =
  pages !! idx
  where
    idx = length pages `div` 2

part1 :: String -> Int
part1 contents =
  sum middlePages
  where
    (rules, updates) = parseInput contents
    goodUpdates = filter (isCompliantWithRules rules) updates
    middlePages = map getMiddlePage goodUpdates

part2 :: String -> Int
part2 contents =
  undefined
