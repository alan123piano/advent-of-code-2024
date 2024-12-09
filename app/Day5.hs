module Day5 (part1, part2) where

import Common (splitList, swapIndicesInList)
import Control.Applicative (asum)
import Data.Maybe (isNothing)

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
isCompliantWithRules rules update =
  isNothing $ asum $ map (findViolation update) rules

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

-- fix an update so that it isn't violating any ordering rules
fixUpdate :: [OrderingRule] -> Update -> Update
fixUpdate rules (Update pages) =
  -- while any rules are being violated, swap pages
  case violation of
    Just (i1, i2) -> fixUpdate rules $ Update (swapIndicesInList pages i1 i2)
    Nothing -> Update pages
  where
    violation = asum $ map (findViolation (Update pages)) rules

-- if violation exists, return corresponding indices in update
findViolation :: Update -> OrderingRule -> Maybe (Int, Int)
findViolation (Update pages) (OrderingRule (p1, p2)) =
  asum $ map check pairs
  where
    -- check if the provided pair has a rule violation, and return indices if so
    check :: ((Int, Int), (Int, Int)) -> Maybe (Int, Int)
    check ((p, i), (p', i'))
      | (p, p') == (p2, p1) =
          Just (i, i')
    check _ = Nothing

    -- all pairs of (P, P') where P' succeeds P
    pairs :: [((Int, Int), (Int, Int))]
    pairs =
      [ ((p, i), (p', i'))
        | (p, i) <- indexedPages,
          (p', i') <- drop (i + 1) indexedPages
      ]
      where
        indexedPages :: [(Int, Int)]
        indexedPages = zip pages [0 ..]

part2 :: String -> Int
part2 contents =
  sum middlePages
  where
    (rules, updates) = parseInput contents
    badUpdates = filter (not . isCompliantWithRules rules) updates
    fixedUpdates = map (fixUpdate rules) badUpdates
    middlePages = map getMiddlePage fixedUpdates
