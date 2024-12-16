module Day11 (part1, part2) where

import Common.List (countFreq)
import Data.Map (Map)
import qualified Data.Map as Map

newtype Stones = Stones (Map Int Int)

parseStones :: String -> Stones
parseStones s =
  Stones $ countFreq $ map read $ words s

countStones :: Int -> Stones -> Int
countStones numBlinks (Stones m) =
  if numBlinks == 0
    then
      Map.foldr (+) 0 m
    else
      countStones
        (numBlinks - 1)
        $ Stones
        $ Map.foldrWithKey
          ( \k v m' ->
              foldr
                (\stone -> Map.insertWith (+) stone v)
                m'
                (nextStones k)
          )
          Map.empty
          m

nextStones :: Int -> [Int]
nextStones stone
  | stone == 0 =
      [1]
  | even len =
      [n1, n2]
  | otherwise =
      [stone * 2024]
  where
    nstr = show stone
    len = length nstr
    hlen = len `div` 2
    (n1str, n2str) = splitAt hlen nstr
    (n1, n2) = (read n1str, read n2str) :: (Int, Int)

part1 :: String -> Int
part1 contents =
  countStones 25 stones
  where
    stones = parseStones contents

part2 :: String -> Int
part2 contents =
  countStones 75 stones
  where
    stones = parseStones contents
