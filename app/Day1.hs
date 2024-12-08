module Day1 (part1, part2) where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

parsePair :: String -> (Int, Int)
parsePair s =
  case words s of
    [a, b] -> (read a :: Int, read b :: Int)
    _ -> undefined

part1 :: String -> Int
part1 contents =
  sum diffs
  where
    pairs = map parsePair (lines contents)
    (xs, ys) = unzip pairs
    (xs', ys') = (List.sort xs, List.sort ys)
    diffs = map abs $ zipWith (-) xs' ys'

freqMap :: (Ord a) => [a] -> Map a Int
freqMap =
  foldr
    (\k -> Map.insertWith (+) k 1)
    Map.empty

part2 :: String -> Int
part2 contents =
  sum scores
  where
    pairs = map parsePair (lines contents)
    (xs, ys) = unzip pairs
    freqs = freqMap ys
    scores = map (\x -> x * Map.findWithDefault 0 x freqs) xs
