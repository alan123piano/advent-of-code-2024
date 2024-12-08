module Day4 (part1, part2) where

import Data.Map (Map)
import qualified Data.Map as Map

data WordSearch = WordSearch
  { wsGrid :: Map (Int, Int) Char, -- map of (r, c) to char, 1-indexed
    wsNRows :: Int,
    wsNCols :: Int
  }
  deriving (Show)

parseWordSearch :: String -> WordSearch
parseWordSearch s =
  WordSearch
    { wsGrid = grid,
      wsNRows = nRows,
      wsNCols = nCols
    }
  where
    ls = lines s
    nRows = length ls
    nCols = length (head ls)
    grid =
      foldr
        ( \(r, line) m ->
            foldr
              (\(c, chr) -> Map.insert (r, c) chr)
              m
              (zip [1 ..] line)
        )
        (Map.empty :: Map (Int, Int) Char)
        (zip [1 ..] ls)

-- returns whether `word` is in the word search starting from (r, c) and going in direction (dr, dc)
findWord :: WordSearch -> String -> (Int, Int) -> (Int, Int) -> Bool
findWord ws word (dr, dc) =
  reducer word
  where
    reducer (chr : word') (r, c) = case Map.lookup (r, c) (wsGrid ws) of
      Just chr' | chr == chr' -> reducer word' (r + dr, c + dc)
      _ -> False
    reducer [] _ = True

-- all eight directions of movement on a grid
allDirections :: [(Int, Int)]
allDirections = filter (\(dr, dc) -> dr /= 0 || dc /= 0) [(dr, dc) | dr <- [-1 .. 1], dc <- [-1 .. 1]]

allPoints :: WordSearch -> [(Int, Int)]
allPoints ws = [(r, c) | r <- [1 .. wsNRows ws], c <- [1 .. wsNCols ws]]

part1 :: String -> Int
part1 contents =
  length $ filter (uncurry (findWord ws "XMAS")) allSearches
  where
    ws = parseWordSearch contents
    allSearches = [(dpos, pos) | pos <- allPoints ws, dpos <- allDirections]

part2 :: String -> Int
part2 contents =
  undefined
