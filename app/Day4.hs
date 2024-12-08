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

-- returns whether the provided "form" is in the word search at (r, c)
findForm :: WordSearch -> [(Char, (Int, Int))] -> (Int, Int) -> Bool
findForm ws forms (r, c) =
  all findChar forms
  where
    findChar (chr, (dr, dc)) = Just chr == Map.lookup (r + dr, c + dc) (wsGrid ws)

xmasForms :: [[(Char, (Int, Int))]]
xmasForms =
  [[(chr, (dr * i, dc * i)) | (i, chr) <- zip [0 ..] "XMAS"] | (dr, dc) <- allDirections]
  where
    -- all eight directions of movement on a grid
    allDirections :: [(Int, Int)]
    allDirections =
      filter
        (\(dr, dc) -> dr /= 0 || dc /= 0)
        [(dr, dc) | dr <- [-1 .. 1], dc <- [-1 .. 1]]

addVec2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
addVec2 (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

scaleVec2 :: Int -> (Int, Int) -> (Int, Int)
scaleVec2 mul (r, c) = (r * mul, c * mul)

negVec2 :: (Int, Int) -> (Int, Int)
negVec2 = scaleVec2 (-1)

crossMasForms :: [[(Char, (Int, Int))]]
crossMasForms =
  [ [ ('A', (0, 0)),
      ('M', addVec2 (negVec2 right) (negVec2 down)),
      ('M', addVec2 (negVec2 right) down),
      ('S', addVec2 right (negVec2 down)),
      ('S', addVec2 right down)
    ]
    | (right, down) <-
        [ ((0, 1), (1, 0)),
          ((1, 0), (0, -1)),
          ((0, -1), (-1, 0)),
          ((-1, 0), (0, 1))
        ]
  ]

allPoints :: WordSearch -> [(Int, Int)]
allPoints ws = [(r, c) | r <- [1 .. wsNRows ws], c <- [1 .. wsNCols ws]]

part1 :: String -> Int
part1 contents =
  length $ filter (uncurry (findForm ws)) allSearches
  where
    ws = parseWordSearch contents
    allSearches = [(form, pos) | form <- xmasForms, pos <- allPoints ws]

part2 :: String -> Int
part2 contents =
  length $ filter (uncurry (findForm ws)) allSearches
  where
    ws = parseWordSearch contents
    allSearches = [(form, pos) | form <- crossMasForms, pos <- allPoints ws]
