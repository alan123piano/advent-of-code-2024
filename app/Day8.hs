module Day8 (part1, part2) where

import Common.Grid (Coord (Coord), addCoord, getGridSize, gridToList, parseGrid, scaleCoord, subCoord)
import Data.List (groupBy, sortBy)
import qualified Data.Set as Set

newtype Antenna = Antenna (Coord, Char) deriving (Eq, Show)

parseAntennae :: String -> ([Antenna], Int, Int)
parseAntennae s =
  (antennae, nrows, ncols)
  where
    grid = parseGrid s
    antennae = map Antenna $ filter (\(_, chr) -> chr /= '.') $ gridToList grid
    (nrows, ncols) = getGridSize grid

inBounds :: Int -> Int -> Coord -> Bool
inBounds nrows ncols (Coord (r, c)) =
  r >= 0 && c >= 0 && r < nrows && c < ncols

getAntinodes1 :: Coord -> Coord -> [Coord]
getAntinodes1 c1 c2 =
  [addCoord c1 diff, subCoord c2 diff]
  where
    diff = subCoord c1 c2

-- Part 2: get antinodes with resonant harmonics
getAntinodes2 :: Int -> Int -> Coord -> Coord -> [Coord]
getAntinodes2 nrows ncols c1 c2 =
  takeWhile inb genA ++ takeWhile inb genB
  where
    diff = subCoord c1 c2
    genA = map (\i -> addCoord c1 (scaleCoord i diff)) [0 ..]
    genB = map (\i -> subCoord c2 (scaleCoord i diff)) [0 ..]
    inb = inBounds nrows ncols

getAntFreq :: Antenna -> Char
getAntFreq (Antenna (_, chr)) = chr

antennaeCompatible :: Antenna -> Antenna -> Bool
antennaeCompatible a1 a2 = getAntFreq a1 == getAntFreq a2

numLocsWithAntinode :: (Coord -> Coord -> [Coord]) -> [Antenna] -> Int -> Int -> Int
numLocsWithAntinode getAntinodes antennae nrows ncols =
  length locsWithAntinode
  where
    antByChar =
      groupBy antennaeCompatible $
        sortBy (\a1 a2 -> compare (getAntFreq a1) (getAntFreq a2)) antennae
    antinodes =
      concat $
        concatMap
          (\ants -> [getAntinodes a b | Antenna (a, _) <- ants, Antenna (b, _) <- ants, a /= b])
          antByChar
    locsWithAntinode =
      Set.fromList $
        filter (inBounds nrows ncols) antinodes

part1 :: String -> Int
part1 contents =
  numLocsWithAntinode getAntinodes1 antennae nrows ncols
  where
    (antennae, nrows, ncols) = parseAntennae contents

part2 :: String -> Int
part2 contents =
  numLocsWithAntinode (getAntinodes2 nrows ncols) antennae nrows ncols
  where
    (antennae, nrows, ncols) = parseAntennae contents
