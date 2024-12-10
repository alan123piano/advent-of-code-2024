module Day8 (part1, part2) where

import Common.Grid (Coord (Coord), addCoord, getGridSize, gridToList, parseGrid, subCoord)
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

getAntinodes :: Coord -> Coord -> [Coord]
getAntinodes c1 c2 =
  [addCoord c1 diff, subCoord c2 diff]
  where
    diff = subCoord c1 c2

getAntFreq :: Antenna -> Char
getAntFreq (Antenna (_, chr)) = chr

antennaeCompatible :: Antenna -> Antenna -> Bool
antennaeCompatible a1 a2 = getAntFreq a1 == getAntFreq a2

part1 :: String -> Int
part1 contents =
  length locsWithAntinode
  where
    (antennae, nrows, ncols) = parseAntennae contents
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
        filter (\(Coord (r, c)) -> r >= 0 && c >= 0 && r < nrows && c < ncols) antinodes

part2 :: String -> Int
part2 contents =
  undefined
