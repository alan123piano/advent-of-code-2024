module Day6 (part1, part2) where

import Common.Grid (Coord (Coord), Grid, addCoord, allCoordsInGrid, atCoord, parseGrid, scaleCoord)
import Data.Set (Set)
import qualified Data.Set as Set

data Cell = Empty | Obstruction

-- parse a lab map along with a starting position for the guard
parseLabMap :: String -> (Grid Cell, Coord)
parseLabMap s =
  (labMap, guardCoord)
  where
    grid = parseGrid s
    labMap = fmap chrToCell grid

    chrToCell :: Char -> Cell
    chrToCell chr =
      case chr of
        '.' -> Empty
        '^' -> Empty
        '#' -> Obstruction
        unknown -> error $ show unknown

    guardCoord :: Coord
    guardCoord = case guardCoords of
      [v] -> v
      -- we only expect one guard in the input
      _ -> error $ show guardCoords
      where
        guardCoords =
          filter
            (\coord -> atCoord coord grid == Just '^')
            (allCoordsInGrid grid)

part1 :: String -> Int
part1 contents =
  length visitedCoords
  where
    (labMap, guardCoord) = parseLabMap contents
    path = getPath [guardCoord] guardCoord (Coord (-1, 0))
    visitedCoords = Set.fromList path

    getPath :: [Coord] -> Coord -> Coord -> [Coord]
    getPath acc currPos currDir =
      case nextCell of
        Just Empty -> getPath (nextPos : acc) nextPos currDir
        Just Obstruction -> getPath acc currPos (rotate90Degrees currDir)
        Nothing -> acc
      where
        nextPos = addCoord currPos currDir
        nextCell = atCoord nextPos labMap

    rotate90Degrees :: Coord -> Coord
    rotate90Degrees (Coord (d1, d2)) =
      {-
      90 deg clockwise rotation matrix:
      [ 0  1]
      [-1  0]
      -}
      addCoord (scaleCoord d1 m1) (scaleCoord d2 m2)
      where
        m1 = Coord (0, -1)
        m2 = Coord (1, 0)

part2 :: String -> Int
part2 contents =
  undefined
