module Day6 (part1, part2) where

import Common.Grid (Coord (Coord), Grid, addCoord, allCoordsInGrid, atCoord, parseGrid, scaleCoord, setCoord)
import Data.Set (Set)
import qualified Data.Set as Set

data Cell = Empty | Obstruction

data Path = FinitePath [Coord] | Cycle

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
    path = getPath labMap guardCoord (Coord (-1, 0))
    visitedCoords = case path of
      FinitePath coords -> Set.fromList coords
      Cycle -> error "unexpected cycle"

getPath :: Grid Cell -> Coord -> Coord -> Path
getPath labMap startPos =
  reducer Set.empty [startPos] startPos
  where
    reducer :: Set (Coord, Coord) -> [Coord] -> Coord -> Coord -> Path
    reducer visited acc currPos currDir =
      if Set.member (currPos, currDir) visited
        then Cycle
        else case nextCell of
          Just Empty -> reducer visited' (nextPos : acc) nextPos currDir
          Just Obstruction -> reducer visited' acc currPos (rotate90Degrees currDir)
          Nothing -> FinitePath acc
      where
        visited' = Set.insert (currPos, currDir) visited
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
  length $
    filter doesGuardGetStuck $
      map (addObstruction labMap) (Set.toList possibleSpots)
  where
    (labMap, guardCoord) = parseLabMap contents

    path = getPath labMap guardCoord (Coord (-1, 0))
    visitedCoords = case path of
      FinitePath coords -> Set.fromList coords
      Cycle -> error "unexpected cycle"

    -- possible spots to place obstruction
    possibleSpots :: Set Coord
    possibleSpots = Set.delete guardCoord visitedCoords

    addObstruction :: Grid Cell -> Coord -> Grid Cell
    addObstruction grid coord =
      setCoord coord Obstruction grid

    doesGuardGetStuck :: Grid Cell -> Bool
    doesGuardGetStuck grid =
      case getPath grid guardCoord (Coord (-1, 0)) of
        FinitePath _ -> False
        Cycle -> True
