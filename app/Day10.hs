module Day10 (part1, part2) where

import Common.Grid (Coord (Coord), Grid, addCoord, atCoord, gridToList, parseGrid)
import Data.Set (Set)
import qualified Data.Set as Set

adjDirections :: [Coord]
adjDirections = [Coord (-1, 0), Coord (0, 1), Coord (1, 0), Coord (0, -1)]

getTrailheadScore :: Grid Int -> Coord -> Int
getTrailheadScore heightMap startPos =
  length reachablePeaks
  where
    reachablePeaks = searchPeaks Set.empty startPos

    searchPeaks :: Set Coord -> Coord -> Set Coord
    searchPeaks visited currPos =
      case atCoord currPos heightMap of
        Just 9 -> Set.singleton currPos
        Just n -> Set.unions $ map (searchPeaks (Set.insert currPos visited)) neighbors
          where
            neighbors = filter isValidNeighbor $ map (addCoord currPos) adjDirections

            isValidNeighbor :: Coord -> Bool
            isValidNeighbor pos =
              not (Set.member pos visited) && heightIncreasesByOne
              where
                heightIncreasesByOne = case atCoord pos heightMap of
                  Just n' | n' == n + 1 -> True
                  _ -> False
        Nothing -> Set.empty

part1 :: String -> Int
part1 contents =
  sum $ map (getTrailheadScore heightMap) trailheads
  where
    grid = parseGrid contents
    heightMap = fmap ((read :: String -> Int) . (: [])) grid
    trailheads = map fst $ filter (\(_, height) -> height == 0) $ gridToList heightMap

part2 :: String -> Int
part2 contents =
  undefined
