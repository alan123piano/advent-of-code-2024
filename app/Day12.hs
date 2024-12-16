module Day12 (part1, part2) where

import Common.Grid (Coord (Coord), Grid, addCoord, allCoordsInGrid, atCoord, parseGrid)
import Data.Set (Set)
import qualified Data.Set as Set

adjDirections :: [Coord]
adjDirections = [Coord (-1, 0), Coord (0, 1), Coord (1, 0), Coord (0, -1)]

getPlots :: Grid Char -> [Set Coord]
getPlots grid =
  snd $ foldr buildPlots (Set.empty, []) (allCoordsInGrid grid)
  where
    getPlot :: Set Coord -> [Coord] -> Char -> Set Coord
    getPlot acc q chr =
      case q of
        (coord : q') ->
          if Set.member coord acc || atCoord coord grid /= Just chr
            then getPlot acc q' chr
            else getPlot (Set.insert coord acc) (adjs ++ q') chr
          where
            adjs = map (addCoord coord) adjDirections
        [] -> acc

    buildPlots :: Coord -> (Set Coord, [Set Coord]) -> (Set Coord, [Set Coord])
    buildPlots coord (visited, plots) =
      if Set.member coord visited
        then
          (visited, plots)
        else case atCoord coord grid of
          Just chr -> (Set.union plot visited', plot : plots)
            where
              plot = getPlot Set.empty [coord] chr
          Nothing -> (visited', plots)
      where
        visited' = Set.insert coord visited

getArea :: Set Coord -> Int
getArea = length

getPerimeter :: Set Coord -> Int
getPerimeter plot =
  sum $ map getP $ Set.toList plot
  where
    -- get # of edges adjacent to cells not in plot
    getP :: Coord -> Int
    getP coord =
      length $ filter (\adj -> not (Set.member adj plot)) adjs
      where
        adjs = map (addCoord coord) adjDirections

part1 :: String -> Int
part1 contents =
  sum $ map (\p -> getArea p * getPerimeter p) plots
  where
    grid = parseGrid contents
    plots = getPlots grid

part2 :: String -> Int
part2 contents =
  undefined
