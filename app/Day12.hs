module Day12 (part1, part2) where

import Common.Grid (Coord (Coord), Grid, addCoord, allCoordsInGrid, atCoord, parseGrid)
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set

-- A horizontal edge contains ((rowInner, rowOuter), column).
-- A vertical edge contains ((colInner, colOuter), row).
data Edge = Horizontal ((Int, Int), Int) | Vertical ((Int, Int), Int) deriving (Eq, Ord, Show)

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
  length $ getEdges plot

getEdges :: Set Coord -> [Edge]
getEdges plot =
  concatMap getE $ Set.toList plot
  where
    getE :: Coord -> [Edge]
    getE coord =
      map (makeEdge coord) edgeAdjs
      where
        adjs = map (addCoord coord) adjDirections
        edgeAdjs = filter (\adj -> not (Set.member adj plot)) adjs

makeEdge :: Coord -> Coord -> Edge
makeEdge (Coord (r1, c1)) (Coord (r2, c2))
  | r1 == r2 = Vertical ((c1, c2), r1)
  | c1 == c2 = Horizontal ((r1, r2), c1)
  | otherwise = error "unexpected"

getSides :: Set Coord -> Int
getSides plot =
  numHEdges + numVEdges
  where
    edges = getEdges plot
    (hEdges, vEdges) =
      foldr
        ( \edge (hAcc, vAcc) -> case edge of
            Horizontal ((r1, r2), c) -> (((r1, r2), c) : hAcc, vAcc)
            Vertical ((c1, c2), r) -> (hAcc, ((c1, c2), r) : vAcc)
        )
        ([], [])
        edges
    hEdges' = List.sort hEdges
    vEdges' = List.sort vEdges
    countSides :: ((Int, Int), Int) -> (Maybe ((Int, Int), Int), Int) -> (Maybe ((Int, Int), Int), Int)
    countSides ((r1, r2), c) (prev, numSides) = (Just ((r1, r2), c), numSides')
      where
        isNewEdge = case prev of
          Just ((r1', r2'), c') -> (r1, r2) /= (r1', r2') || abs (c - c') > 1
          Nothing -> True
        numSides' = if isNewEdge then numSides + 1 else numSides
    numHEdges = snd $ foldr countSides (Nothing, 0) hEdges'
    numVEdges = snd $ foldr countSides (Nothing, 0) vEdges'

part1 :: String -> Int
part1 contents =
  sum $ map (\p -> getArea p * getPerimeter p) plots
  where
    grid = parseGrid contents
    plots = getPlots grid

part2 :: String -> Int
part2 contents =
  sum $ map (\p -> getArea p * getSides p) plots
  where
    grid = parseGrid contents
    plots = getPlots grid
