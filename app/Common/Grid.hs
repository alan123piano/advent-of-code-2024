module Common.Grid
  ( Grid,
    Coord (Coord),
    parseGrid,
    gridToList,
    getGridSize,
    allCoordsInGrid,
    atCoord,
    setCoord,
    addCoord,
    subCoord,
    scaleCoord,
    negCoord,
  )
where

import Data.Map (Map)
import qualified Data.Map as Map

-- A grid maps 2D coords to values and is zero-indexed.
data Grid a = Grid
  { gValues :: Map Coord a,
    gNRows :: Int,
    gNCols :: Int
  }
  deriving (Show)

newtype Coord = Coord (Int, Int) deriving (Eq, Ord, Show)

instance Functor Grid where
  fmap :: (a -> b) -> Grid a -> Grid b
  fmap f g =
    Grid
      { gValues = fmap f (gValues g),
        gNRows = gNRows g,
        gNCols = gNCols g
      }

parseGrid :: String -> Grid Char
parseGrid s =
  Grid
    { gValues = values,
      gNRows = nRows,
      gNCols = nCols
    }
  where
    ls = lines s
    nRows = length ls
    nCols = length (head ls)
    values =
      foldr
        ( \(r, line) m ->
            foldr
              (\(c, chr) -> Map.insert (Coord (r, c)) chr)
              m
              (zip [0 ..] line)
        )
        (Map.empty :: Map Coord Char)
        (zip [0 ..] ls)

gridToList :: Grid a -> [(Coord, a)]
gridToList grid = Map.toList (gValues grid)

getGridSize :: Grid a -> (Int, Int)
getGridSize grid = (gNRows grid, gNCols grid)

allCoordsInGrid :: Grid a -> [Coord]
allCoordsInGrid grid =
  [Coord (r, c) | r <- [0 .. (nRows - 1)], c <- [0 .. (nCols - 1)]]
  where
    nRows = gNRows grid
    nCols = gNCols grid

atCoord :: Coord -> Grid a -> Maybe a
atCoord coord (Grid {gValues = values}) =
  Map.lookup coord values

setCoord :: Coord -> a -> Grid a -> Grid a
setCoord coord v grid =
  Grid
    { gValues = Map.insert coord v (gValues grid),
      gNRows = gNRows grid,
      gNCols = gNCols grid
    }

addCoord :: Coord -> Coord -> Coord
addCoord (Coord (r1, c1)) (Coord (r2, c2)) = Coord (r1 + r2, c1 + c2)

subCoord :: Coord -> Coord -> Coord
subCoord (Coord (r1, c1)) (Coord (r2, c2)) = Coord (r1 - r2, c1 - c2)

scaleCoord :: Int -> Coord -> Coord
scaleCoord mul (Coord (r, c)) = Coord (r * mul, c * mul)

negCoord :: Coord -> Coord
negCoord = scaleCoord (-1)
