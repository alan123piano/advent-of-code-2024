module Day15 (part1, part2) where

import Common.Grid (Coord (Coord), Grid, addCoord, allCoordsInGrid, atCoord, gridToList, parseGrid, setCoord)
import Common.List (splitList)

data Tile = Robot | Box | Wall | Empty deriving (Eq)

data Move = UpMove | DownMove | LeftMove | RightMove

-- (warehouse map, robot location)
newtype Warehouse = Warehouse (Grid Tile, Coord)

parseTile :: Char -> Tile
parseTile c =
  case c of
    '@' -> Robot
    'O' -> Box
    '#' -> Wall
    '.' -> Empty
    _ -> error (show c)

parseMove :: Char -> Move
parseMove c =
  case c of
    '^' -> UpMove
    'v' -> DownMove
    '<' -> LeftMove
    '>' -> RightMove
    _ -> error (show c)

parseWarehouse :: String -> (Warehouse, [Move])
parseWarehouse s =
  (Warehouse (grid, robotLoc), moves)
  where
    [s1, s2] = "\n\n" `splitList` s
    grid = parseTile <$> parseGrid s1
    moves = parseMove <$> concat (lines s2)
    robotLoc = head $ filter (\loc -> atCoord loc grid == Just Robot) (allCoordsInGrid grid)

applyMove :: Warehouse -> Move -> Warehouse
applyMove (Warehouse (grid, robotLoc)) move =
  case atCoord robotLoc' grid of
    Just Robot -> error "inconsistent grid state"
    Just Box -> case getPushTarget moveDelta robotLoc of
      Just boxLoc ->
        Warehouse (setCoord boxLoc Box $ moveRobot grid, robotLoc')
      Nothing -> noop
    Just Wall -> noop
    Just Empty ->
      Warehouse (moveRobot grid, robotLoc')
    Nothing -> noop
  where
    moveDelta = case move of
      UpMove -> Coord (-1, 0)
      DownMove -> Coord (1, 0)
      LeftMove -> Coord (0, -1)
      RightMove -> Coord (0, 1)
    robotLoc' = addCoord moveDelta robotLoc
    noop = Warehouse (grid, robotLoc)
    moveRobot = setCoord robotLoc Empty . setCoord robotLoc' Robot

    getPushTarget :: Coord -> Coord -> Maybe Coord
    getPushTarget dir boxLoc =
      case atCoord boxLoc' grid of
        Just Box -> getPushTarget dir boxLoc'
        Just Empty -> Just boxLoc'
        _ -> Nothing
      where
        boxLoc' = addCoord dir boxLoc

part1 :: String -> Int
part1 contents =
  sum $ map (\(Coord (r, c)) -> r * 100 + c) boxCoords
  where
    (wh, moves) = parseWarehouse contents
    Warehouse (finalGrid, _) = foldl applyMove wh moves
    boxCoords = map fst $ filter (\(_, tile) -> tile == Box) $ gridToList finalGrid

part2 :: String -> Int
part2 contents =
  undefined
