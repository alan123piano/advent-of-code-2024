module Day4 (part1, part2) where

import Common.Grid (Coord (Coord), Grid, addCoord, allCoordsInGrid, atCoord, negCoord, parseGrid, scaleCoord)

parseWordSearch :: String -> Grid Char
parseWordSearch = parseGrid

-- returns whether the provided "form" is in the word search at the given coord
findForm :: Grid Char -> [(Char, Coord)] -> Coord -> Bool
findForm ws forms coord =
  all findChar forms
  where
    findChar (chr, delta) = Just chr == atCoord (addCoord coord delta) ws

xmasForms :: [[(Char, Coord)]]
xmasForms =
  [[(chr, scaleCoord i delta) | (i, chr) <- zip [0 ..] "XMAS"] | delta <- allDirections]
  where
    -- all eight directions of movement on a grid
    allDirections :: [Coord]
    allDirections =
      map Coord $
        filter
          (\(dr, dc) -> dr /= 0 || dc /= 0)
          [(dr, dc) | dr <- [-1 .. 1], dc <- [-1 .. 1]]

crossMasForms :: [[(Char, Coord)]]
crossMasForms =
  [ [ ('A', Coord (0, 0)),
      ('M', addCoord (negCoord right) (negCoord down)),
      ('M', addCoord (negCoord right) down),
      ('S', addCoord right (negCoord down)),
      ('S', addCoord right down)
    ]
    | (right, down) <-
        [ (r, d),
          (d, l),
          (l, u),
          (u, r)
        ]
  ]
  where
    r = Coord (0, 1)
    d = Coord (1, 0)
    l = Coord (0, -1)
    u = Coord (-1, 0)

part1 :: String -> Int
part1 contents =
  length $ filter (uncurry (findForm ws)) allSearches
  where
    ws = parseWordSearch contents
    allSearches = [(form, coord) | form <- xmasForms, coord <- allCoordsInGrid ws]

part2 :: String -> Int
part2 contents =
  length $ filter (uncurry (findForm ws)) allSearches
  where
    ws = parseWordSearch contents
    allSearches = [(form, coord) | form <- crossMasForms, coord <- allCoordsInGrid ws]
