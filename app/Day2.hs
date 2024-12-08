module Day2 (part1, part2) where

newtype Report = Report [Int]

parseReport :: String -> Report
parseReport s =
  Report levels
  where
    levels = map read (words s) :: [Int]

isReportSafe :: Report -> Bool
isReportSafe (Report levels) =
  allDiffsInBounds && (isAllIncreasing || isAllDecreasing)
  where
    neighbors = zip levels (drop 1 levels)
    diffs = map (\(a, b) -> abs (a - b)) neighbors
    isAllIncreasing = all (uncurry (<)) neighbors
    isAllDecreasing = all (uncurry (>)) neighbors
    allDiffsInBounds = all (\x -> 1 <= x && x <= 3) diffs

part1 :: String -> Int
part1 contents =
  length $ filter isReportSafe reports
  where
    reports = map parseReport (lines contents)

part2 :: String -> Int
part2 contents =
  undefined
