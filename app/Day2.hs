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
  length safeReports
  where
    reports = map parseReport (lines contents)
    safeReports = filter isReportSafe reports

removeLevel :: Report -> Int -> Report
removeLevel (Report levels) index =
  Report (before ++ after)
  where
    before = take index levels
    after = drop (index + 1) levels

-- The 'Problem Dampener' can remove a bad level from a report, making it safe
isReportSafeWithProblemDampener :: Report -> Bool
isReportSafeWithProblemDampener (Report levels) =
  any isReportSafe (Report levels : moddedReports)
  where
    indices = [0 .. (length levels - 1)]
    moddedReports = map (removeLevel (Report levels)) indices

part2 :: String -> Int
part2 contents =
  length safeReports
  where
    reports = map parseReport (lines contents)
    safeReports = filter isReportSafeWithProblemDampener reports
