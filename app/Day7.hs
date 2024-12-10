module Day7 (part1, part2) where

import Common.List (splitList)

newtype Calibration = Calibration (Int, [Int])

parseCalibrations :: String -> [Calibration]
parseCalibrations s =
  map parseCalibration (lines s)
  where
    parseCalibration line =
      case splitList ": " line of
        [s1, s2] -> Calibration (read s1, map read (splitList " " s2))
        _ -> error line

isSolvable :: [Int -> Int -> Int] -> Calibration -> Bool
isSolvable _ (Calibration (_, [])) = error "empty list"
isSolvable ops (Calibration (lhs, a : rhs)) =
  rec rhs a
  where
    rec :: [Int] -> Int -> Bool
    rec [] acc = acc == lhs
    rec (hd : tl) acc =
      any (\op -> rec tl (op acc hd)) ops

part1 :: String -> Int
part1 contents =
  sum $ map (\(Calibration (n, _)) -> n) solvableCals
  where
    cals = parseCalibrations contents
    solvableCals = filter (isSolvable [(+), (*)]) cals

concatInts :: Int -> Int -> Int
concatInts a b =
  read $ show a ++ show b

part2 :: String -> Int
part2 contents =
  sum $ map (\(Calibration (n, _)) -> n) solvableCals
  where
    cals = parseCalibrations contents
    solvableCals = filter (isSolvable [(+), (*), concatInts]) cals
