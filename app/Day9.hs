module Day9 (part1, part2) where

newtype DiskMap = DiskMap [Int]

parseDiskMap :: String -> DiskMap
parseDiskMap s =
  DiskMap (map (read . (: [])) s)

part1 :: String -> Int
part1 contents =
  undefined

part2 :: String -> Int
part2 contents =
  undefined
