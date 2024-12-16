module Day11 (part1, part2) where

parseStones :: String -> [Int]
parseStones s =
  map read $ words s

blinkOnce :: [Int] -> [Int]
blinkOnce =
  concatMap changeStone
  where
    changeStone :: Int -> [Int]
    changeStone 0 = [1]
    changeStone n
      | even (length (show n)) =
          [read n1, read n2]
      where
        nstr = show n
        len = length nstr
        hlen = len `div` 2
        (n1, n2) = splitAt hlen nstr
    changeStone n = [n * 2024]

part1 :: String -> Int
part1 contents =
  length stones'
  where
    stones = parseStones contents
    stones' = last $ take 26 $ iterate blinkOnce stones

part2 :: String -> Int
part2 contents =
  undefined
