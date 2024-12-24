module Main (main) where

import Data.Maybe (fromMaybe)
import qualified Day1
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import System.Environment (lookupEnv)

main :: IO ()
main = do
  mDay <- lookupEnv "DAY"
  mPart <- lookupEnv "PART"
  mInput <- lookupEnv "INPUT"

  let day = fromMaybe 1 (fmap read mDay :: Maybe Int)
  let part = fromMaybe 1 (fmap read mPart :: Maybe Int)
  let input = fromMaybe "example.txt" mInput

  contents <- readFile ("input" ++ "/" ++ show day ++ "/" ++ input)
  let runner =
        ( case (day, part) of
            (1, 1) -> Day1.part1
            (1, 2) -> Day1.part2
            (2, 1) -> Day2.part1
            (2, 2) -> Day2.part2
            (3, 1) -> Day3.part1
            (3, 2) -> Day3.part2
            (4, 1) -> Day4.part1
            (4, 2) -> Day4.part2
            (5, 1) -> Day5.part1
            (5, 2) -> Day5.part2
            (6, 1) -> Day6.part1
            (6, 2) -> Day6.part2
            (7, 1) -> Day7.part1
            (7, 2) -> Day7.part2
            (8, 1) -> Day8.part1
            (8, 2) -> Day8.part2
            (9, 1) -> Day9.part1
            (9, 2) -> Day9.part2
            (10, 1) -> Day10.part1
            (10, 2) -> Day10.part2
            (11, 1) -> Day11.part1
            (11, 2) -> Day11.part2
            (12, 1) -> Day12.part1
            (12, 2) -> Day12.part2
            (13, 1) -> Day13.part1
            (13, 2) -> Day13.part2
            (_, _) -> error $ "Unhandled (day, part): " ++ show (day, part)
        )
  let solution = runner contents
  print solution
