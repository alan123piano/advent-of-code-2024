module Main (main) where

import Data.Maybe (fromMaybe)
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
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
  let solution =
        ( case (day, part) of
            (1, 1) -> Day1.part1 contents
            (1, 2) -> Day1.part2 contents
            (2, 1) -> Day2.part1 contents
            (2, 2) -> Day2.part2 contents
            (3, 1) -> Day3.part1 contents
            (3, 2) -> Day3.part2 contents
            (4, 1) -> Day4.part1 contents
            (4, 2) -> Day4.part2 contents
            (5, 1) -> Day5.part1 contents
            (5, 2) -> Day5.part2 contents
            (_, _) -> error $ "Unhandled (day, part): " ++ show (day, part)
        )
  print solution
