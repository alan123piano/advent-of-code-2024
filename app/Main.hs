module Main (main) where

import Data.Maybe (fromMaybe)
import qualified Day1
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
            (_, _) -> error $ "Unhandled (day, part): " ++ show (day, part)
        )
  print solution
