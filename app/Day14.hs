module Day14 (part1, part2) where

import Common.List (countFreq, splitList)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)

newtype RobotDesc = RobotDesc (Int, Int, Int, Int)

parseRobotDesc :: String -> RobotDesc
parseRobotDesc s =
  RobotDesc (p1, p2, v1, v2)
  where
    [sp1, sp2] = " " `splitList` s
    [ps1, ps2] = "," `splitList` Maybe.fromJust ("p=" `List.stripPrefix` sp1)
    [vs1, vs2] = "," `splitList` Maybe.fromJust ("v=" `List.stripPrefix` sp2)
    (p1, p2) = (read ps1, read ps2) :: (Int, Int)
    (v1, v2) = (read vs1, read vs2) :: (Int, Int)

getQuadrant :: Int -> Int -> (Int, Int) -> Maybe Int
getQuadrant w h (x1, x2)
  | x1 > wmid && x2 < hmid = Just 1
  | x1 < wmid && x2 < hmid = Just 2
  | x1 < wmid && x2 > hmid = Just 3
  | x1 > wmid && x2 > hmid = Just 4
  | otherwise = Nothing
  where
    wmid = w `div` 2
    hmid = h `div` 2

posAfter :: Int -> Int -> Int -> RobotDesc -> (Int, Int)
posAfter w h secs (RobotDesc (p1, p2, v1, v2)) =
  ((p1 + v1 * secs) `mod` (w + 1), (p2 + v2 * secs) `mod` (h + 1))

part1 :: String -> Int
part1 contents =
  product $ Map.elems quadrantCounts
  where
    robotDescs = map parseRobotDesc $ lines contents
    robotsAfter = map (posAfter w h 100) robotDescs
    quadrants =
      concatMap
        ( \p -> case getQuadrant w h p of
            Just q -> [q]
            Nothing -> []
        )
        robotsAfter
    quadrantCounts = countFreq quadrants
    w = maximum $ map (\(RobotDesc (p1, _, _, _)) -> p1) robotDescs
    h = maximum $ map (\(RobotDesc (_, p2, _, _)) -> p2) robotDescs

showPoses :: Int -> Int -> Set (Int, Int) -> String
showPoses w h poses =
  unlines $ map (\y -> map (\x -> if Set.member (x, y) poses then '#' else '.') [0 .. w]) [0 .. h]

part2 :: String -> Int
part2 contents =
  trace
    (showPoses w h (Set.fromList $ map (posAfter w h n') robotDescs))
    n'
  where
    robotDescs = map parseRobotDesc $ lines contents
    w = maximum $ map (\(RobotDesc (p1, _, _, _)) -> p1) robotDescs
    h = maximum $ map (\(RobotDesc (_, p2, _, _)) -> p2) robotDescs
    n' = head $ filter isChristmasTree [0 ..]

    isChristmasTree :: Int -> Bool
    isChristmasTree n =
      length poses == length robotsAfter
      where
        robotsAfter = map (posAfter w h n) robotDescs
        poses = Set.fromList robotsAfter
