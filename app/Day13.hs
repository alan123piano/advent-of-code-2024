module Day13 (part1, part2) where

import Common.Grid (Coord (Coord), addCoord, scaleCoord)
import Common.List (splitList)
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Maybe as Maybe

data ClawMachine = ClawMachine
  { cmButtonA :: Coord,
    cmButtonB :: Coord,
    cmPrize :: Coord
  }

newtype ClawMachineInput = ClawMachineInput (Int, Int)

data ClawMachineResult
  = Prize ClawMachineInput
  | NoPrize

parseClawMachines :: String -> [ClawMachine]
parseClawMachines s =
  map (parseClawMachine . lines) ("\n\n" `splitList` s)
  where
    parseClawMachine :: [String] -> ClawMachine
    parseClawMachine ls =
      case ls of
        [l1, l2, l3] -> ClawMachine {cmButtonA = a, cmButtonB = b, cmPrize = prize}
          where
            a = readCoord "+" $ Maybe.fromJust ("Button A: " `List.stripPrefix` l1)
            b = readCoord "+" $ Maybe.fromJust ("Button B: " `List.stripPrefix` l2)
            prize = readCoord "=" $ Maybe.fromJust ("Prize: " `List.stripPrefix` l3)

            readCoord :: String -> String -> Coord
            readCoord op suffix =
              Coord (n1, n2)
              where
                (s1, s2) = case ", " `splitList` suffix of
                  [s1', s2'] -> (s1', s2')
                  _ -> error "unexpected"
                n1 = read $ Maybe.fromJust (("X" ++ op) `List.stripPrefix` s1) :: Int
                n2 = read $ Maybe.fromJust (("Y" ++ op) `List.stripPrefix` s2) :: Int
        _ -> error "unexpected"

getCost :: ClawMachineInput -> Int
getCost (ClawMachineInput (a, b)) = a * 3 + b

solveClawMachine :: ClawMachine -> ClawMachineResult
solveClawMachine ClawMachine {cmButtonA = ba, cmButtonB = bb, cmPrize = prize} =
  maybe NoPrize Prize bestPress
  where
    presses = [ClawMachineInput (a, b) | a <- [0 .. 100], b <- [0 .. 100]]
    presses' = filter (\(ClawMachineInput (a, b)) -> addCoord (scaleCoord a ba) (scaleCoord b bb) == prize) presses
    bestPress = case presses' of
      [] -> Nothing
      _ -> Just $ List.minimumBy (compare `on` getCost) presses'

part1 :: String -> Int
part1 contents =
  sum $ map getCost inputs
  where
    cms = parseClawMachines contents
    results = map solveClawMachine cms
    inputs =
      concatMap
        ( \case
            Prize i -> [i]
            NoPrize -> []
        )
        results

part2 :: String -> Int
part2 contents =
  undefined
