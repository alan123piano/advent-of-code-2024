module Day13 (part1, part2) where

import Common.Grid (Coord (Coord))
import Common.List (splitList)
import qualified Data.List as List
import qualified Data.Maybe as Maybe

data ClawMachine = ClawMachine
  { cmButtonA :: Coord,
    cmButtonB :: Coord,
    cmPrize :: Coord
  }

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

correctClawMachine :: ClawMachine -> ClawMachine
correctClawMachine ClawMachine {cmButtonA = ba, cmButtonB = bb, cmPrize = Coord (p1, p2)} =
  ClawMachine {cmButtonA = ba, cmButtonB = bb, cmPrize = Coord (p1 + offset, p2 + offset)}
  where
    offset = 10000000000000 :: Int

getCost :: Int -> Int -> Int
getCost x1 x2 = 3 * x1 + x2

solveClawMachine :: ClawMachine -> Maybe Int
solveClawMachine ClawMachine {cmButtonA = Coord (a11, a21), cmButtonB = Coord (a12, a22), cmPrize = Coord (y1, y2)} =
  if isValid then Just (getCost x1' x2') else Nothing
  where
    det = a11 * a22 - a12 * a21
    idet = 1 / fromIntegral det :: Rational
    ai11 = idet * fromIntegral a22
    ai21 = -(idet * fromIntegral a21)
    ai12 = -(idet * fromIntegral a12)
    ai22 = idet * fromIntegral a11
    x1 = ai11 * fromIntegral y1 + ai12 * fromIntegral y2
    x2 = ai21 * fromIntegral y1 + ai22 * fromIntegral y2
    x1' = floor x1 :: Int
    x2' = floor x2 :: Int
    isValid = x1 == fromIntegral x1' && x2 == fromIntegral x2'

part1 :: String -> Int
part1 contents =
  sum $ map (Maybe.fromMaybe 0) results
  where
    cms = parseClawMachines contents
    results = map solveClawMachine cms

part2 :: String -> Int
part2 contents =
  sum $ map (Maybe.fromMaybe 0) results
  where
    cms = parseClawMachines contents
    cms' = map correctClawMachine cms
    results = map solveClawMachine cms'
