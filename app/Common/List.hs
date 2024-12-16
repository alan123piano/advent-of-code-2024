module Common.List
  ( splitList,
    sliceList,
    swapIndicesInList,
    countFreq,
  )
where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

splitList :: (Eq a) => [a] -> [a] -> [[a]]
splitList delim =
  reducer [] []
  where
    reducer acc wip s =
      case List.stripPrefix delim s of
        Just s' -> reducer (acc ++ [wip]) [] s'
        Nothing -> case s of
          (hd : tl) -> reducer acc (wip ++ [hd]) tl
          [] -> if not (null wip) then acc ++ [wip] else acc

-- return xs[i1..i2] (incl. i1, excl. i2), using zero-indexed convention
sliceList :: [a] -> Int -> Int -> [a]
sliceList xs i1 i2 =
  take len (drop i1' xs)
  where
    len = i2 - i1'
    i1' = max i1 0

swapIndicesInList :: [a] -> Int -> Int -> [a]
swapIndicesInList xs i1 i2 | i1 == i2 = xs
swapIndicesInList xs i1 i2 =
  sliceList xs 0 l
    ++ [xr]
    ++ sliceList xs (l + 1) i2
    ++ [xl]
    ++ drop (i2 + 1) xs
  where
    xl = xs !! l
    xr = xs !! r
    (l, r) = (min i1 i2, max i1 i2)

countFreq :: (Ord a) => [a] -> Map a Int
countFreq = foldr (\x -> Map.insertWith (+) x 1) Map.empty
