module Day9 (part1, part2) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

newtype DiskMap = DiskMap [Int]

-- (pos, len, file)
newtype ContiguousBlock = ContiguousBlock (Int, Int, Maybe Int) deriving (Show)

parseDiskMap :: String -> DiskMap
parseDiskMap s =
  DiskMap (map (read . (: [])) s)

initBlocks :: DiskMap -> [ContiguousBlock]
initBlocks (DiskMap nums) =
  reverse $ reducer nums True 0 0 []
  where
    reducer :: [Int] -> Bool -> Int -> Int -> [ContiguousBlock] -> [ContiguousBlock]
    reducer xs parity nextId pos acc = case (xs, parity) of
      ([], _) -> acc
      (hd : tl, True) -> reducer tl False (nextId + 1) (pos + hd) (ContiguousBlock (pos, hd, Just nextId) : acc)
      (hd : tl, False) -> reducer tl True nextId (pos + hd) (ContiguousBlock (pos, hd, Nothing) : acc)

makeBlockMap :: [ContiguousBlock] -> Map Int (Maybe Int)
makeBlockMap blocks =
  Map.fromList $ zip [0 ..] segments
  where
    makeSegment (ContiguousBlock (_, len, mId)) = replicate len mId
    segments = concatMap makeSegment blocks

compress :: Map Int (Maybe Int) -> [Int]
compress initM =
  fromJust <$> map snd (Map.toList $ reducer 0 (length initM - 1) initM)
  where
    reducer :: Int -> Int -> Map Int (Maybe Int) -> Map Int (Maybe Int)
    reducer lptr rptr m =
      case (Map.lookup lptr m, Map.lookup rptr m) of
        (Just Nothing, Just (Just n)) ->
          reducer
            (lptr + 1)
            (rptr - 1)
            (Map.delete rptr $ Map.insert lptr (Just n) m)
        (Just (Just _), _) -> reducer (lptr + 1) rptr m
        (_, Just Nothing) -> reducer lptr (rptr - 1) (Map.delete rptr m)
        (Nothing, _) -> m
        (_, Nothing) -> m

checksum :: [Int] -> Int
checksum xs = sum $ zipWith (*) [0 ..] xs

part1 :: String -> Int
part1 contents =
  checksum compressed
  where
    dm = parseDiskMap contents
    blocks = initBlocks dm
    bm = makeBlockMap blocks
    compressed = compress bm

part2 :: String -> Int
part2 contents =
  undefined
