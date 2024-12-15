module Day9 (part1, part2) where

import Data.Map (Map)
import qualified Data.Map as Map

newtype DiskMap = DiskMap [Int] deriving (Show)

data FileBlock = FileBlock {fbLen :: Int, fbFileId :: Int} deriving (Show)

parseDiskMap :: String -> DiskMap
parseDiskMap s =
  DiskMap (map (read . (: [])) s)

-- Make a map of positions to file blocks.
initBlocks :: DiskMap -> Map Int FileBlock
initBlocks (DiskMap nums) =
  bm
  where
    (_, _, _, bm) = foldl reducer (True, 0, 0, Map.empty) nums
    reducer (parity, nextId, pos, acc) n =
      if parity
        then (False, nextId + 1, pos + n, Map.insert pos FileBlock {fbLen = n, fbFileId = nextId} acc)
        else (True, nextId, pos + n, acc)

compress1 :: Map Int FileBlock -> Map Int FileBlock
compress1 initBm =
  reducer 0 initRptr initBm
  where
    (initRptr, _) = Map.findMax initBm

    reducer :: Int -> Int -> Map Int FileBlock -> Map Int FileBlock
    reducer lptr rptr bm =
      if lptr >= rptr
        then bm
        else case Map.lookup rptr bm of
          Just b ->
            if freeSpace == 0
              then
                reducer (lptr + 1) rptr bm
              else
                if movedEntireBlock
                  then
                    reducer
                      (lptr + newBlockSize)
                      (rptr - 1)
                      (Map.insert lptr b $ Map.delete rptr bm)
                  else
                    reducer
                      (lptr + newBlockSize)
                      rptr
                      ( Map.insert lptr FileBlock {fbLen = newBlockSize, fbFileId = fileId} $
                          Map.insert rptr FileBlock {fbLen = remaining, fbFileId = fileId} bm
                      )
            where
              fileId = fbFileId b
              freeSpace = case Map.splitLookup lptr bm of
                (_, Just _, _) -> 0
                (l, Nothing, r) -> if leftBlockInTheWay then 0 else nextFileIndex - lptr
                  where
                    lv = Map.maxViewWithKey l
                    leftBlockInTheWay = case lv of
                      Just ((lpos, lblock), _) -> lpos + fbLen lblock > lptr
                      Nothing -> False
                    (nextFileIndex, _) = Map.findMin r
              newBlockSize = min (fbLen b) freeSpace
              remaining = fbLen b - newBlockSize
              movedEntireBlock = remaining == 0
          Nothing -> reducer lptr (rptr - 1) bm

compress2 :: Map Int FileBlock -> Map Int FileBlock
compress2 initBm =
  reducer maxFileId initRptr initBm
  where
    maxFileId = maximum $ map (fbFileId . snd) $ Map.toList initBm
    (initRptr, _) = Map.findMax initBm

    reducer :: Int -> Int -> Map Int FileBlock -> Map Int FileBlock
    reducer fileId rptr bm =
      if fileId == 0
        then bm
        else case Map.lookup rptr bm of
          Just b
            | fbFileId b == fileId ->
                reducer
                  (fileId - 1)
                  (rptr - 1)
                  ( case leftmostInsertionSpot bm (fbLen b) rptr of
                      Just pos' -> Map.insert pos' b $ Map.delete rptr bm
                      Nothing -> bm
                  )
          _ -> reducer fileId (rptr - 1) bm

    -- Can I insert a block of size `len` at `pos`?
    hasFreeSpace :: Map Int FileBlock -> Int -> Int -> Bool
    hasFreeSpace bm len pos =
      case Map.splitLookup pos bm of
        (_, Just _, _) -> False
        (l, Nothing, r) ->
          let (lv, rv) = (Map.maxViewWithKey l, Map.minViewWithKey r)
           in maybe True (\((k, lb), _) -> pos >= k + fbLen lb) lv
                && maybe True (\((k, _), _) -> k >= pos + len) rv

    -- Find leftmost insertion point for a block at a position less than rbound.
    leftmostInsertionSpot :: Map Int FileBlock -> Int -> Int -> Maybe Int
    leftmostInsertionSpot bm len rbound =
      case freeSpaces of
        (hd : _) -> Just hd
        _ -> Nothing
      where
        freeSpaces = filter (hasFreeSpace bm len) [0 .. (rbound - 1)]

checksum :: Map Int FileBlock -> Int
checksum bm =
  sum $ map cksum $ Map.toList bm
  where
    cksum :: (Int, FileBlock) -> Int
    cksum (pos, FileBlock {fbLen = len, fbFileId = fileId}) =
      fileId * sum [pos + i | i <- [0 .. len - 1]]

part1 :: String -> Int
part1 contents =
  checksum compressed
  where
    dm = parseDiskMap contents
    blocks = initBlocks dm
    compressed = compress1 blocks

part2 :: String -> Int
part2 contents =
  checksum compressed
  where
    dm = parseDiskMap contents
    blocks = initBlocks dm
    compressed = compress2 blocks
