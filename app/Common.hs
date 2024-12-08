module Common (splitList) where

import Data.List (stripPrefix)

splitList :: (Eq a) => [a] -> [a] -> [[a]]
splitList delim =
  reducer [] []
  where
    reducer acc wip s =
      case stripPrefix delim s of
        Just s' -> reducer (acc ++ [wip]) [] s'
        Nothing -> case s of
          (hd : tl) -> reducer acc (wip ++ [hd]) tl
          [] -> if not (null wip) then acc ++ [wip] else acc
