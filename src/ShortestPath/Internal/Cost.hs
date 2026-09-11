module ShortestPath.Internal.Cost
  ( addCost
  , addCostDefault
  ) where

addCost :: Int -> Int -> Maybe Int
addCost a b
  | a == maxBound || b == maxBound || b < 0 || a > maxBound - b = Nothing
  | otherwise = Just (a + b)

addCostDefault :: Int -> Int -> Int -> Int
addCostDefault fallback a b = maybe fallback id (addCost a b)
