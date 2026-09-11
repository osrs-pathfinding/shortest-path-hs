module ShortestPath.Internal.Cost
  ( addCost
  , addCostDefault
  ) where

addCost :: Int -> Int -> Maybe Int
{-# INLINE addCost #-}
addCost a b
  | a == maxBound || b == maxBound || b < 0 || a > maxBound - b = Nothing
  | otherwise = Just (a + b)

addCostDefault :: Int -> Int -> Int -> Int
{-# INLINE addCostDefault #-}
addCostDefault fallback a b = maybe fallback id (addCost a b)
