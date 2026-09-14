module ShortestPath.Wilderness
  ( GlobalCapability(..)
  , globalCapabilityAt
  ) where

import ShortestPath.Tile

data GlobalCapability = NoGlobals | WildernessGlobals | AllGlobals
  deriving stock (Eq, Ord, Show)

globalCapabilityAt :: Tile -> GlobalCapability
{-# INLINE globalCapabilityAt #-}
globalCapabilityAt tile
  | inArea 2944 3760 448 448 || inArea 2944 10155 518 221 = NoGlobals
  | inArea 2944 3680 448 448 || inArea 2944 10075 518 301 = WildernessGlobals
  | otherwise = AllGlobals
 where
  (x, y, _) = unpackTile tile
  inArea left bottom width height =
    x >= left && x < left + width && y >= bottom && y < bottom + height

