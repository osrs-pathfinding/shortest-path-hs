module ShortestPath.Tile
  ( Tile(..)
  , packTile
  , unpackTile
  , coordinateText
  , chebyshev2
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Int (Int32)

newtype Tile = Tile { unTile :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

packTile :: Int -> Int -> Int -> Tile
packTile x y p = Tile ((x .&. 0x7fff) .|. ((y .&. 0x7fff) `shiftL` 15) .|. ((p .&. 0x3) `shiftL` 30))

unpackTile :: Tile -> (Int, Int, Int)
unpackTile (Tile n) =
  ( n .&. 0x7fff
  , (n `shiftR` 15) .&. 0x7fff
  , fromIntegral ((fromIntegral n :: Int32) `shiftR` 30) .&. 0x3
  )

coordinateText :: Tile -> String
coordinateText t = let (x, y, p) = unpackTile t in show x <> "/" <> show y <> "/" <> show p

chebyshev2 :: Tile -> Tile -> Maybe Int
chebyshev2 a b =
  let (ax, ay, ap) = unpackTile a
      (bx, by, bp) = unpackTile b
   in if ap /= bp then Nothing else Just (max (abs (ax - bx)) (abs (ay - by)))
