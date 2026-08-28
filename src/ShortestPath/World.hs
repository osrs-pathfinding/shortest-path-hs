module ShortestPath.World
  ( World(..)
  , CollisionMap(..)
  , collisionFlag
  , collisionTiles
  , isWalkable
  , loadWorld
  , walkingNeighbors
  ) where

import Codec.Archive.Zip
import Data.Bits ((.&.), Bits(testBit))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import ShortestPath.Tile
import ShortestPath.Transport

data World = World
  { worldCollision :: CollisionMap
  , worldTransports :: Map.Map Tile [Transport]
  , worldGlobalTeleports :: [Transport]
  , worldBanks :: Set.Set Tile
  }
  deriving stock (Show)

data CollisionMap = CollisionMap
  { collisionRegions :: Map.Map (Int, Int) BL.ByteString
  }
  deriving stock (Show)

loadWorld :: SourcePaths -> IO World
loadWorld paths = do
  collision <- loadCollision (collisionZip paths)
  transports <- loadTransports paths
  banks <- Set.fromList <$> loadBanks paths
  let (globals, locals) = splitGlobals transports
  pure
    World
      { worldCollision = collision
      , worldTransports = Map.fromListWith (<>) [(o, [t]) | t <- locals, Just o <- [origin t]]
      , worldGlobalTeleports = globals
      , worldBanks = banks
      }

walkingNeighbors :: World -> Tile -> [Tile]
walkingNeighbors world tile =
  let (x, y, p) = unpackTile tile
      candidates =
        [ (-1, 0, w x y p)
        , (1, 0, e x y p)
        , (0, -1, s x y p)
        , (0, 1, n x y p)
        , (-1, -1, sw x y p)
        , (1, -1, se x y p)
        , (-1, 1, nw x y p)
        , (1, 1, ne x y p)
        ]
   in [packTile (x + dx) (y + dy) p | (dx, dy, ok) <- candidates, ok]
 where
  cm = worldCollision world
  n x y p = collisionFlag cm x y p 0
  s x y p = n x (y - 1) p
  e x y p = collisionFlag cm x y p 1
  w x y p = e (x - 1) y p
  ne x y p = n x y p && e x (y + 1) p && e x y p && n (x + 1) y p
  nw x y p = n x y p && w x (y + 1) p && w x y p && n (x - 1) y p
  se x y p = s x y p && e x (y - 1) p && e x y p && s (x + 1) y p
  sw x y p = s x y p && w x (y - 1) p && w x y p && s (x - 1) y p

isWalkable :: CollisionMap -> Tile -> Bool
isWalkable cm tile =
  let (x, y, p) = unpackTile tile
   in or
        [ collisionFlag cm x y p 0
        , collisionFlag cm x y p 1
        , collisionFlag cm x (y - 1) p 0
        , collisionFlag cm (x - 1) y p 1
        ]

collisionTiles :: CollisionMap -> [Tile]
collisionTiles cm =
  [ tile
  | ((rx, ry), _) <- Map.toList (collisionRegions cm)
  , p <- [0 .. 3]
  , lx <- [0 .. 63]
  , ly <- [0 .. 63]
  , let tile = packTile (rx * 64 + lx) (ry * 64 + ly) p
  , isWalkable cm tile
  ]

loadCollision :: FilePath -> IO CollisionMap
loadCollision path = do
  archive <- toArchive <$> BL.readFile path
  pure (CollisionMap (Map.fromList (mapMaybe entry (zEntries archive))))
 where
  entry e =
    case parseRegionName (eRelativePath e) of
      Just key -> Just (key, fromEntry e)
      Nothing -> Nothing

collisionFlag :: CollisionMap -> Int -> Int -> Int -> Int -> Bool
collisionFlag cm x y p f =
  case Map.lookup (x `div` 64, y `div` 64) (collisionRegions cm) of
    Nothing -> False
    Just bytes ->
      let bit = ((p * 64 * 64) + ((y .&. 63) * 64) + (x .&. 63)) * 2 + f
          byteIndex = bit `div` 8
          bitIndex = bit `mod` 8
       in byteIndex < fromIntegral (BL.length bytes) && BL.index bytes (fromIntegral byteIndex) `testBit` bitIndex

splitGlobals :: [Transport] -> ([Transport], [Transport])
splitGlobals = foldr go ([], [])
 where
  go t (gs, ls) = case origin t of
    Nothing -> (t : gs, ls)
    Just _ -> (gs, t : ls)

parseRegionName :: FilePath -> Maybe (Int, Int)
parseRegionName name =
  case break (== '_') name of
    (a, '_' : b) -> (,) <$> readMaybe a <*> readMaybe b
    _ -> Nothing

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = foldr (\x acc -> maybe acc (: acc) (f x)) []
