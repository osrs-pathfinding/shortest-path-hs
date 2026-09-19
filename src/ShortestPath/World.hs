module ShortestPath.World
  ( World(..)
  , CollisionMap(..)
  , collisionFlag
  , collisionTiles
  , isWalkable
  , loadWorld
  , loadWorldWithoutSeparators
  , ordinaryWalkingMask
  , ordinaryWalkingNeighborsFromMask
  , walkingNeighbors
  ) where

import Codec.Archive.Zip
import Data.Aeson (eitherDecodeFileStrict')
import Data.Bits ((.&.), Bits(setBit, testBit))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Word (Word8)
import System.Directory (doesFileExist)

import ShortestPath.Tile
import ShortestPath.Separator
import ShortestPath.Transport

data World = World
  { worldCollision :: CollisionMap
  , worldTransports :: Map.Map Tile [Transport]
  , worldGlobalTeleports :: [Transport]
  , worldBanks :: Set.Set Tile
  , worldSeparatorArtifact :: Maybe SeparatorArtifact
  }
  deriving stock (Show)

data CollisionMap = CollisionMap
  { collisionRegions :: Map.Map (Int, Int) BL.ByteString
  }
  deriving stock (Show)

loadWorld :: SourcePaths -> IO World
loadWorld paths = do
  world <- loadWorldWithoutSeparators paths
  exists <- doesFileExist (separatorFile paths)
  if exists then pure () else fail ("required routing separator artifact is missing: " <> separatorFile paths)
  artifact <- either fail pure =<< eitherDecodeFileStrict' (separatorFile paths)
  pure world {worldSeparatorArtifact = Just artifact}

loadWorldWithoutSeparators :: SourcePaths -> IO World
loadWorldWithoutSeparators paths = do
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
      , worldSeparatorArtifact = Nothing
      }

walkingNeighbors :: World -> Tile -> [Tile]
walkingNeighbors world tile =
  let (x, y, p) = unpackTile tile
      adjacent = [(dx, dy, packTile (x + dx) (y + dy) p) | (dx, dy) <- directions]
      regular = ordinaryWalkingNeighborsFromMask tile (ordinaryWalkingMask cm tile)
      blockedOrigins =
        [ next
        | (dx, dy, next) <- adjacent
        , abs dx + abs dy == 1
        , not (isWalkable cm next)
        , Map.member next (worldTransports world)
        ]
      blockedExits =
        [ next
        | (dx, dy, next) <- adjacent
        , isWalkable cm next
        , abs dx + abs dy == 1 || cardinalOpen x y p dx dy
        ]
   in if isWalkable cm tile then regular <> blockedOrigins else blockedExits
 where
  cm = worldCollision world
  directions = [(-1, 0), (1, 0), (0, -1), (0, 1), (-1, -1), (1, -1), (-1, 1), (1, 1)]
  cardinalOpen x y p dx dy =
    isWalkable cm (packTile (x + dx) y p) && isWalkable cm (packTile x (y + dy) p)

-- Bits are clockwise: 0 N, 1 NE, 2 E, 3 SE, 4 S, 5 SW, 6 W, 7 NW.
ordinaryWalkingMask :: CollisionMap -> Tile -> Word8
ordinaryWalkingMask cm tile
  | not (isWalkable cm tile) = 0
  | otherwise = foldl set 0 moves
 where
  (x, y, p) = unpackTile tile
  n a b = collisionFlag cm a b p 0
  s a b = n a (b - 1)
  e a b = collisionFlag cm a b p 1
  w a b = e (a - 1) b
  moves =
    [ (0, n x y)
    , (1, n x y && e x (y + 1) && e x y && n (x + 1) y)
    , (2, e x y)
    , (3, s x y && e x (y - 1) && e x y && s (x + 1) y)
    , (4, s x y)
    , (5, s x y && w x (y - 1) && w x y && s (x - 1) y)
    , (6, w x y)
    , (7, n x y && w x (y + 1) && w x y && n (x - 1) y)
    ]
  set mask (bit, True) = setBit mask bit
  set mask _ = mask

ordinaryWalkingNeighborsFromMask :: Tile -> Word8 -> [Tile]
ordinaryWalkingNeighborsFromMask tile mask =
  [ packTile (x + dx) (y + dy) p
  | (bit, dx, dy) <- [(6, -1, 0), (2, 1, 0), (4, 0, -1), (0, 0, 1), (5, -1, -1), (3, 1, -1), (7, -1, 1), (1, 1, 1)]
  , testBit mask bit
  ]
 where
  (x, y, p) = unpackTile tile

isWalkable :: CollisionMap -> Tile -> Bool
{-# INLINE isWalkable #-}
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
