module ShortestPath.World
  ( World(..)
  , CollisionMap(..)
  , VirtualWall(..)
  , collisionFlag
  , collisionTiles
  , isWalkable
  , isVirtualWallTile
  , loadWorld
  , virtualWalls
  , walkingNeighbors
  , walkingNeighborsRaw
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

data VirtualWall = VirtualWall
  { wallName :: String
  , wallStart :: Tile
  , wallEnd :: Tile
  , wallCrossing :: (Tile, Tile)
  }
  deriving stock (Eq, Show)

virtualWalls :: [VirtualWall]
virtualWalls =
  [ wall "Cathery / White Wolf Mountain" (2854, 3442) (2856, 3440) (2854, 3441) (2855, 3442)
  , wall "Members gate 1" (2836, 3452) (2836, 3449) (2835, 3451) (2837, 3451)
  , wall "Members gate 3" (2932, 3320) (2935, 3320) (2933, 3319) (2933, 3321)
  ]
 where
  wall name (sx, sy) (ex, ey) (ax, ay) (bx, by) =
    VirtualWall name (packTile sx sy 0) (packTile ex ey 0) (packTile ax ay 0, packTile bx by 0)

loadWorld :: SourcePaths -> IO World
loadWorld paths = do
  collision <- loadCollision (collisionZip paths)
  transports <- loadTransports paths
  banks <- Set.fromList <$> loadBanks paths
  let (globals, locals) = splitGlobals transports
      walls = concatMap wallTransports virtualWalls
  pure
    World
      { worldCollision = collision
      , worldTransports = Map.fromListWith (<>) [(o, [t]) | t <- locals <> walls, Just o <- [origin t]]
      , worldGlobalTeleports = globals
      , worldBanks = banks
      }

walkingNeighbors :: World -> Tile -> [Tile]
walkingNeighbors = walkingNeighborsMode True

walkingNeighborsRaw :: World -> Tile -> [Tile]
walkingNeighborsRaw = walkingNeighborsMode False

walkingNeighborsMode :: Bool -> World -> Tile -> [Tile]
{-# INLINE walkingNeighborsMode #-}
walkingNeighborsMode useWalls world tile =
  let (x, y, p) = unpackTile tile
      ordinary =
        [ (-1, 0, w x y p)
        , (1, 0, e x y p)
        , (0, -1, s x y p)
        , (0, 1, n x y p)
        , (-1, -1, sw x y p)
        , (1, -1, se x y p)
        , (-1, 1, nw x y p)
        , (1, 1, ne x y p)
        ]
      adjacent = [(dx, dy, packTile (x + dx) (y + dy) p) | (dx, dy) <- directions]
      regular = [next | (dx, dy, ok) <- ordinary, ok, let next = packTile (x + dx) (y + dy) p, allowed next]
      blockedOrigins =
        [ next
        | (dx, dy, next) <- adjacent
        , abs dx + abs dy == 1
        , not (isWalkable cm next)
        , Map.member next (worldTransports world)
        , allowed next
        ]
      blockedExits =
        [ next
        | (dx, dy, next) <- adjacent
        , isWalkable cm next
        , abs dx + abs dy == 1 || cardinalOpen x y p dx dy
        , allowed next
        ]
   in if isWalkable cm tile then regular <> blockedOrigins else blockedExits
 where
  cm = worldCollision world
  directions = [(-1, 0), (1, 0), (0, -1), (0, 1), (-1, -1), (1, -1), (-1, 1), (1, 1)]
  cardinalOpen x y p dx dy =
    isWalkable cm (packTile (x + dx) y p) && isWalkable cm (packTile x (y + dy) p)
  allowed next = not useWalls || (not (isVirtualWallTile next) && not (blocked tile next))
  n x y p = collisionFlag cm x y p 0
  s x y p = n x (y - 1) p
  e x y p = collisionFlag cm x y p 1
  w x y p = e (x - 1) y p
  ne x y p = n x y p && e x (y + 1) p && e x y p && n (x + 1) y p
  nw x y p = n x y p && w x (y + 1) p && w x y p && n (x - 1) y p
  se x y p = s x y p && e x (y - 1) p && e x y p && s (x + 1) y p
  sw x y p = s x y p && w x (y - 1) p && w x y p && s (x - 1) y p
  blocked a b = Set.member (min a b, max a b) blockedEdges
  blockedEdges = virtualWallEdgeSet

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

isVirtualWallTile :: Tile -> Bool
isVirtualWallTile tile = tile `Set.member` virtualWallTileSet

virtualWallTileSet :: Set.Set Tile
virtualWallTileSet = Set.fromList (concatMap wallTiles virtualWalls)

wallTiles :: VirtualWall -> [Tile]
wallTiles wall
  | sx == ex = [packTile sx y 0 | y <- [min sy ey .. max sy ey]]
  | sy == ey = [packTile x sy 0 | x <- [min sx ex .. max sx ex]]
  | otherwise = [packTile x (sy - (x - sx)) 0 | x <- [min sx ex .. max sx ex]]
 where
  (sx, sy, _) = unpackTile (wallStart wall)
  (ex, ey, _) = unpackTile (wallEnd wall)

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

wallTransports :: VirtualWall -> [Transport]
wallTransports wall = [make a b, make b a]
 where
  (a, b) = wallCrossing wall
  make from to =
    Transport
      { transportType = "VIRTUAL_WALL"
      , origin = Just from
      , destination = Just to
      , duration = 1
      , displayInfo = wallName wall
      , objectInfo = ""
      , consumable = False
      , maxWildernessLevel = Nothing
      , skills = []
      , items = Nothing
      , quests = []
      , varbits = []
      , varPlayers = []
      , source = "virtual-walls"
      }

wallBlockedEdges :: VirtualWall -> [(Tile, Tile)]
wallBlockedEdges wall =
  cardinal <> diagonal
 where
  (sx, sy, _) = unpackTile (wallStart wall)
  (ex, ey, _) = unpackTile (wallEnd wall)
  cardinal
    | sx == ex =
        [ (packTile x y 0, packTile (x + dx) y 0)
        | y <- [min sy ey .. max sy ey]
        , (x, dx) <- [(sx - 1, 1), (sx, 1)]
        ]
    | sy == ey =
        [ (packTile x y 0, packTile x (y + dy) 0)
        | x <- [min sx ex .. max sx ex]
        , (y, dy) <- [(sy - 1, 1), (sy, 1)]
        ]
    | otherwise = []
  diagonal
    | sx == ex || sy == ey = []
    | otherwise =
        [ (packTile x y 0, packTile (x + 1) y 0)
        | x <- [min sx ex .. max sx ex - 1]
        , let y = sy - (x - sx)
        ]
          <> [ (packTile x y 0, packTile x (y - 1) 0)
             | x <- [min sx ex .. max sx ex - 1]
             , let y = sy - (x - sx)
             ]

virtualWallEdgeSet :: Set.Set (Tile, Tile)
virtualWallEdgeSet = Set.fromList (concatMap wallBlockedEdges virtualWalls)

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
