{-# LANGUAGE DeriveAnyClass #-}

module ShortestPath.Hierarchy.Partition
  ( LeafId(..)
  , TileClass(..)
  , Partition(..)
  , PartitionAssignment(..)
  , loadPartition
  , partitionFromAssignments
  , syntheticPartitionCheck
  ) where

import Control.Monad (forM_)
import Data.Binary (Binary)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed.Mutable as Mutable
import GHC.Generics (Generic)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

import ShortestPath.Tile
import ShortestPath.Transport (Transport(..))
import ShortestPath.World

data LeafId = LeafId Int String
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Binary)

data TileClass
  = LeafTile LeafId
  | SeparatorTile String Int
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Binary)

data Partition = Partition
  { tileClasses :: IntMap.IntMap TileClass
  , leafTileSets :: Map.Map LeafId IntSet.IntSet
  , separatorTileSet :: IntSet.IntSet
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Binary)

data PartitionAssignment = PartitionAssignment
  { assignmentComponent :: Int
  , assignmentTile :: Tile
  , assignmentRegion :: String
  , assignmentKind :: String
  , assignmentLevel :: Int
  }
  deriving stock (Eq, Show)

loadPartition :: World -> FilePath -> IO Partition
loadPartition world path = do
  putStrLn ("loading partition assignments: " <> path)
  contents <- readFile path
  rows <- either fail pure (parseAssignments contents)
  putStrLn ("partition assignment rows: " <> show (length rows))
  walkable <- enumerateWalkable (worldCollision world)
  (components, owner) <- rawComponents world walkable
  let reachable = reachableComponents world owner
      raw = [(cid, ns) | (cid, ns) <- components, IntSet.member cid reachable]
      selected = Set.fromList (map assignmentComponent rows)
      complete = rows <> [ PartitionAssignment cid (Tile n) ("raw-" <> show cid) "leaf" 0
                         | (cid, ns) <- raw
                         , Set.notMember cid selected
                         , n <- ns
                         ]
  putStrLn ("reachable raw components: " <> show (length raw))
  putStrLn ("building partition index from " <> show (length complete) <> " assignments")
  either fail pure (partitionFromAssignments owner reachable complete)

partitionFromAssignments
  :: IntMap.IntMap Int
  -> IntSet.IntSet
  -> [PartitionAssignment]
  -> Either String Partition
partitionFromAssignments owner reachable assignments = do
  let expected = IntSet.fromList [tile | (tile, cid) <- IntMap.toList owner, IntSet.member cid reachable]
      rows = [(unTile tile, row) | row <- assignments, let tile = assignmentTile row]
      duplicateTiles = duplicates (map fst rows)
      classes = [(n, toClass row) | (n, row) <- rows]
      leafEntries = [(n, leaf) | (n, Right (LeafTile leaf)) <- classes]
      separatorEntries = [(n, c) | (n, Right c@(SeparatorTile _ _)) <- classes]
      badComponents = [ (n, assignmentComponent row, IntMap.lookup n owner)
                      | row <- assignments
                      , let n = unTile (assignmentTile row)
                      , IntMap.lookup n owner /= Just (assignmentComponent row)
                      ]
      assigned = IntSet.fromList (map fst rows)
      badKinds = [err | (_, Left err) <- classes]
      leaves = Map.fromListWith IntSet.union [(leaf, IntSet.singleton n) | (n, leaf) <- leafEntries]
      separators = IntSet.fromList (map fst separatorEntries)
      partition = Partition (IntMap.fromList [(n, c) | (n, Right c) <- classes]) leaves separators
  whenLeft (not (null duplicateTiles)) ("duplicate assignment tiles: " <> show duplicateTiles)
  whenLeft (not (null badKinds)) ("invalid assignment rows: " <> show badKinds)
  whenLeft (not (null badComponents)) ("source-component ownership mismatch: " <> show badComponents)
  whenLeft (not (IntSet.null (assigned `IntSet.difference` expected))) "assignments contain non-reachable or non-walkable tiles"
  whenLeft (assigned /= expected) ("incomplete assignment coverage: expected " <> show (IntSet.size expected) <> ", got " <> show (IntSet.size assigned))
  whenLeft (IntSet.fromList (IntMap.keys (tileClasses partition)) /= expected) "leaf map consistency failed: tile class map differs from coverage"
  whenLeft (IntSet.fromList (concatMap IntSet.toList (Map.elems leaves)) /= expected `IntSet.difference` separators) "leaf map consistency failed: leaf tiles differ from non-separator tiles"
  whenLeft (IntSet.size (IntSet.fromList (concatMap IntSet.toList (Map.elems leaves))) /= sum (map IntSet.size (Map.elems leaves))) "leaf map consistency failed: tile appears in multiple leaves"
  whenLeft (separators /= IntSet.fromList [n | (n, SeparatorTile _ _) <- [(n, c) | (n, Right c) <- classes]]) "separator set consistency failed"
  pure partition
 where
  toClass row
    | assignmentKind row == "leaf" = Right (LeafTile (LeafId (assignmentComponent row) (assignmentRegion row)))
    | assignmentKind row == "separator" = Right (SeparatorTile (assignmentRegion row) (assignmentLevel row))
    | otherwise = Left ("unknown kind " <> assignmentKind row)

  whenLeft True message = Left message
  whenLeft False _ = Right ()

duplicates :: Ord a => [a] -> [a]
duplicates values = Map.keys (Map.filter (> (1 :: Int)) (Map.fromListWith (+) [(v, 1) | v <- values]))

syntheticPartitionCheck :: Either String ()
syntheticPartitionCheck = do
  let tiles = [packTile x y 0 | (x, y) <- [(0, 0), (1, 0), (2, 0), (3, 0)]]
      owner = IntMap.fromList [(unTile tile, 1) | tile <- tiles]
      rows = case tiles of
        [a, b, separator, d] ->
          [ PartitionAssignment 1 a "1a" "leaf" 1
          , PartitionAssignment 1 b "1a" "leaf" 1
          , PartitionAssignment 1 separator "1" "separator" 0
          , PartitionAssignment 1 d "1b" "leaf" 1
          ]
        _ -> []
  partition <- partitionFromAssignments owner (IntSet.singleton 1) rows
  if IntSet.size (separatorTileSet partition) == 1
    && Map.size (leafTileSets partition) == 2
    then pure ()
    else Left "synthetic partition shape mismatch"

parseAssignments :: String -> Either String [PartitionAssignment]
parseAssignments input = traverse parseRow (filter dataRow (drop 1 (lines input)))
 where
  dataRow [] = False
  dataRow ('#':_) = False
  dataRow _ = True
  parseRow line =
    case map Text.strip (Text.splitOn (Text.singleton ',') (Text.pack line)) of
      [component, x, y, plane, region, kind, level] -> do
        componentNumber <- number "component" component
        tileX <- number "x" x
        tileY <- number "y" y
        tilePlane <- number "plane" plane
        separatorLevel <- number "level" level
        pure (PartitionAssignment componentNumber (packTile tileX tileY tilePlane) (Text.unpack region) (Text.unpack kind) separatorLevel)
      fields -> Left ("invalid KaHIP assignment row with " <> show (length fields) <> " fields")
  number name value = maybe (Left ("invalid " <> name <> " value: " <> Text.unpack value)) Right (readMaybe (Text.unpack value))

enumerateWalkable :: CollisionMap -> IO IntSet.IntSet
enumerateWalkable collision = do
  let regions = Map.toList (collisionRegions collision)
      tiles = [unTile (packTile (rx * 64 + x) (ry * 64 + y) plane)
              | ((rx, ry), _) <- regions
              , plane <- [0 .. 3]
              , x <- [0 .. 63]
              , y <- [0 .. 63]
              , isWalkable collision (packTile (rx * 64 + x) (ry * 64 + y) plane)]
  putStrLn ("enumerated raw walkable tiles: " <> show (length tiles))
  pure (IntSet.fromList tiles)

rawComponents :: World -> IntSet.IntSet -> IO ([(Int, [Int])], IntMap.IntMap Int)
rawComponents world walkable = do
  queue <- Mutable.new (max 1 (IntSet.size walkable))
  go queue 1 [] IntMap.empty walkable
 where
  go queue cid done owner remaining =
    case IntSet.minView remaining of
      Nothing -> pure (reverse done, owner)
      Just (start, rest) -> do
        Mutable.write queue 0 start
        (tiles, owner', remaining') <- flood queue cid owner rest 0 1 []
        if length tiles >= 10000 || cid `mod` 1000 == 0
          then putStrLn ("raw component " <> show cid <> ": " <> show (length tiles) <> " tiles") >> hFlush stdout
          else pure ()
        go queue (cid + 1) ((cid, tiles) : done) owner' remaining'

  flood queue cid owner remaining readIx writeIx found
    | readIx == writeIx = pure (reverse found, owner, remaining)
    | otherwise = do
        packed <- Mutable.read queue readIx
        let next = [unTile tile | tile <- walkingNeighborsRaw world (Tile packed), IntSet.member (unTile tile) remaining]
            remaining' = foldr IntSet.delete remaining next
        forM_ (zip [writeIx ..] next) (uncurry (Mutable.write queue))
        let owner' = IntMap.insert packed cid owner
        flood queue cid owner' remaining' (readIx + 1) (writeIx + length next) (packed : found)

reachableComponents :: World -> IntMap.IntMap Int -> IntSet.IntSet
reachableComponents world owner =
  case IntMap.lookup (unTile (packTile 3221 3218 0)) owner of
    Nothing -> IntSet.empty
    Just start -> close (IntSet.singleton start) [start]
 where
  localEdges =
    [ (a, b)
    | transport <- concat (Map.elems (worldTransports world))
    , transportType transport /= "VIRTUAL_WALL"
    , Just originTile <- [origin transport]
    , Just destinationTile <- [destination transport]
    , a <- componentsAt originTile
    , b <- componentsAt destinationTile
    ]
  globalDestinations =
    IntSet.fromList
      [ component
      | transport <- worldGlobalTeleports world
      , Just destinationTile <- [destination transport]
      , component <- componentsAt destinationTile
      ]
  componentsAt tile = IntSet.toList (IntSet.fromList
    [ component
    | candidate <- tile : walkingNeighborsRaw world tile
    , Just component <- [IntMap.lookup (unTile candidate) owner]
    ])
  close seen [] = seen
  close seen (component:rest) =
    let next = [b | (a, b) <- localEdges, a == component] <> IntSet.toList globalDestinations
        fresh = filter (`IntSet.notMember` seen) next
     in close (foldr IntSet.insert seen fresh) (fresh <> rest)
