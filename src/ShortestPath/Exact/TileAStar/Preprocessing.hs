module ShortestPath.Exact.TileAStar.Preprocessing
  ( buildTileStatic
  , componentTileGroups
  ) where

import Control.Monad.ST (runST)
import Data.Int (Int32)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as Mutable

import ShortestPath.Exact.TileAStar.SparseWalking
import ShortestPath.Exact.TileAStar.RelaxedGraph (binarySearch, siteComponentGroups)
import ShortestPath.Exact.TileAStar.Types
import ShortestPath.Tile
import ShortestPath.Topology
import ShortestPath.Transport
import ShortestPath.World

componentTileGroups :: NaturalComponents -> Boxed.Vector (Vector.Vector Int)
componentTileGroups components = runST $ do
  counts <- Mutable.replicate groupCount 0
  Vector.forM_ (componentOwnerIds components) $ \cid ->
    Mutable.modify counts (+ 1) cid
  frozenCounts <- Vector.freeze counts
  let starts = Vector.scanl' (+) 0 frozenCounts
  cursors <- Vector.thaw starts
  grouped <- Mutable.new (Vector.length (componentOwnerTiles components))
  Vector.iforM_ (componentOwnerTiles components) $ \ix packed -> do
    let cid = componentOwnerIds components Vector.! ix
    writeIx <- Mutable.read cursors cid
    Mutable.write grouped writeIx packed
    Mutable.write cursors cid (writeIx + 1)
  frozenGrouped <- Vector.freeze grouped
  pure (Boxed.generate groupCount (\cid -> Vector.slice (starts Vector.! cid) (frozenCounts Vector.! cid) frozenGrouped))
 where
  groupCount = maxComponentId components + 1

buildTileStatic :: WorldTopology -> TileStatic
buildTileStatic topology =
  TileStatic searchTiles searchComponents walkingMasks northNodes southNodes tiles comps siteIndex componentSites reachableBanks network
 where
  searchPairs = reachableComponentTiles topology
  searchTiles = Vector.fromList (map fst searchPairs)
  searchComponents = Vector.fromList (map snd searchPairs)
  walkingMasks = Vector.map (ordinaryWalkingMask (worldCollision world) . Tile) searchTiles
  northNodes = Vector.map (nodeAt . (+ 32768)) searchTiles
  southNodes = Vector.map (nodeAt . subtract 32768) searchTiles
  nodeAt packed = fromIntegral (fromMaybe (-1) (binarySearch packed searchTiles)) :: Int32
  sites = Set.toAscList (Set.fromList (staticEndpoints <> crossingEndpoints <> Set.toList reachableBanks))
  reachableBanks = Set.filter (not . null . routingPointAttachments topology) (worldBanks world)
  staticEndpoints =
    [ tile
    | t <- concat (Map.elems (worldTransports world)) <> worldGlobalTeleports world
    , Just tile <- [origin t] <> [destination t]
    ]
  crossingEndpoints = [tile | edge <- topologySeparatorCrossings topology, tile <- [crossingFromTile edge, crossingToTile edge]]
  tiles = Vector.fromList (map unTile sites)
  comps = Boxed.fromList [Vector.fromList (routingPointAttachments topology (Tile packed)) | packed <- Vector.toList tiles]
  siteIndex = IntMap.fromList [(packed, ix) | (ix, packed) <- Vector.toList (Vector.indexed tiles)]
  componentSites = siteComponentGroups (maxComponentId components) comps
  network = buildSparseWalkingNetworkComponents (Vector.length tiles)
    [(cid, ix, Tile packed) | (ix, packed) <- Vector.toList (Vector.indexed tiles), cid <- Vector.toList (comps Boxed.! ix)]
  world = topologyWorld topology
  components = topologyRoutingComponents topology
