module ShortestPath.Exact.TileAStar.RelaxedGraph
  ( SiteGraph(..)
  , compileRoutingAccount
  , addCost
  , addCostDefault
  , attachedSites
  , binarySearch
  , chebyshevPacked
  , siteComponentGroups
  , stateId
  , targetOverlay
  , targetSeeds
  ) where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Boxed
import qualified Data.Vector.Mutable as BoxedMutable
import qualified Data.Vector.Unboxed as Vector

import ShortestPath.Exact.TileAStar.Types
import ShortestPath.Internal.Cost
import ShortestPath.Pathfinder
import ShortestPath.Tile
import ShortestPath.Topology
import ShortestPath.Transport

compileRoutingAccount :: TileAStar -> RoutingOptions -> CompiledRoutingAccount
compileRoutingAccount astar@(TileAStar topology _) options = account
 where
  account = compileRoutingAccountWithGraph (topologyWorld topology) options (accountSiteGraph astar account)

-- | Immutable account-specific relaxed graph.
accountSiteGraph :: TileAStar -> CompiledRoutingAccount -> SiteGraph
accountSiteGraph (TileAStar topology static) account =
  SiteGraph tiles tileIndex comps staticCount (staticWalkingNetwork static) componentSites reverseEdges
 where
  staticCount = Vector.length (staticTiles static)
  tiles = staticTiles static
  tileIndex = staticSiteTileIndex static
  comps = staticComponents static
  componentSites = staticSiteComponentIds static
  nodeCount = Vector.length tiles
  reverseEdges = reverseAdjacency (nodeCount * 2) (localEdges <> bankEdges <> bankGlobalEdges <> crossingEdges)
  reachableBanks = staticReachableBanks static

  localEdges =
    [ (stateId from banked, stateId to banked, stepCost)
    | compiledAllowTransports account
    , banked <- [False, True]
    , transports <- Map.elems (if banked then bankedLocalTransports availability else carriedLocalTransports availability)
    , t <- transports
    , Just originTile <- [origin t]
    , Just destinationTile <- [destination t]
    , let stepCost = duration t + Map.findWithDefault 0 (transportType t) (compiledTransportPenalties account)
    , Just from <- [nodeFor originTile]
    , Just to <- [nodeFor destinationTile]
    ]

  bankEdges =
    [ (stateId node False, stateId node True, 0)
    | compiledBankPathEnabled account
    , tile <- Set.toList reachableBanks
    , Just node <- [nodeFor tile]
    ]

  bankGlobalEdges =
    [ (stateId from False, stateId to True, stepCost)
    | compiledAllowTransports account
    , compiledBankPathEnabled account
    , bank <- Set.toList reachableBanks
    , Just from <- [nodeFor bank]
    , t <- preparedGlobalTransports availability True
    , Just destinationTile <- [destination t]
    , let stepCost = duration t + Map.findWithDefault 0 (transportType t) (compiledTransportPenalties account)
    , Just to <- [nodeFor destinationTile]
    ]

  crossingEdges =
    [ (stateId from banked, stateId to banked, crossingCost edge)
    | edge <- topologySeparatorCrossings topology
    , (fromTile, toTile) <- [(crossingFromTile edge, crossingToTile edge), (crossingToTile edge, crossingFromTile edge)]
    , banked <- [False, True]
    , Just from <- [nodeFor fromTile]
    , Just to <- [nodeFor toTile]
    ]

  nodeFor tile = IntMap.lookup (unTile tile) tileIndex
  availability = compiledTransportAvailability account

siteComponentGroups :: Int -> Boxed.Vector (Vector.Vector Int) -> Boxed.Vector (Vector.Vector Int)
siteComponentGroups highestComponent comps = runST $ do
  groups <- BoxedMutable.replicate (highestComponent + 1) []
  Boxed.iforM_ comps $ \node attachments ->
    Vector.forM_ attachments $ \cid -> do
      nodes <- BoxedMutable.read groups cid
      BoxedMutable.write groups cid (node : nodes)
  Boxed.map Vector.fromList <$> Boxed.freeze groups

attachedSites :: SiteGraph -> Int -> Vector.Vector Int
attachedSites graph node =
  case Vector.length attachments of
    0 -> Vector.empty
    1 -> siteComponentSiteIds graph Boxed.! Vector.head attachments
    _ -> Vector.fromList (IntSet.toList (IntSet.fromList
      [site | cid <- Vector.toList attachments, site <- Vector.toList (siteComponentSiteIds graph Boxed.! cid)]))
 where
  attachments = siteComponents graph Boxed.! node

targetOverlay :: TileAStar -> CompiledRoutingAccount -> Tile -> TargetOverlay
targetOverlay (TileAStar topology _) account target =
  TargetOverlay packed components attachments node synthetic
 where
  graph = compiledSiteGraph account
  packed = unTile target
  components = Vector.fromList (routingPointAttachments topology target)
  existing = IntMap.lookup packed (siteTileIndex graph)
  node = maybe (Vector.length (siteTiles graph)) id existing
  synthetic = maybe True (const False) existing
  -- Assign a multi-component site to its first shared component so the union is emitted once.
  attachments = Vector.fromList
    [ (site, chebyshevPacked packed (siteTiles graph Vector.! site))
    | cid <- Vector.toList components
    , site <- Vector.toList (siteComponentSiteIds graph Boxed.! cid)
    , cid == firstSharedComponent site
    ]
  firstSharedComponent site = case Vector.find (`Vector.elem` components) (siteComponents graph Boxed.! site) of
    Just cid -> cid
    Nothing -> error "target attachment missing shared routing component"

targetSeeds :: TargetOverlay -> [(Int, Int)]
targetSeeds overlay = [(stateId (targetSite overlay) banked, 0) | banked <- [False, True]]

stateId :: Int -> Bool -> Int
{-# INLINE stateId #-}
stateId node banked = node * 2 + if banked then 1 else 0

chebyshevPacked :: Int -> Int -> Int
{-# INLINE chebyshevPacked #-}
chebyshevPacked a b =
  let (ax, ay, ap) = unpackTile (Tile a)
      (bx, by, bp) = unpackTile (Tile b)
   in if ap == bp then max (abs (ax - bx)) (abs (ay - by)) else maxBound

reverseAdjacency :: Int -> [(Int, Int, Int)] -> Boxed.Vector (Vector.Vector (Int, Int))
reverseAdjacency size edges = runST $ do
  lists <- Boxed.thaw (Boxed.replicate size [])
  forM_ edges $ \(from, to, cost) -> do
    current <- BoxedMutable.read lists to
    BoxedMutable.write lists to ((from, cost) : current)
  Boxed.map Vector.fromList <$> Boxed.freeze lists

binarySearch :: Int -> Vector.Vector Int -> Maybe Int
{-# INLINE binarySearch #-}
binarySearch needle values = go 0 (Vector.length values - 1)
 where
  go lo hi
    | lo > hi = Nothing
    | current == needle = Just mid
    | current < needle = go (mid + 1) hi
    | otherwise = go lo (mid - 1)
   where
    mid = (lo + hi) `div` 2
    current = values Vector.! mid
