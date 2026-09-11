module ShortestPath.Exact.TileAStar.RelaxedGraph
  ( SiteGraph(..)
  , addCost
  , addCostDefault
  , attachedSites
  , binarySearch
  , chebyshevPacked
  , siteGraph
  , stateId
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

import ShortestPath.Exact.TileAStar.SparseWalking
import ShortestPath.Exact.TileAStar.Types
import ShortestPath.Pathfinder
import ShortestPath.Tile
import ShortestPath.Topology
import ShortestPath.Transport
import ShortestPath.World

-- | The query-specific relaxed graph. Sites retain every natural-component
-- attachment; walking inside an attachment is relaxed to Chebyshev distance.
data SiteGraph = SiteGraph
  { siteTiles :: Vector.Vector Int
  , siteTileIndex :: IntMap.IntMap Int
  , siteComponents :: Boxed.Vector (Vector.Vector Int)
  , siteStaticCount :: !Int
  , siteSparseNetwork :: SparseWalkingNetwork
  , siteComponentSiteIds :: Boxed.Vector (Vector.Vector Int)
  , siteReverseEdges :: Boxed.Vector (Vector.Vector (Int, Int))
  }

siteGraph :: TileAStar -> Query -> QueryTransportAvailability -> SiteGraph
siteGraph (TileAStar topology static) q availability =
  SiteGraph tiles tileIndex comps staticCount (staticWalkingNetwork static) componentSites reverseEdges
 where
  staticCount = Vector.length (staticTiles static)
  queryExtras =
    [ packed
    | packed <- IntSet.toAscList (IntSet.fromList [unTile (queryStart q), unTile (queryTarget q)])
    , binarySearch packed (staticTiles static) == Nothing
    ]
  tiles = staticTiles static <> Vector.fromList queryExtras
  tileIndex = IntMap.fromList [(packed, ix) | (ix, packed) <- Vector.toList (Vector.indexed tiles)]
  extraComps = Boxed.fromList [Vector.fromList (structurallyReachablePointAttachments topology (Tile packed)) | packed <- queryExtras]
  comps = staticComponents static <> extraComps
  componentSites = siteComponentGroups (maxComponentId components) comps
  nodeCount = Vector.length tiles
  reverseEdges = reverseAdjacency (nodeCount * 2) (localEdges <> bankEdges <> bankGlobalEdges)
  reachableBanks = Set.filter (not . null . structurallyReachablePointAttachments topology) (worldBanks world)
  world = topologyWorld topology
  components = topologyNaturalComponents topology

  localEdges =
    [ (stateId from banked, stateId to banked, stepCost)
    | allowTransports q
    , banked <- [False, True]
    , transports <- Map.elems (if banked then bankedLocalTransports availability else carriedLocalTransports availability)
    , t <- transports
    , Just originTile <- [origin t]
    , Just (destinationTile, stepCost, _) <- [preparedTransport q t]
    , Just from <- [nodeFor originTile]
    , Just to <- [nodeFor destinationTile]
    ]

  bankEdges =
    [ (stateId node False, stateId node True, 0)
    | bankPathEnabled q
    , tile <- Set.toList reachableBanks
    , Just node <- [nodeFor tile]
    ]

  bankGlobalEdges =
    [ (stateId from False, stateId to True, stepCost)
    | allowTransports q
    , bankPathEnabled q
    , bank <- Set.toList reachableBanks
    , Just from <- [nodeFor bank]
    , t <- preparedGlobalTransports availability True
    , Just (destinationTile, stepCost, _) <- [preparedTransport q t]
    , Just to <- [nodeFor destinationTile]
    ]

  nodeFor tile = IntMap.lookup (unTile tile) tileIndex

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

targetSeeds :: SiteGraph -> Tile -> [(Int, Int)]
targetSeeds graph target =
  [ (stateId node banked, 0)
  | Just node <- [IntMap.lookup (unTile target) (siteTileIndex graph)]
  , banked <- [False, True]
  ]

stateId :: Int -> Bool -> Int
stateId node banked = node * 2 + if banked then 1 else 0

addCost :: Int -> Int -> Maybe Int
addCost a b
  | a == maxBound || b == maxBound || b < 0 || a > maxBound - b = Nothing
  | otherwise = Just (a + b)

addCostDefault :: Int -> Int -> Int -> Int
addCostDefault fallback a b = maybe fallback id (addCost a b)

chebyshevPacked :: Int -> Int -> Int
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
