module ShortestPath.Exact.TileAStar.SparseWalking
  ( SparseWalkingNetwork(..)
  , buildSparseWalkingNetwork
  , buildSparseWalkingNetworkComponents
  , sparseWalkingDistance
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Binary (Binary(..))
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as Mutable

import ShortestPath.Internal.MutableHeap
import ShortestPath.Internal.Cost
import ShortestPath.Tile

-- | A CSR graph preserving twice the Chebyshev distance between original
-- same-plane sites. Rotated Manhattan coordinates and Steiner vertices avoid
-- materialising the complete same-component walking clique.
data SparseWalkingNetwork = SparseWalkingNetwork
  { sparseOriginalCount :: !Int
  , sparseVertexCount :: !Int
  , sparseSteinerCount :: !Int
  , sparseWalkingEdgeCount :: !Int
  , sparseOffsets :: Vector.Vector Int
  , sparseDestinations :: Vector.Vector Int
  , sparseWeights :: Vector.Vector Int
  }
  deriving stock (Eq, Show)

instance Binary SparseWalkingNetwork where
  put value = do
    put (sparseOriginalCount value)
    put (sparseVertexCount value)
    put (sparseSteinerCount value)
    put (sparseWalkingEdgeCount value)
    put (Vector.toList (sparseOffsets value))
    put (Vector.toList (sparseDestinations value))
    put (Vector.toList (sparseWeights value))
  get =
    SparseWalkingNetwork
      <$> get
      <*> get
      <*> get
      <*> get
      <*> (Vector.fromList <$> get)
      <*> (Vector.fromList <$> get)
      <*> (Vector.fromList <$> get)

data ManhattanPoint = ManhattanPoint
  { pointOriginal :: !Int
  , pointA :: !Int
  , pointB :: !Int
  }
  deriving stock (Eq, Show)

buildSparseWalkingNetwork :: [(Int, Tile)] -> SparseWalkingNetwork
buildSparseWalkingNetwork sites =
  SparseWalkingNetwork originalCount vertexCount steinerCount edgeCount offsets destinations weights
 where
  points = map toPoint sites
  originalCount = if null sites then 0 else maximum (map fst sites) + 1
  (nextVertex, edges) = buildManhattanEdges originalCount points
  vertexCount = nextVertex
  steinerCount = vertexCount - originalCount
  edgeCount = length edges
  (offsets, destinations, weights) = undirectedAdjacency vertexCount edges
  toPoint (siteId, tile) =
    let (x, y, _) = unpackTile tile
     in ManhattanPoint siteId (x + y) (x - y)

sparseWalkingDistance :: SparseWalkingNetwork -> Int -> Int -> Maybe Int
sparseWalkingDistance network source target
  | source < 0 || source >= sparseOriginalCount network = Nothing
  | target < 0 || target >= sparseOriginalCount network = Nothing
  | otherwise = finiteDistance (dijkstra source Vector.! target)
 where
  finiteDistance value
    | value == maxBound = Nothing
    | otherwise = Just value
  dijkstra start = runST $ do
    result <- Mutable.replicate (sparseVertexCount network) maxBound
    queue <- heapNew (max 1 (sparseWalkingEdgeCount network * 4 + 1))
    Mutable.write result start 0
    heapPush queue 0 start 0
    let go = do
          popped <- heapPop queue
          case popped of
            Nothing -> Vector.freeze result
            Just (_, node, cost) -> do
              known <- Mutable.read result node
              if cost /= known
                then go
                else relaxEdges result queue cost node >> go
    go
  relaxEdges :: Mutable.MVector s Int -> MutableHeap s -> Int -> Int -> ST s ()
  relaxEdges result queue cost node = goEdges (sparseOffsets network Vector.! node)
   where
    end = sparseOffsets network Vector.! (node + 1)
    goEdges ix
      | ix >= end = pure ()
      | otherwise = do
          let next = sparseDestinations network Vector.! ix
              edgeCost = sparseWeights network Vector.! ix
          case addCost cost edgeCost of
            Nothing -> pure ()
            Just newCost -> do
              old <- Mutable.read result next
              when (newCost < old) $ do
                Mutable.write result next newCost
                heapPush queue newCost next newCost
          goEdges (ix + 1)

buildSparseWalkingNetworkComponents :: Int -> [(Int, Int, Tile)] -> SparseWalkingNetwork
buildSparseWalkingNetworkComponents originalCount sites =
  SparseWalkingNetwork originalCount vertexCount steinerCount edgeCount offsets destinations weights
 where
  grouped = Map.elems (Map.fromListWith (<>) [(cid, [(siteId, tile)]) | (cid, siteId, tile) <- sites])
  (vertexCount, edges) = foldl' addComponent (originalCount, []) grouped
  steinerCount = vertexCount - originalCount
  edgeCount = length edges
  (offsets, destinations, weights) = undirectedAdjacency vertexCount edges
  addComponent (next, allEdges) componentSites =
    let points = map toPoint componentSites
        (next', componentEdges) = buildManhattanEdges next points
     in (next', componentEdges <> allEdges)
  toPoint (siteId, tile) =
    let (x, y, _) = unpackTile tile
     in ManhattanPoint siteId (x + y) (x - y)

data SplitAxis = SplitA | SplitB

buildManhattanEdges :: Int -> [ManhattanPoint] -> (Int, [(Int, Int, Int)])
buildManhattanEdges firstSteiner points = go firstSteiner (sortPoints points) []
 where
  sortPoints = sortBy (comparing pointA <> comparing pointB <> comparing pointOriginal)
  go next [] edges = (next, edges)
  go next [_] edges = (next, edges)
  go next pts@(firstPoint:_:_) edges
    | allSameA && allSameB = (next, zeroChain pts edges)
    | allSameA = splitOn SplitB next pts edges
    | otherwise = splitOn SplitA next pts edges
   where
    allSameA = all ((== pointA firstPoint) . pointA) pts
    allSameB = all ((== pointB firstPoint) . pointB) pts
  splitOn axis next pts edges =
    let sorted = sortBy (comparing (coordOf axis) <> comparing pointA <> comparing pointB <> comparing pointOriginal) pts
        (left, right, m) = balancedGapSplit (coordOf axis) sorted
        (next', edges') = addProjectionLevel axis m next pts edges
        (next'', edges'') = go next' left edges'
     in go next'' right edges''
  zeroChain [] edges = edges
  zeroChain [_] edges = edges
  zeroChain (p:q:rest) edges = zeroChain (q:rest) ((pointOriginal p, pointOriginal q, 0) : edges)

balancedGapSplit :: (ManhattanPoint -> Int) -> [ManhattanPoint] -> ([ManhattanPoint], [ManhattanPoint], Int)
balancedGapSplit coord pts = pickGap gaps
 where
  preferred = length pts `div` 2
  gaps = [(abs (i - preferred), i, coord left, coord right) | (i, (left, right)) <- zip [1 :: Int ..] (zip pts (drop 1 pts)), coord left < coord right]
  pickGap [] = error "sparse walking split without a strict coordinate gap"
  pickGap candidates =
    let (_, i, l, r) = minimum candidates
     in (take i pts, drop i pts, (l + r) `div` 2)

coordOf, otherCoordOf :: SplitAxis -> ManhattanPoint -> Int
coordOf SplitA = pointA
coordOf SplitB = pointB
otherCoordOf SplitA = pointB
otherCoordOf SplitB = pointA

addProjectionLevel :: SplitAxis -> Int -> Int -> [ManhattanPoint] -> [(Int, Int, Int)] -> (Int, [(Int, Int, Int)])
addProjectionLevel axis m firstProjection pts edges =
  (firstProjection + length projections, chain sortedProjections (spokes <> edges))
 where
  projections = zip [firstProjection ..] pts
  spokes = [(pointOriginal p, steiner, abs (coordOf axis p - m)) | (steiner, p) <- projections]
  chain sorted edgesSoFar = foldr addChain edgesSoFar (zip sorted (drop 1 sorted))
  sortedProjections = sortBy (comparing (otherCoordOf axis . snd) <> comparing (pointOriginal . snd)) projections
  addChain ((leftId, left), (rightId, right)) edgesSoFar =
    (leftId, rightId, abs (otherCoordOf axis left - otherCoordOf axis right)) : edgesSoFar

undirectedAdjacency :: Int -> [(Int, Int, Int)] -> (Vector.Vector Int, Vector.Vector Int, Vector.Vector Int)
undirectedAdjacency size edges = runST $ do
  counts <- Mutable.replicate size 0
  forM_ edges $ \(a, b, _) -> Mutable.modify counts (+ 1) a >> Mutable.modify counts (+ 1) b
  frozenCounts <- Vector.freeze counts
  let offsets = Vector.scanl' (+) 0 frozenCounts
  cursors <- Vector.thaw offsets
  destinations <- Mutable.new (Vector.last offsets)
  weights <- Mutable.new (Vector.last offsets)
  forM_ edges $ \(a, b, cost) -> add cursors destinations weights a b cost >> add cursors destinations weights b a cost
  (offsets,,) <$> Vector.freeze destinations <*> Vector.freeze weights
 where
  add cursors destinations weights from to cost = do
    ix <- Mutable.read cursors from
    Mutable.write destinations ix to
    Mutable.write weights ix cost
    Mutable.write cursors from (ix + 1)
