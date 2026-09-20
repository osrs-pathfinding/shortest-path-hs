module ShortestPath.Exact.TileAStar.SparseWalking
  ( SparseWalkingNetwork(..)
  , buildSparseWalkingNetwork
  , buildSparseWalkingNetworkComponents
  , foldGatewayAttachments
  , sparseWalkingDistance
  , sparseWalkingDistanceFromQuery
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Binary (Binary(..))
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Word (Word8)
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
  , sparseComponentRoots :: Vector.Vector Int
  , sparseAttachmentNodeKinds :: Vector.Vector Word8
  , sparseAttachmentSplitCoords :: Vector.Vector Int
  , sparseAttachmentChainOffsets :: Vector.Vector Int
  , sparseAttachmentChainLengths :: Vector.Vector Int
  , sparseAttachmentLeftChildren :: Vector.Vector Int
  , sparseAttachmentRightChildren :: Vector.Vector Int
  , sparseAttachmentLeafOriginals :: Vector.Vector Int
  , sparseAttachmentChainCoords :: Vector.Vector Int
  , sparseAttachmentChainVertices :: Vector.Vector Int
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
    put (Vector.toList (sparseComponentRoots value))
    put (Vector.toList (sparseAttachmentNodeKinds value))
    put (Vector.toList (sparseAttachmentSplitCoords value))
    put (Vector.toList (sparseAttachmentChainOffsets value))
    put (Vector.toList (sparseAttachmentChainLengths value))
    put (Vector.toList (sparseAttachmentLeftChildren value))
    put (Vector.toList (sparseAttachmentRightChildren value))
    put (Vector.toList (sparseAttachmentLeafOriginals value))
    put (Vector.toList (sparseAttachmentChainCoords value))
    put (Vector.toList (sparseAttachmentChainVertices value))
  get =
    SparseWalkingNetwork
      <$> get
      <*> get
      <*> get
      <*> get
      <*> (Vector.fromList <$> get)
      <*> (Vector.fromList <$> get)
      <*> (Vector.fromList <$> get)
      <*> (Vector.fromList <$> get)
      <*> (Vector.fromList <$> get)
      <*> (Vector.fromList <$> get)
      <*> (Vector.fromList <$> get)
      <*> (Vector.fromList <$> get)
      <*> (Vector.fromList <$> get)
      <*> (Vector.fromList <$> get)
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
    roots kinds splits chainOffsets chainLengths lefts rights leaves chainCoords chainVertices
 where
  points = map toPoint sites
  originalCount = if null sites then 0 else maximum (map fst sites) + 1
  (nextVertex, edges, tree) = buildManhattanComponent originalCount points
  vertexCount = nextVertex
  steinerCount = vertexCount - originalCount
  edgeCount = length edges
  (offsets, destinations, weights) = undirectedAdjacency vertexCount edges
  (roots, kinds, splits, chainOffsets, chainLengths, lefts, rights, leaves, chainCoords, chainVertices) =
    flattenAttachmentTrees 1 [(0, tree)]
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

-- | Fold the one or two projection neighbours at each separator followed by
-- the terminal leaf. Costs use transformed (doubled Chebyshev) units.
foldGatewayAttachments :: SparseWalkingNetwork -> Int -> Tile -> a -> (a -> Int -> Int -> a) -> a
foldGatewayAttachments network component tile initial step
  | component < 0 || component >= Vector.length (sparseComponentRoots network) = initial
  | root < 0 = initial
  | otherwise = go root initial
 where
  root = sparseComponentRoots network Vector.! component
  (x, y, _) = unpackTile tile
  qa = x + y
  qb = x - y
  go node acc = case sparseAttachmentNodeKinds network Vector.! node of
    0 -> step acc (sparseAttachmentLeafOriginals network Vector.! node)
      (abs (qa - sparseAttachmentSplitCoords network Vector.! node)
        + abs (qb - sparseAttachmentChainOffsets network Vector.! node))
    kind -> go child (emitSuccessor insertion (emitPredecessor insertion acc))
     where
      split = sparseAttachmentSplitCoords network Vector.! node
      queryOther = if kind == 1 then qb else qa
      queryAxis = if kind == 1 then qa else qb
      offset = sparseAttachmentChainOffsets network Vector.! node
      count = sparseAttachmentChainLengths network Vector.! node
      insertion = lowerBound offset (offset + count) queryOther
      child
        | queryAxis <= split = sparseAttachmentLeftChildren network Vector.! node
        | otherwise = sparseAttachmentRightChildren network Vector.! node
      emitPredecessor ix value
        | ix <= offset = value
        | otherwise = emit (ix - 1) value
      emitSuccessor ix value
        | ix >= offset + count = value
        | otherwise = emit ix value
      emit ix value = step value (sparseAttachmentChainVertices network Vector.! ix)
        (abs (queryAxis - split) + abs (queryOther - sparseAttachmentChainCoords network Vector.! ix))
  lowerBound lo hi needle
    | lo >= hi = lo
    | sparseAttachmentChainCoords network Vector.! mid < needle = lowerBound (mid + 1) hi needle
    | otherwise = lowerBound lo mid needle
   where
    mid = (lo + hi) `div` 2

sparseWalkingDistanceFromQuery :: SparseWalkingNetwork -> Int -> Tile -> Int -> Maybe Int
sparseWalkingDistanceFromQuery network component query target
  | target < 0 || target >= sparseOriginalCount network = Nothing
  | otherwise = finiteDistance (dijkstra Vector.! target)
 where
  finiteDistance value | value == maxBound = Nothing
                       | otherwise = Just value
  dijkstra = runST $ do
    result <- Mutable.replicate (sparseVertexCount network) maxBound
    queue <- heapNew (max 1 (sparseWalkingEdgeCount network * 4 + 1))
    let seed action vertex cost = action >> do
          old <- Mutable.read result vertex
          when (cost < old) (Mutable.write result vertex cost >> heapPush queue cost vertex cost)
    foldGatewayAttachments network component query (pure ()) seed
    let relax cost node = go (sparseOffsets network Vector.! node)
         where
          end = sparseOffsets network Vector.! (node + 1)
          go ix
            | ix >= end = pure ()
            | otherwise = do
                let next = sparseDestinations network Vector.! ix
                case addCost cost (sparseWeights network Vector.! ix) of
                  Nothing -> pure ()
                  Just newCost -> do
                    old <- Mutable.read result next
                    when (newCost < old) (Mutable.write result next newCost >> heapPush queue newCost next newCost)
                go (ix + 1)
        search = heapPop queue >>= \case
          Nothing -> Vector.freeze result
          Just (_, node, cost) -> do
            known <- Mutable.read result node
            when (cost == known) (relax cost node)
            search
    search

buildSparseWalkingNetworkComponents :: Int -> [(Int, Int, Tile)] -> SparseWalkingNetwork
buildSparseWalkingNetworkComponents originalCount sites =
  SparseWalkingNetwork originalCount vertexCount steinerCount edgeCount offsets destinations weights
    roots kinds splits chainOffsets chainLengths lefts rights leaves chainCoords chainVertices
 where
  grouped = Map.toAscList (Map.fromListWith (<>) [(cid, [(siteId, tile)]) | (cid, siteId, tile) <- sites])
  (vertexCount, edges, trees) = foldl' addComponent (originalCount, [], []) grouped
  steinerCount = vertexCount - originalCount
  edgeCount = length edges
  (offsets, destinations, weights) = undirectedAdjacency vertexCount edges
  componentCount = if null sites then 0 else maximum [cid | (cid, _, _) <- sites] + 1
  (roots, kinds, splits, chainOffsets, chainLengths, lefts, rights, leaves, chainCoords, chainVertices) =
    flattenAttachmentTrees componentCount (reverse trees)
  addComponent (next, allEdges, allTrees) (cid, componentSites) =
    let points = map toPoint componentSites
        (next', componentEdges, tree) = buildManhattanComponent next points
     in (next', componentEdges <> allEdges, (cid, tree) : allTrees)
  toPoint (siteId, tile) =
    let (x, y, _) = unpackTile tile
     in ManhattanPoint siteId (x + y) (x - y)

data SplitAxis = SplitA | SplitB

data AttachmentTree
  = AttachmentLeaf !ManhattanPoint
  | AttachmentSplit !SplitAxis !Int ![(Int, Int)] AttachmentTree AttachmentTree

data FlatAttachmentNode = FlatAttachmentNode !Word8 !Int !Int !Int !Int !Int !Int

buildManhattanComponent :: Int -> [ManhattanPoint] -> (Int, [(Int, Int, Int)], Maybe AttachmentTree)
buildManhattanComponent firstSteiner points = go firstSteiner (sortPoints points) []
 where
  sortPoints = sortBy (comparing pointA <> comparing pointB <> comparing pointOriginal)
  go next [] edges = (next, edges, Nothing)
  go next [p] edges = (next, edges, Just (AttachmentLeaf p))
  go next pts@(firstPoint:_:_) edges
    | allSameA && allSameB = (next, zeroChain pts edges, Just (AttachmentLeaf firstPoint))
    | allSameA = splitOn SplitB next pts edges
    | otherwise = splitOn SplitA next pts edges
   where
    allSameA = all ((== pointA firstPoint) . pointA) pts
    allSameB = all ((== pointB firstPoint) . pointB) pts
  splitOn axis next pts edges =
    let sorted = sortBy (comparing (coordOf axis) <> comparing pointA <> comparing pointB <> comparing pointOriginal) pts
        (left, right, m) = balancedGapSplit (coordOf axis) sorted
        (next', edges', chainEntries) = addProjectionLevel axis m next pts edges
        (next'', edges'', leftTree) = go next' left edges'
        (next''', edges''', rightTree) = go next'' right edges''
     in (next''', edges''', AttachmentSplit axis m chainEntries <$> leftTree <*> rightTree)
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

addProjectionLevel :: SplitAxis -> Int -> Int -> [ManhattanPoint] -> [(Int, Int, Int)] -> (Int, [(Int, Int, Int)], [(Int, Int)])
addProjectionLevel axis m firstProjection pts edges =
  (firstProjection + length projections, chain sortedProjections (spokes <> edges), map entry sortedProjections)
 where
  projections = zip [firstProjection ..] pts
  spokes = [(pointOriginal p, steiner, abs (coordOf axis p - m)) | (steiner, p) <- projections]
  chain sorted edgesSoFar = foldr addChain edgesSoFar (zip sorted (drop 1 sorted))
  sortedProjections = sortBy (comparing (otherCoordOf axis . snd) <> comparing (pointOriginal . snd)) projections
  addChain ((leftId, left), (rightId, right)) edgesSoFar =
    (leftId, rightId, abs (otherCoordOf axis left - otherCoordOf axis right)) : edgesSoFar
  entry (vertex, point) = (otherCoordOf axis point, vertex)

flattenAttachmentTrees :: Int -> [(Int, Maybe AttachmentTree)]
  -> (Vector.Vector Int, Vector.Vector Word8, Vector.Vector Int, Vector.Vector Int, Vector.Vector Int,
      Vector.Vector Int, Vector.Vector Int, Vector.Vector Int, Vector.Vector Int, Vector.Vector Int)
flattenAttachmentTrees componentCount trees =
  ( Vector.generate componentCount (\cid -> Map.findWithDefault (-1) cid rootMap)
  , Vector.fromList [kind | FlatAttachmentNode kind _ _ _ _ _ _ <- nodes]
  , Vector.fromList [split | FlatAttachmentNode _ split _ _ _ _ _ <- nodes]
  , Vector.fromList [offset | FlatAttachmentNode _ _ offset _ _ _ _ <- nodes]
  , Vector.fromList [count | FlatAttachmentNode _ _ _ count _ _ _ <- nodes]
  , Vector.fromList [left | FlatAttachmentNode _ _ _ _ left _ _ <- nodes]
  , Vector.fromList [right | FlatAttachmentNode _ _ _ _ _ right _ <- nodes]
  , Vector.fromList [leaf | FlatAttachmentNode _ _ _ _ _ _ leaf <- nodes]
  , Vector.fromList (map fst chains)
  , Vector.fromList (map snd chains)
  )
 where
  (rootMap, nodes, chains) = foldl' addTree (Map.empty, [], []) trees
  addTree acc (_, Nothing) = acc
  addTree (roots, priorNodes, priorChains) (cid, Just tree) =
    let nodeBase = length priorNodes
        chainBase = length priorChains
        (newNodes, newChains) = flattenTree nodeBase chainBase tree
     in (Map.insert cid nodeBase roots, priorNodes <> newNodes, priorChains <> newChains)
  flattenTree _ _ (AttachmentLeaf point) =
    ([FlatAttachmentNode 0 (pointA point) (pointB point) 0 (-1) (-1) (pointOriginal point)], [])
  flattenTree nodeBase chainBase (AttachmentSplit axis split chain left right) =
    (FlatAttachmentNode kind split chainBase (length chain) leftRoot rightRoot (-1) : leftNodes <> rightNodes,
      chain <> leftChains <> rightChains)
   where
    kind = case axis of SplitA -> 1; SplitB -> 2
    leftRoot = nodeBase + 1
    (leftNodes, leftChains) = flattenTree leftRoot (chainBase + length chain) left
    rightRoot = leftRoot + length leftNodes
    (rightNodes, rightChains) = flattenTree rightRoot (chainBase + length chain + length leftChains) right

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
