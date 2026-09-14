module ShortestPath.Exact.TileAStar.ReverseSearch
  ( assertReverseLabelsEqual
  , emptyReverseCounters
  , halveDistances
  , ManhattanReverseResult(..)
  , reverseDijkstra
  , reverseDijkstraManhattan
  , reverseDijkstraManhattanUncounted
  , reverseDijkstraUncounted
  ) where

import Control.Monad (foldM, forM_, when)
import Control.Monad.ST (ST, runST)
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as Mutable

import ShortestPath.Exact.TileAStar.RelaxedGraph
import ShortestPath.Exact.TileAStar.SparseWalking
import ShortestPath.Exact.TileAStar.Types
import ShortestPath.Internal.MutableHeap

data ManhattanReverseResult = ManhattanReverseResult
  { manhattanDistances :: Vector.Vector Int
  , manhattanGeneratorOrigins :: Vector.Vector Int
  , manhattanGeneratorWeights :: Vector.Vector Int
  }

emptyReverseCounters :: TileReverseCounters
emptyReverseCounters = TileReverseCounters 0 0 0 0 0 0 0 0 0 0 0

reverseDijkstra :: SiteGraph -> TargetOverlay -> (Vector.Vector Int, TileReverseCounters)
reverseDijkstra graph overlay = runST $ do
  result <- Mutable.replicate stateCount maxBound
  queue <- heapNew (max 262144 (stateCount * 16))
  counters <- foldM (seed result queue) emptyReverseCounters seeds
  let searchReverse currentCounters = do
        popped <- heapPop queue
        case popped of
          Nothing -> do
            distances <- Vector.freeze result
            pure (distances, currentCounters)
          Just (_, node, cost) -> do
            let poppedCounters = currentCounters
                  { reversePqPops = reversePqPops currentCounters + 1
                  }
            known <- Mutable.read result node
            if cost /= known
              then searchReverse poppedCounters {reverseStalePqEntries = reverseStalePqEntries poppedCounters + 1}
              else do
                transportCounters <- if node `div` 2 < routingCount
                  then Vector.foldM (relax result queue cost True) (poppedCounters {reverseStatesPopped = reverseStatesPopped poppedCounters + 1}) (siteReverseEdges graph Boxed.! node)
                  else pure (poppedCounters {reverseStatesPopped = reverseStatesPopped poppedCounters + 1})
                scanCounters <- relaxSameComponent result queue cost node transportCounters
                searchReverse scanCounters
  searchReverse counters
 where
  siteCount = Vector.length (siteTiles graph)
  routingCount = routingNodeCount graph
  stateCount = queryRoutingNodeCount graph overlay * 2
  seeds = targetSeeds overlay
  seed :: Mutable.MVector s Int -> MutableHeap s -> TileReverseCounters -> (Int, Int) -> ST s TileReverseCounters
  seed result queue counters (node, cost) = do
    Mutable.write result node cost
    heapPush queue cost node cost
    pure (pushed counters)
  relax :: Mutable.MVector s Int -> MutableHeap s -> Int -> Bool -> TileReverseCounters -> ReverseRoutingEdge -> ST s TileReverseCounters
  relax result queue cost transportEdge counters (next, edgeCost, _) =
    case addCost cost edgeCost of
      Nothing -> pure counters'
      Just newCost -> do
        known <- Mutable.read result next
        if newCost >= known
          then pure counters'
          else do
            Mutable.write result next newCost
            heapPush queue newCost next newCost
            pure (pushed counters')
   where
    counters' = (if transportEdge
      then counters {reverseTransportRelaxations = reverseTransportRelaxations counters + 1}
      else counters) {reverseEdgesRelaxed = reverseEdgesRelaxed counters + 1}
  pushed counters = counters
    { reversePqPushes = reversePqPushes counters + 1
    , reversePqMaxSize = max (reversePqMaxSize counters) (reversePqPushes counters + 1 - reversePqPops counters)
    }
  relaxSameComponent :: Mutable.MVector s Int -> MutableHeap s -> Int -> Int -> TileReverseCounters -> ST s TileReverseCounters
  relaxSameComponent _ _ _ node counters
    | node `div` 2 >= siteCount && node `div` 2 /= targetSite overlay = pure counters
  relaxSameComponent result queue cost node counters =
    go 0 counters'
     where
      site = node `div` 2
      banked = odd node
      sourceTile = querySiteTile graph overlay site
      targetNode = targetSynthetic overlay && site == targetSite overlay
      targetAttachments = targetAttachmentSites overlay
      sameComponentSites
        | targetNode = Vector.empty
        | otherwise = attachedSites graph site
      scanned
        | targetNode = Vector.length targetAttachments
        | otherwise = Vector.length sameComponentSites
      counters' = counters
        { reverseSameComponentSiteScans = reverseSameComponentSiteScans counters + 1
        , reverseTotalSitesScanned = reverseTotalSitesScanned counters + scanned
        , reverseMaxSitesScannedPerPop = max (reverseMaxSitesScannedPerPop counters) scanned
        }
      go ix countersSoFar
        | ix >= scanned = relaxTarget countersSoFar
        | other == site = go (ix + 1) countersSoFar
        | otherwise = do
            nextCounters <- relax result queue cost False scanCounters (stateId other banked, edgeCost, False)
            go (ix + 1) nextCounters
       where
        (other, edgeCost)
          | targetNode = targetAttachments Vector.! ix
          | otherwise =
              let otherSite = sameComponentSites Vector.! ix
               in (otherSite, chebyshevPacked sourceTile (querySiteTile graph overlay otherSite))
        scanCounters = countersSoFar {reverseChebyshevComparisons = reverseChebyshevComparisons countersSoFar + 1}
      relaxTarget countersSoFar
        | site >= siteCount || not (targetAttachedToSite graph overlay site) = pure countersSoFar
        | otherwise = relax result queue cost False
            (countersSoFar {reverseChebyshevComparisons = reverseChebyshevComparisons countersSoFar + 1})
            (stateId (targetSite overlay) banked, chebyshevPacked sourceTile (targetPacked overlay), False)

reverseDijkstraUncounted :: SiteGraph -> TargetOverlay -> Vector.Vector Int
reverseDijkstraUncounted graph overlay = runST $ do
  result <- Mutable.replicate stateCount maxBound
  queue <- heapNew (max 262144 (stateCount * 16))
  forM_ seeds (seed result queue)
  let searchReverse = do
        popped <- heapPop queue
        case popped of
          Nothing -> Vector.freeze result
          Just (_, node, cost) -> do
            known <- Mutable.read result node
            if cost /= known
              then searchReverse
              else do
                when (node `div` 2 < routingCount) $
                  Vector.forM_ (siteReverseEdges graph Boxed.! node) (relax result queue cost)
                relaxSameComponent result queue cost node
                searchReverse
  searchReverse
 where
  siteCount = Vector.length (siteTiles graph)
  routingCount = routingNodeCount graph
  stateCount = queryRoutingNodeCount graph overlay * 2
  seeds = targetSeeds overlay
  seed :: Mutable.MVector s Int -> MutableHeap s -> (Int, Int) -> ST s ()
  seed result queue (node, cost) = do
    Mutable.write result node cost
    heapPush queue cost node cost
  relax :: Mutable.MVector s Int -> MutableHeap s -> Int -> ReverseRoutingEdge -> ST s ()
  relax result queue cost (next, edgeCost, _) =
    case addCost cost edgeCost of
      Nothing -> pure ()
      Just newCost -> do
        known <- Mutable.read result next
        when (newCost < known) $ do
          Mutable.write result next newCost
          heapPush queue newCost next newCost
  relaxSameComponent :: Mutable.MVector s Int -> MutableHeap s -> Int -> Int -> ST s ()
  relaxSameComponent _ _ _ node
    | node `div` 2 >= siteCount && node `div` 2 /= targetSite overlay = pure ()
  relaxSameComponent result queue cost node =
    go 0
   where
    site = node `div` 2
    banked = odd node
    sourceTile = querySiteTile graph overlay site
    targetNode = targetSynthetic overlay && site == targetSite overlay
    targetAttachments = targetAttachmentSites overlay
    sameComponentSites
      | targetNode = Vector.empty
      | otherwise = attachedSites graph site
    scanned
      | targetNode = Vector.length targetAttachments
      | otherwise = Vector.length sameComponentSites
    go ix
      | ix >= scanned = when (site < siteCount && targetAttachedToSite graph overlay site) $
          relax result queue cost (stateId (targetSite overlay) banked, chebyshevPacked sourceTile (targetPacked overlay), False)
      | other == site = go (ix + 1)
      | otherwise = do
          relax result queue cost (stateId other banked, edgeCost, False)
          go (ix + 1)
     where
      (other, edgeCost)
        | targetNode = targetAttachments Vector.! ix
        | otherwise =
            let otherSite = sameComponentSites Vector.! ix
             in (otherSite, chebyshevPacked sourceTile (querySiteTile graph overlay otherSite))

reverseDijkstraManhattan :: SiteGraph -> TargetOverlay -> (ManhattanReverseResult, TileReverseCounters)
reverseDijkstraManhattan graph overlay = runST $ do
  result <- Mutable.replicate stateCount maxBound
  generatorOrigins <- Mutable.replicate stateCount (-1)
  generatorWeights <- Mutable.replicate stateCount maxBound
  queue <- heapNew (max 262144 (stateCount * 16))
  counters <- Mutable.replicate reverseCounterCount 0
  forM_ seeds (seed result generatorOrigins generatorWeights queue counters)
  let searchReverse = do
        popped <- heapPop queue
        case popped of
          Nothing -> do
            distances <- Vector.freeze result
            origins <- Vector.freeze generatorOrigins
            weights <- Vector.freeze generatorWeights
            finalCounters <- readReverseCounters counters
            pure (ManhattanReverseResult distances origins weights, finalCounters)
          Just (_, node, cost) -> do
            reversePop counters
            known <- Mutable.read result node
            if cost /= known
              then bumpReverse counters reverseCounterStalePops 1 >> searchReverse
              else do
                bumpReverse counters reverseCounterStatesSettled 1
                relaxWalking result generatorOrigins generatorWeights queue counters cost node
                when (node `div` 2 < routingCount) $
                  -- Explicit routing edges are never walking edges; each edge
                  -- separately says whether it establishes fresh provenance.
                  Vector.forM_ (siteReverseEdges graph Boxed.! node)
                    (relax result generatorOrigins generatorWeights queue counters cost node True . doubleEdge)
                searchReverse
  searchReverse
 where
  siteCount = Vector.length (siteTiles graph)
  routingCount = routingNodeCount graph
  reverseNodeCount = queryRoutingNodeCount graph overlay
  staticCount = siteStaticCount graph
  network = siteSparseNetwork graph
  stateCount = (reverseNodeCount + sparseSteinerCount network) * 2
  seeds = targetSeeds overlay
  seed :: Mutable.MVector s Int -> Mutable.MVector s Int -> Mutable.MVector s Int -> MutableHeap s -> Mutable.MVector s Int -> (Int, Int) -> ST s ()
  seed result generatorOrigins generatorWeights queue counters (node, cost) = do
    Mutable.write result node (cost * 2)
    Mutable.write generatorOrigins node node
    Mutable.write generatorWeights node (cost * 2)
    heapPush queue (cost * 2) node (cost * 2)
    reversePush counters
  doubleEdge (next, edgeCost, startsGenerator) = (next, edgeCost * 2, startsGenerator)
  relax :: Mutable.MVector s Int -> Mutable.MVector s Int -> Mutable.MVector s Int -> MutableHeap s -> Mutable.MVector s Int -> Int -> Int -> Bool -> ReverseRoutingEdge -> ST s ()
  relax result generatorOrigins generatorWeights queue counters cost from externalEdge (next, edgeCost, startsGenerator) = do
    bumpReverse counters reverseCounterEdgesRelaxed 1
    when externalEdge (bumpReverse counters reverseCounterTransportRelaxations 1)
    case addCost cost edgeCost of
      Nothing -> pure ()
      Just newCost -> do
        known <- Mutable.read result next
        when (newCost < known) $ do
            Mutable.write result next newCost
            if startsGenerator
              then Mutable.write generatorOrigins next next >> Mutable.write generatorWeights next newCost
              else do
                -- Sparse/Steiner and query-attachment edges are exact relaxed
                -- walking edges, so they preserve the entering generator.
                origin <- Mutable.read generatorOrigins from
                weight <- Mutable.read generatorWeights from
                Mutable.write generatorOrigins next origin
                Mutable.write generatorWeights next weight
            heapPush queue newCost next newCost
            reversePush counters
  relaxWalking :: Mutable.MVector s Int -> Mutable.MVector s Int -> Mutable.MVector s Int -> MutableHeap s -> Mutable.MVector s Int -> Int -> Int -> ST s ()
  relaxWalking result generatorOrigins generatorWeights queue counters cost state = do
    relaxSparseWalkingEdges result generatorOrigins generatorWeights queue counters cost state banked vertex
    when (vertex < reverseNodeCount) (relaxQueryAttachments result generatorOrigins generatorWeights queue counters cost state vertex banked)
   where
    vertex = state `div` 2
    banked = odd state
  relaxSparseWalkingEdges :: Mutable.MVector s Int -> Mutable.MVector s Int -> Mutable.MVector s Int -> MutableHeap s -> Mutable.MVector s Int -> Int -> Int -> Bool -> Int -> ST s ()
  relaxSparseWalkingEdges result generatorOrigins generatorWeights queue counters cost state banked vertex
    | vertex < staticCount = go (sparseOffsets network Vector.! vertex)
    | vertex < reverseNodeCount = pure ()
    | otherwise = go (sparseOffsets network Vector.! sparseVertex)
   where
    sparseVertex = staticCount + vertex - reverseNodeCount
    end
      | vertex < staticCount = sparseOffsets network Vector.! (vertex + 1)
      | otherwise = sparseOffsets network Vector.! (sparseVertex + 1)
    go ix
      | ix >= end = pure ()
      | otherwise = do
          let sparseNext = sparseDestinations network Vector.! ix
              edgeCost = sparseWeights network Vector.! ix
              next
                | sparseNext < staticCount = sparseNext
                | otherwise = reverseNodeCount + sparseNext - staticCount
          relax result generatorOrigins generatorWeights queue counters cost state False (stateId next banked, edgeCost, False)
          go (ix + 1)
  relaxQueryAttachments :: Mutable.MVector s Int -> Mutable.MVector s Int -> Mutable.MVector s Int -> MutableHeap s -> Mutable.MVector s Int -> Int -> Int -> Int -> Bool -> ST s ()
  relaxQueryAttachments result generatorOrigins generatorWeights queue counters cost state vertex banked
    | targetSynthetic overlay && vertex == targetSite overlay = do
        bumpReverse counters reverseCounterComponentScans 1
        bumpReverse counters reverseCounterSitesScanned count
        maxReverse counters reverseCounterMaxSitesPerPop count
        go 0
    | vertex < siteCount && targetAttachedToSite graph overlay vertex = do
        bumpReverse counters reverseCounterChebyshevComparisons 1
        relax result generatorOrigins generatorWeights queue counters cost state False
          (stateId (targetSite overlay) banked, chebyshevPacked vertexTile (targetPacked overlay) * 2, False)
    | otherwise = pure ()
   where
    vertexTile = querySiteTile graph overlay vertex
    attachments = targetAttachmentSites overlay
    count = Vector.length attachments
    go ix
      | ix >= count = pure ()
      | otherwise = do
          bumpReverse counters reverseCounterChebyshevComparisons 1
          relax result generatorOrigins generatorWeights queue counters cost state False
            (stateId other banked, edgeCost * 2, False)
          go (ix + 1)
     where
      (other, edgeCost) = attachments Vector.! ix

reverseDijkstraManhattanUncounted :: SiteGraph -> TargetOverlay -> ManhattanReverseResult
reverseDijkstraManhattanUncounted graph overlay = runST $ do
  result <- Mutable.replicate stateCount maxBound
  generatorOrigins <- Mutable.replicate stateCount (-1)
  generatorWeights <- Mutable.replicate stateCount maxBound
  queue <- heapNew (max 262144 (stateCount * 16))
  forM_ seeds (seed result generatorOrigins generatorWeights queue)
  let searchReverse = do
        popped <- heapPop queue
        case popped of
          Nothing -> do
            distances <- Vector.freeze result
            origins <- Vector.freeze generatorOrigins
            weights <- Vector.freeze generatorWeights
            pure (ManhattanReverseResult distances origins weights)
          Just (_, node, cost) -> do
            known <- Mutable.read result node
            if cost /= known
              then searchReverse
              else do
                relaxWalking result generatorOrigins generatorWeights queue cost node
                when (node `div` 2 < routingCount) $
                  Vector.forM_ (siteReverseEdges graph Boxed.! node)
                    (relax result generatorOrigins generatorWeights queue cost node True . doubleEdge)
                searchReverse
  searchReverse
 where
  siteCount = Vector.length (siteTiles graph)
  routingCount = routingNodeCount graph
  reverseNodeCount = queryRoutingNodeCount graph overlay
  staticCount = siteStaticCount graph
  network = siteSparseNetwork graph
  stateCount = (reverseNodeCount + sparseSteinerCount network) * 2
  seeds = targetSeeds overlay
  seed :: Mutable.MVector s Int -> Mutable.MVector s Int -> Mutable.MVector s Int -> MutableHeap s -> (Int, Int) -> ST s ()
  seed result generatorOrigins generatorWeights queue (node, cost) = do
    Mutable.write result node (cost * 2)
    Mutable.write generatorOrigins node node
    Mutable.write generatorWeights node (cost * 2)
    heapPush queue (cost * 2) node (cost * 2)
  doubleEdge (next, edgeCost, startsGenerator) = (next, edgeCost * 2, startsGenerator)
  relax :: Mutable.MVector s Int -> Mutable.MVector s Int -> Mutable.MVector s Int -> MutableHeap s -> Int -> Int -> Bool -> ReverseRoutingEdge -> ST s ()
  relax result generatorOrigins generatorWeights queue cost from _externalEdge (next, edgeCost, startsGenerator) =
    case addCost cost edgeCost of
      Nothing -> pure ()
      Just newCost -> do
        known <- Mutable.read result next
        when (newCost < known) $ do
          Mutable.write result next newCost
          if startsGenerator
            then Mutable.write generatorOrigins next next >> Mutable.write generatorWeights next newCost
            else do
              origin <- Mutable.read generatorOrigins from
              weight <- Mutable.read generatorWeights from
              Mutable.write generatorOrigins next origin
              Mutable.write generatorWeights next weight
          heapPush queue newCost next newCost
  relaxWalking :: Mutable.MVector s Int -> Mutable.MVector s Int -> Mutable.MVector s Int -> MutableHeap s -> Int -> Int -> ST s ()
  relaxWalking result generatorOrigins generatorWeights queue cost state = do
    relaxSparseWalkingEdges result generatorOrigins generatorWeights queue cost state banked vertex
    when (vertex < reverseNodeCount) (relaxQueryAttachments result generatorOrigins generatorWeights queue cost state vertex banked)
   where
    vertex = state `div` 2
    banked = odd state
  relaxSparseWalkingEdges :: Mutable.MVector s Int -> Mutable.MVector s Int -> Mutable.MVector s Int -> MutableHeap s -> Int -> Int -> Bool -> Int -> ST s ()
  relaxSparseWalkingEdges result generatorOrigins generatorWeights queue cost state banked vertex
    | vertex < staticCount = go (sparseOffsets network Vector.! vertex)
    | vertex < reverseNodeCount = pure ()
    | otherwise = go (sparseOffsets network Vector.! sparseVertex)
   where
    sparseVertex = staticCount + vertex - reverseNodeCount
    end
      | vertex < staticCount = sparseOffsets network Vector.! (vertex + 1)
      | otherwise = sparseOffsets network Vector.! (sparseVertex + 1)
    go ix
      | ix >= end = pure ()
      | otherwise = do
          let sparseNext = sparseDestinations network Vector.! ix
              edgeCost = sparseWeights network Vector.! ix
              next
                | sparseNext < staticCount = sparseNext
                | otherwise = reverseNodeCount + sparseNext - staticCount
          relax result generatorOrigins generatorWeights queue cost state False (stateId next banked, edgeCost, False)
          go (ix + 1)
  relaxQueryAttachments :: Mutable.MVector s Int -> Mutable.MVector s Int -> Mutable.MVector s Int -> MutableHeap s -> Int -> Int -> Int -> Bool -> ST s ()
  relaxQueryAttachments result generatorOrigins generatorWeights queue cost state vertex banked
    | targetSynthetic overlay && vertex == targetSite overlay = go 0
    | vertex < siteCount && targetAttachedToSite graph overlay vertex =
        relax result generatorOrigins generatorWeights queue cost state False
          (stateId (targetSite overlay) banked, chebyshevPacked vertexTile (targetPacked overlay) * 2, False)
    | otherwise = pure ()
   where
    vertexTile = querySiteTile graph overlay vertex
    attachments = targetAttachmentSites overlay
    count = Vector.length attachments
    go ix
      | ix >= count = pure ()
      | otherwise = do
          relax result generatorOrigins generatorWeights queue cost state False
            (stateId other banked, edgeCost * 2, False)
          go (ix + 1)
     where
      (other, edgeCost) = attachments Vector.! ix

queryRoutingNodeCount :: SiteGraph -> TargetOverlay -> Int
queryRoutingNodeCount graph overlay = routingNodeCount graph + if targetSynthetic overlay then 1 else 0

querySiteTile :: SiteGraph -> TargetOverlay -> Int -> Int
querySiteTile graph overlay site
  | targetSynthetic overlay && site == targetSite overlay = targetPacked overlay
  | otherwise = siteTiles graph Vector.! site

targetAttachedToSite :: SiteGraph -> TargetOverlay -> Int -> Bool
targetAttachedToSite graph overlay site = targetSynthetic overlay &&
  Vector.any (`Vector.elem` targetComponents overlay) (siteComponents graph Boxed.! site)

reverseCounterStatesSettled, reverseCounterStalePops, reverseCounterPushes, reverseCounterPops,
  reverseCounterMaxSize, reverseCounterEdgesRelaxed, reverseCounterComponentScans,
  reverseCounterSitesScanned, reverseCounterChebyshevComparisons, reverseCounterMaxSitesPerPop,
  reverseCounterTransportRelaxations, reverseCounterQueueSize, reverseCounterCount :: Int
reverseCounterStatesSettled = 0
reverseCounterStalePops = 1
reverseCounterPushes = 2
reverseCounterPops = 3
reverseCounterMaxSize = 4
reverseCounterEdgesRelaxed = 5
reverseCounterComponentScans = 6
reverseCounterSitesScanned = 7
reverseCounterChebyshevComparisons = 8
reverseCounterMaxSitesPerPop = 9
reverseCounterTransportRelaxations = 10
reverseCounterQueueSize = 11
reverseCounterCount = 12

bumpReverse :: Mutable.MVector s Int -> Int -> Int -> ST s ()
{-# INLINE bumpReverse #-}
bumpReverse counters index amount = when (amount /= 0) $ do
  current <- Mutable.read counters index
  Mutable.write counters index (current + amount)

maxReverse :: Mutable.MVector s Int -> Int -> Int -> ST s ()
{-# INLINE maxReverse #-}
maxReverse counters index value = do
  current <- Mutable.read counters index
  when (value > current) (Mutable.write counters index value)

reversePush :: Mutable.MVector s Int -> ST s ()
{-# INLINE reversePush #-}
reversePush counters = do
  bumpReverse counters reverseCounterPushes 1
  bumpReverse counters reverseCounterQueueSize 1
  size <- Mutable.read counters reverseCounterQueueSize
  maxReverse counters reverseCounterMaxSize size

reversePop :: Mutable.MVector s Int -> ST s ()
{-# INLINE reversePop #-}
reversePop counters = do
  bumpReverse counters reverseCounterPops 1
  bumpReverse counters reverseCounterQueueSize (-1)

readReverseCounters :: Mutable.MVector s Int -> ST s TileReverseCounters
readReverseCounters counters =
  TileReverseCounters
    <$> Mutable.read counters reverseCounterStatesSettled
    <*> Mutable.read counters reverseCounterStalePops
    <*> Mutable.read counters reverseCounterPushes
    <*> Mutable.read counters reverseCounterPops
    <*> Mutable.read counters reverseCounterMaxSize
    <*> Mutable.read counters reverseCounterEdgesRelaxed
    <*> Mutable.read counters reverseCounterComponentScans
    <*> Mutable.read counters reverseCounterSitesScanned
    <*> Mutable.read counters reverseCounterChebyshevComparisons
    <*> Mutable.read counters reverseCounterMaxSitesPerPop
    <*> Mutable.read counters reverseCounterTransportRelaxations

halveDistances :: Vector.Vector Int -> Vector.Vector Int
halveDistances = Vector.map halve
 where
  halve value
    | value == maxBound = maxBound
    | otherwise = value `div` 2

assertReverseLabelsEqual :: SiteGraph -> TargetOverlay -> Vector.Vector Int -> Vector.Vector Int -> IO ()
assertReverseLabelsEqual graph overlay clique manhattan =
  case [ (state, clique Vector.! state, manhattan Vector.! state)
       | node <- [0 .. queryRoutingNodeCount graph overlay - 1]
       , banked <- [False, True]
       , let state = stateId node banked
       , clique Vector.! state /= manhattan Vector.! state
       ] of
    [] -> pure ()
    ((state, expected, actual):_) ->
      fail ("sparse Manhattan reverse label mismatch at state " <> show state <> ": clique=" <> show expected <> " manhattan=" <> show actual)
