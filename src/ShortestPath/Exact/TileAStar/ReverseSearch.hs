module ShortestPath.Exact.TileAStar.ReverseSearch
  ( assertReverseLabelsEqual
  , emptyReverseCounters
  , halveDistances
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

emptyReverseCounters :: TileReverseCounters
emptyReverseCounters = TileReverseCounters 0 0 0 0 0 0 0 0 0 0 0

reverseDijkstra :: SiteGraph -> [(Int, Int)] -> (Vector.Vector Int, TileReverseCounters)
reverseDijkstra graph seeds = runST $ do
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
                transportCounters <- Vector.foldM (relax result queue cost True) (poppedCounters {reverseStatesPopped = reverseStatesPopped poppedCounters + 1}) (siteReverseEdges graph Boxed.! node)
                scanCounters <- relaxSameComponent result queue cost node transportCounters
                searchReverse scanCounters
  searchReverse counters
 where
  siteCount = Vector.length (siteTiles graph)
  stateCount = siteCount * 2
  seed :: Mutable.MVector s Int -> MutableHeap s -> TileReverseCounters -> (Int, Int) -> ST s TileReverseCounters
  seed result queue counters (node, cost) = do
    Mutable.write result node cost
    heapPush queue cost node cost
    pure (pushed counters)
  relax :: Mutable.MVector s Int -> MutableHeap s -> Int -> Bool -> TileReverseCounters -> (Int, Int) -> ST s TileReverseCounters
  relax result queue cost transportEdge counters (next, edgeCost) =
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
  relaxSameComponent result queue cost node counters =
    go 0 counters'
     where
      site = node `div` 2
      banked = odd node
      sourceTile = siteTiles graph Vector.! site
      sameComponentSites = attachedSites graph site
      scanned = Vector.length sameComponentSites
      counters' = counters
        { reverseSameComponentSiteScans = reverseSameComponentSiteScans counters + 1
        , reverseTotalSitesScanned = reverseTotalSitesScanned counters + scanned
        , reverseMaxSitesScannedPerPop = max (reverseMaxSitesScannedPerPop counters) scanned
        }
      go ix countersSoFar
        | ix >= scanned = pure countersSoFar
        | other == site = go (ix + 1) countersSoFar
        | otherwise = do
            nextCounters <- relax result queue cost False scanCounters (stateId other banked, chebyshevPacked sourceTile otherTile)
            go (ix + 1) nextCounters
       where
        other = sameComponentSites Vector.! ix
        otherTile = siteTiles graph Vector.! other
        scanCounters = countersSoFar {reverseChebyshevComparisons = reverseChebyshevComparisons countersSoFar + 1}

reverseDijkstraUncounted :: SiteGraph -> [(Int, Int)] -> Vector.Vector Int
reverseDijkstraUncounted graph seeds = runST $ do
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
                Vector.forM_ (siteReverseEdges graph Boxed.! node) (relax result queue cost)
                relaxSameComponent result queue cost node
                searchReverse
  searchReverse
 where
  siteCount = Vector.length (siteTiles graph)
  stateCount = siteCount * 2
  seed :: Mutable.MVector s Int -> MutableHeap s -> (Int, Int) -> ST s ()
  seed result queue (node, cost) = do
    Mutable.write result node cost
    heapPush queue cost node cost
  relax :: Mutable.MVector s Int -> MutableHeap s -> Int -> (Int, Int) -> ST s ()
  relax result queue cost (next, edgeCost) =
    case addCost cost edgeCost of
      Nothing -> pure ()
      Just newCost -> do
        known <- Mutable.read result next
        when (newCost < known) $ do
          Mutable.write result next newCost
          heapPush queue newCost next newCost
  relaxSameComponent :: Mutable.MVector s Int -> MutableHeap s -> Int -> Int -> ST s ()
  relaxSameComponent result queue cost node =
    go 0
   where
    site = node `div` 2
    banked = odd node
    sourceTile = siteTiles graph Vector.! site
    sameComponentSites = attachedSites graph site
    scanned = Vector.length sameComponentSites
    go ix
      | ix >= scanned = pure ()
      | other == site = go (ix + 1)
      | otherwise = do
          relax result queue cost (stateId other banked, chebyshevPacked sourceTile otherTile)
          go (ix + 1)
     where
      other = sameComponentSites Vector.! ix
      otherTile = siteTiles graph Vector.! other

reverseDijkstraManhattan :: SiteGraph -> [(Int, Int)] -> (Vector.Vector Int, TileReverseCounters)
reverseDijkstraManhattan graph seeds = runST $ do
  result <- Mutable.replicate stateCount maxBound
  queue <- heapNew (max 262144 (stateCount * 16))
  counters <- Mutable.replicate reverseCounterCount 0
  forM_ seeds (seed result queue counters)
  let searchReverse = do
        popped <- heapPop queue
        case popped of
          Nothing -> do
            distances <- Vector.freeze result
            finalCounters <- readReverseCounters counters
            pure (distances, finalCounters)
          Just (_, node, cost) -> do
            reversePop counters
            known <- Mutable.read result node
            if cost /= known
              then bumpReverse counters reverseCounterStalePops 1 >> searchReverse
              else do
                bumpReverse counters reverseCounterStatesSettled 1
                relaxWalking result queue counters cost node
                when (node `div` 2 < siteCount) $
                  Vector.forM_ (siteReverseEdges graph Boxed.! node) (relax result queue counters cost True . doubleEdge)
                searchReverse
  searchReverse
 where
  siteCount = Vector.length (siteTiles graph)
  staticCount = siteStaticCount graph
  network = siteSparseNetwork graph
  stateCount = (siteCount + sparseSteinerCount network) * 2
  seed :: Mutable.MVector s Int -> MutableHeap s -> Mutable.MVector s Int -> (Int, Int) -> ST s ()
  seed result queue counters (node, cost) = do
    Mutable.write result node (cost * 2)
    heapPush queue (cost * 2) node (cost * 2)
    reversePush counters
  doubleEdge (next, edgeCost) = (next, edgeCost * 2)
  relax :: Mutable.MVector s Int -> MutableHeap s -> Mutable.MVector s Int -> Int -> Bool -> (Int, Int) -> ST s ()
  relax result queue counters cost transportEdge (next, edgeCost) = do
    bumpReverse counters reverseCounterEdgesRelaxed 1
    when transportEdge (bumpReverse counters reverseCounterTransportRelaxations 1)
    case addCost cost edgeCost of
      Nothing -> pure ()
      Just newCost -> do
        known <- Mutable.read result next
        when (newCost < known) $ do
            Mutable.write result next newCost
            heapPush queue newCost next newCost
            reversePush counters
  relaxWalking :: Mutable.MVector s Int -> MutableHeap s -> Mutable.MVector s Int -> Int -> Int -> ST s ()
  relaxWalking result queue counters cost state = do
    relaxSparseWalkingEdges result queue counters cost banked vertex
    when (vertex < siteCount) (relaxQueryAttachments result queue counters cost vertex banked)
   where
    vertex = state `div` 2
    banked = odd state
  relaxSparseWalkingEdges :: Mutable.MVector s Int -> MutableHeap s -> Mutable.MVector s Int -> Int -> Bool -> Int -> ST s ()
  relaxSparseWalkingEdges result queue counters cost banked vertex
    | vertex < staticCount = go (sparseOffsets network Vector.! vertex)
    | vertex < siteCount = pure ()
    | otherwise = go (sparseOffsets network Vector.! sparseVertex)
   where
    sparseVertex = staticCount + vertex - siteCount
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
                | otherwise = siteCount + sparseNext - staticCount
          relax result queue counters cost False (stateId next banked, edgeCost)
          go (ix + 1)
  relaxQueryAttachments :: Mutable.MVector s Int -> MutableHeap s -> Mutable.MVector s Int -> Int -> Int -> Bool -> ST s ()
  relaxQueryAttachments result queue counters cost vertex banked = do
    bumpReverse counters reverseCounterComponentScans 1
    bumpReverse counters reverseCounterSitesScanned count
    maxReverse counters reverseCounterMaxSitesPerPop count
    go 0
   where
    vertexTile = siteTiles graph Vector.! vertex
    componentSites = attachedSites graph vertex
    count = Vector.length componentSites
    go ix
      | ix >= count = pure ()
      | other == vertex = go (ix + 1)
      | vertex < staticCount && other < staticCount = go (ix + 1)
      | otherwise = do
          bumpReverse counters reverseCounterChebyshevComparisons 1
          relax result queue counters cost False (stateId other banked, chebyshevPacked vertexTile otherTile * 2)
          go (ix + 1)
     where
      other = componentSites Vector.! ix
      otherTile = siteTiles graph Vector.! other

reverseDijkstraManhattanUncounted :: SiteGraph -> [(Int, Int)] -> Vector.Vector Int
reverseDijkstraManhattanUncounted graph seeds = fst (reverseDijkstraManhattan graph seeds)

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

assertReverseLabelsEqual :: SiteGraph -> Vector.Vector Int -> Vector.Vector Int -> IO ()
assertReverseLabelsEqual graph clique manhattan =
  case [ (state, clique Vector.! state, manhattan Vector.! state)
       | node <- [0 .. Vector.length (siteTiles graph) - 1]
       , banked <- [False, True]
       , let state = stateId node banked
       , clique Vector.! state /= manhattan Vector.! state
       ] of
    [] -> pure ()
    ((state, expected, actual):_) ->
      fail ("sparse Manhattan reverse label mismatch at state " <> show state <> ": clique=" <> show expected <> " manhattan=" <> show actual)
