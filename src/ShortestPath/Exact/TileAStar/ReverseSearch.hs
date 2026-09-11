module ShortestPath.Exact.TileAStar.ReverseSearch
  ( assertReverseLabelsEqual
  , emptyReverseCounters
  , halveDistances
  , reverseDijkstra
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
emptyReverseCounters = TileReverseCounters 0 0 0 0 0 0 0 0 0

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
    pure counters {reversePqPushes = reversePqPushes counters + 1}
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
            pure counters' {reversePqPushes = reversePqPushes counters' + 1}
   where
    counters'
      | transportEdge = counters {reverseTransportRelaxations = reverseTransportRelaxations counters + 1}
      | otherwise = counters
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

reverseDijkstraManhattanUncounted :: SiteGraph -> [(Int, Int)] -> Vector.Vector Int
reverseDijkstraManhattanUncounted graph seeds = runST $ do
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
                relaxWalking result queue cost node
                when (node `div` 2 < siteCount) $
                  Vector.forM_ (siteReverseEdges graph Boxed.! node) (relax result queue cost . doubleEdge)
                searchReverse
  searchReverse
 where
  siteCount = Vector.length (siteTiles graph)
  staticCount = siteStaticCount graph
  network = siteSparseNetwork graph
  stateCount = (siteCount + sparseSteinerCount network) * 2
  seed :: Mutable.MVector s Int -> MutableHeap s -> (Int, Int) -> ST s ()
  seed result queue (node, cost) = do
    Mutable.write result node (cost * 2)
    heapPush queue (cost * 2) node (cost * 2)
  doubleEdge (next, edgeCost) = (next, edgeCost * 2)
  relax :: Mutable.MVector s Int -> MutableHeap s -> Int -> (Int, Int) -> ST s ()
  relax result queue cost (next, edgeCost) =
    case addCost cost edgeCost of
      Nothing -> pure ()
      Just newCost -> do
        known <- Mutable.read result next
        when (newCost < known) $ do
          Mutable.write result next newCost
          heapPush queue newCost next newCost
  relaxWalking :: Mutable.MVector s Int -> MutableHeap s -> Int -> Int -> ST s ()
  relaxWalking result queue cost state = do
    relaxSparseWalkingEdges result queue cost banked vertex
    when (vertex < siteCount) (relaxQueryAttachments result queue cost vertex banked)
   where
    vertex = state `div` 2
    banked = odd state
  relaxSparseWalkingEdges :: Mutable.MVector s Int -> MutableHeap s -> Int -> Bool -> Int -> ST s ()
  relaxSparseWalkingEdges result queue cost banked vertex
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
          relax result queue cost (stateId next banked, edgeCost)
          go (ix + 1)
  relaxQueryAttachments :: Mutable.MVector s Int -> MutableHeap s -> Int -> Int -> Bool -> ST s ()
  relaxQueryAttachments result queue cost vertex banked =
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
          relax result queue cost (stateId other banked, chebyshevPacked vertexTile otherTile * 2)
          go (ix + 1)
     where
      other = componentSites Vector.! ix
      otherTile = siteTiles graph Vector.! other

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


