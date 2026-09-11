{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -ddump-stg-final #-}

module ShortestPath.Exact.TileAStar.Search
  ( search
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Bits (testBit)
import Data.Int (Int32)
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Word (Word8)
import qualified Data.Vector as Boxed
import qualified Data.Vector.Mutable as BoxedMutable
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as Mutable
import GHC.Exts (Int(I#), Int#)

import ShortestPath.Exact.TileAStar.Heuristic
import ShortestPath.Exact.TileAStar.RelaxedGraph
import ShortestPath.Exact.TileAStar.Reconstruct
import ShortestPath.Exact.TileAStar.Types
import ShortestPath.Internal.MutableHeap
import ShortestPath.Pathfinder
import ShortestPath.Tile
import ShortestPath.Topology
import ShortestPath.Transport
import ShortestPath.World

data SearchSpace = SearchSpace
  { searchBaseTiles :: Vector.Vector Int
  , searchBaseComponents :: Vector.Vector Int
  , searchBaseWalkingMasks :: Vector.Vector Word8
  , searchBaseNorthNodes :: Vector.Vector Int32
  , searchBaseSouthNodes :: Vector.Vector Int32
  , searchExtraTiles :: Vector.Vector Int
  , searchExtraComponents :: Boxed.Vector (Vector.Vector Int)
  , searchExtraSites :: Vector.Vector Int
  , searchSize :: !Int
  }

search :: Bool -> TileAStar -> CompiledRoutingAccount -> PreparedTarget -> Tile -> SearchOptions -> (Route, TileAStarCounters, [(Tile, Bool)])
search trace astar@(TileAStar topology static) account prepared start options = runST $ do
  best <- Mutable.replicate stateCount maxBound
  prevState <- Mutable.replicate stateCount maxBound
  prevKind <- Mutable.replicate stateCount 0
  prevLabel <- BoxedMutable.replicate stateCount ""
  exploredRef <- newSTRef []
  bestBankRef <- newSTRef maxBound
  bankTraceRef <- newSTRef []
  counters <- Mutable.replicate counterCount 0
  queue <- heapNew (min stateCount 262144)
  forM_ initialStates $ \(state, cost, kind, label) -> do
    known <- Mutable.read best state
    when (cost < known) $ do
      updateBestBank counters bestBankRef state cost
      bestBank <- readSTRef bestBankRef
      case effectiveHeuristicRaw bestBank cost state of
        (# h#, dominated# #)
          | I# h# == maxBound -> countHeuristicPrune counters state
          | otherwise -> do
              Mutable.write best state cost
              when (kind >= 0) $ do
                Mutable.write prevState state startState
                Mutable.write prevKind state kind
                BoxedMutable.write prevLabel state label
              heapPush queue (addCostDefault maxBound cost (weightedHeuristic (I# h#))) state cost
              bump counters counterPqPushes 1
              bump counters counterUniqueStates (if known == maxBound then 1 else 0)
              bump counters counterHeuristicEvaluations 1
              bump counters counterBankDominated (I# dominated#)
  go exploredRef bankTraceRef counters best prevState prevKind prevLabel queue bestBankRef
 where
  space = searchSpace astar start prepared
  stateCount = searchSize space * 2
  startNode = max 0 (nodeForPacked (unTile start))
  startState = stateId startNode False
  target = preparedTargetTile prepared
  heuristic = preparedTargetHeuristic prepared
  availability = compiledTransportAvailability account
  reachableBanks = staticReachableBanks static
  bankGlobalRelevant = compiledAllowTransports account && compiledBankPathEnabled account
  isBankCandidate state = bankGlobalRelevant && not (stateBanked state) && Set.member (stateTile state) reachableBanks
  initialStates = (startState, 0, -1, "") :
    [ (next, transportCost t, 1, transportLabel t)
    | compiledAllowTransports account
    , t <- preparedGlobalTransports availability False
    , Just dst <- [destination t]
    , let next = stateForPacked (unTile dst) False
    , next >= 0
    ]

  go ::
    STRef s [(Tile, Bool)] ->
    STRef s [TileBankGlobalObservation] ->
    Mutable.MVector s Int ->
    Mutable.MVector s Int ->
    Mutable.MVector s Int ->
    Mutable.MVector s Int ->
    BoxedMutable.MVector s String ->
    MutableHeap s ->
    STRef s Int ->
    ST s (Route, TileAStarCounters, [(Tile, Bool)])
  go exploredRef bankTraceRef counters best prevState prevKind prevLabel queue bestBankRef = do
    popped <- heapPop queue
    case popped of
      Nothing -> do
        explored <- reverse <$> readSTRef exploredRef
        finalCounters <- readCounters counters bestBankRef bankTraceRef
        pure (Route maxBound (tileStatesPopped finalCounters) [], finalCounters, explored)
      Just (priority, state, cost) -> do
        known <- Mutable.read best state
        if cost /= known
          then bump counters counterStalePqEntries 1 >> go exploredRef bankTraceRef counters best prevState prevKind prevLabel queue bestBankRef
          else do
            bestBank <- readSTRef bestBankRef
            case effectiveHeuristicRaw bestBank cost state of
              (# h#, dominated# #)
                | I# h# == maxBound -> countHeuristicPrune counters state >> go exploredRef bankTraceRef counters best prevState prevKind prevLabel queue bestBankRef
                | addCostDefault maxBound cost (weightedHeuristic (I# h#)) > priority -> do
                    heapPush queue (addCostDefault maxBound cost (weightedHeuristic (I# h#))) state cost
                    bump counters counterBankBoundPQRekeys 1
                    bump counters counterBankDominated (I# dominated#)
                    go exploredRef bankTraceRef counters best prevState prevKind prevLabel queue bestBankRef
                | otherwise -> do
                    let tile = stateTile state
                    when trace $ do
                      explored <- readSTRef exploredRef
                      writeSTRef exploredRef ((tile, stateBanked state) : explored)
                    if tile == target
                      then do
                        steps <- reconstructRouteSteps stateTile prevState prevKind prevLabel state
                        explored <- reverse <$> readSTRef exploredRef
                        finalCounters <- readCounters counters bestBankRef bankTraceRef
                        pure (Route cost (tileStatesPopped finalCounters) steps, finalCounters, explored)
                      else do
                        currentBestBank <- readSTRef bestBankRef
                        let banked = stateBanked state
                            globals = preparedGlobalTransports availability True
                            suppressed = if compiledAllowTransports account && compiledBankTransitionAvailable banked tile then countDestinations True globals else 0
                        bump counters counterStatesPopped 1
                        bump counters counterBankGlobalSuppressed suppressed
                        when (compiledAllowTransports account && Set.member tile reachableBanks) $ do
                          observations <- readSTRef bankTraceRef
                          writeSTRef bankTraceRef (bankObservation currentBestBank cost state : observations)
                        relaxNeighbors counters best prevState prevKind prevLabel queue bestBankRef currentBestBank cost state
                        go exploredRef bankTraceRef counters best prevState prevKind prevLabel queue bestBankRef

  relaxNeighbors ::
    Mutable.MVector s Int -> Mutable.MVector s Int -> Mutable.MVector s Int -> Mutable.MVector s Int ->
    BoxedMutable.MVector s String -> MutableHeap s -> STRef s Int -> Int -> Int -> Int -> ST s ()
  relaxNeighbors counters best prevState prevKind prevLabel queue bestBankRef bestBank cost state = do
    if node < baseLength
      then do
        walkNodes node
          (searchBaseWalkingMasks space Vector.! node)
          (searchBaseNorthNodes space Vector.! node)
          (searchBaseSouthNodes space Vector.! node)
          relaxNode
        forM_ cardinalNeighbors $ \nextTile ->
          when (usableOrigin banked nextTile && not (isWalkable (worldCollision world) nextTile)) (relaxWalk nextTile)
      else forM_ (walkingNeighborsRaw world tile) $ \nextTile ->
        when (isWalkable (worldCollision world) nextTile || usableOrigin banked nextTile) (relaxWalk nextTile)
    when (compiledBankTransitionAvailable banked tile) $
      relaxEdge counters best prevState prevKind prevLabel queue bestBankRef cost state (state + 1) 0 "" transportEdge
    when (compiledAllowTransports account) $
      forM_ (localTransports banked tile) $ \transport ->
        when (transportType transport /= "VIRTUAL_WALL") (relaxTransport banked transport)
    when (compiledAllowTransports account && compiledBankTransitionAvailable banked tile && not dominatedBankGlobal) $
      forM_ (preparedGlobalTransports availability True) (relaxTransport True)
   where
    tile = stateTile state
    node = state `div` 2
    baseLength = Vector.length (searchBaseTiles space)
    banked = stateBanked state
    cardinalNeighbors =
      [ packTile (x - 1) y p
      , packTile (x + 1) y p
      , packTile x (y - 1) p
      , packTile x (y + 1) p
      ]
    (x, y, p) = unpackTile tile
    dominatedBankGlobal = not banked && Set.member tile reachableBanks && cost > bestBank
    relaxWalk nextTile =
      let next = stateForPacked (unTile nextTile) banked
       in when (next >= 0) (relaxEdge counters best prevState prevKind prevLabel queue bestBankRef cost state next 1 "" walkingEdge)
    relaxNode nextNode =
      relaxEdge counters best prevState prevKind prevLabel queue bestBankRef cost state (stateId nextNode banked) 1 "" walkingEdge
    relaxTransport nextBanked transport =
      case destination transport of
        Just dst ->
          let next = stateForPacked (unTile dst) nextBanked
              stepCost = transportCost transport
           in when (next >= 0) (relaxEdge counters best prevState prevKind prevLabel queue bestBankRef cost state next stepCost (transportLabel transport) transportEdge)
        Nothing -> pure ()

  relaxEdge ::
    Mutable.MVector s Int -> Mutable.MVector s Int -> Mutable.MVector s Int -> Mutable.MVector s Int ->
    BoxedMutable.MVector s String -> MutableHeap s -> STRef s Int -> Int -> Int -> Int -> Int -> String -> Int -> ST s ()
  relaxEdge counters best prevState prevKind prevLabel queue bestBankRef cost state next stepCost label kind =
    when (cost /= maxBound && stepCost /= maxBound && stepCost >= 0 && cost <= maxBound - stepCost) $ do
        let newCost = cost + stepCost
        bump counters kind 1
        known <- Mutable.read best next
        when (newCost < known) $ do
            updateBestBank counters bestBankRef next newCost
            currentBestBank <- readSTRef bestBankRef
            case effectiveHeuristicRaw currentBestBank newCost next of
              (# h#, dominated# #)
                | I# h# == maxBound -> countHeuristicPrune counters next
                | otherwise -> do
                    Mutable.write best next newCost
                    Mutable.write prevState next state
                    Mutable.write prevKind next (if kind == walkingEdge then 0 else 1)
                    when (kind == transportEdge) (BoxedMutable.write prevLabel next label)
                    heapPush queue (addCostDefault maxBound newCost (weightedHeuristic (I# h#))) next newCost
                    bump counters counterPqPushes 1
                    bump counters counterUniqueStates (if known == maxBound then 1 else 0)
                    bump counters counterHeuristicEvaluations 1
                    bump counters counterBankDominated (I# dominated#)

  weightedHeuristic value = min maxBound (round (searchHeuristicWeight options * fromIntegral value))
  effectiveHeuristicRaw :: Int -> Int -> Int -> (# Int#, Int# #)
  effectiveHeuristicRaw bestBank cost state
    | stateBanked state =
        case heuristicAtSearchNodeRaw state True of
          I# resolved# -> (# resolved#, 0# #)
    | bankGlobalRelevant && cost > bestBank =
        case heuristicAtSearchNodeRaw state False of
          I# unbanked# -> case heuristicAtSearchNodeRaw state True of
            I# resolved# -> case max (I# unbanked#) (if I# resolved# == maxBound then I# unbanked# else I# resolved#) of
              I# effective# -> (# effective#, 1# #)
    | otherwise =
        case heuristicAtSearchNodeRaw state False of
          I# unbanked# -> (# unbanked#, 0# #)
  heuristicAtSearchNodeRaw state banked
    | node < baseLength = heuristicAtComponentRaw heuristic packed (searchBaseComponents space Vector.! node) banked
    | otherwise = heuristicAtResolvedRaw heuristic packed
        (searchExtraComponents space Boxed.! extra) (searchExtraSites space Vector.! extra) banked
   where
    node = state `div` 2
    packed = searchTileAt space node
    baseLength = Vector.length (searchBaseTiles space)
    extra = node - baseLength
  countHeuristicPrune :: forall s. Mutable.MVector s Int -> Int -> ST s ()
  countHeuristicPrune counters state = do
    bump counters counterHeuristicUnreachable 1
    if nodeHasAttachment state
      then bump counters counterNoReverseSeedPrunes 1
      else bump counters counterUnknownComponentPrunes 1
  nodeHasAttachment state
    | node < baseLength = True
    | otherwise = not (Vector.null (searchExtraComponents space Boxed.! (node - baseLength)))
   where
    node = state `div` 2
    baseLength = Vector.length (searchBaseTiles space)

  countDestinations banked = foldl' (\count transport -> count + case destination transport of
      Just dst | stateForPacked (unTile dst) banked >= 0 -> 1
      _ -> 0) 0

  bankObservation bestBank cost state =
    TileBankGlobalObservation tile banked cost bestBank dominatedBankGlobal
      [(dst, transportCost t, transportLabel t) | t <- preparedGlobalTransports availability True, Just dst <- [destination t]]
   where
    tile = stateTile state
    banked = stateBanked state
    dominatedBankGlobal = not banked && cost > bestBank

  usableOrigin banked tile = compiledAllowTransports account && any ((/= "VIRTUAL_WALL") . transportType) (localTransports banked tile)

  compiledBankTransitionAvailable banked tile = compiledBankPathEnabled account && not banked && Set.member tile reachableBanks

  localTransports banked tile = Map.findWithDefault [] tile
    (if banked then bankedLocalTransports availability else carriedLocalTransports availability)
  transportCost transport = duration transport + Map.findWithDefault 0 (transportType transport) (compiledTransportPenalties account)
  stateForPacked packed banked =
    let node = nodeForPacked packed
     in if node < 0 then -1 else stateId node banked
  nodeForPacked = searchNodeForRaw space
  stateTile state = Tile (searchTileAt space (state `div` 2))
  stateBanked state = odd state
  world = topologyWorld topology

  updateBestBank :: forall s. Mutable.MVector s Int -> STRef s Int -> Int -> Int -> ST s ()
  updateBestBank counters bestBankRef state cost = when (isBankCandidate state) $ do
    previous <- readSTRef bestBankRef
    when (cost < previous) $ do
      writeSTRef bestBankRef cost
      bump counters counterBestBankUpdates 1

counterStatesPopped, counterStalePqEntries, counterPqPushes, counterUniqueStates,
  counterWalkingRelaxations, counterTransportRelaxations, counterHeuristicEvaluations,
  counterHeuristicUnreachable, counterUnknownComponentPrunes, counterNoReverseSeedPrunes,
  counterBestBankUpdates, counterBankDominated, counterBankGlobalSuppressed,
  counterBankBoundPQRekeys, counterCount :: Int
counterStatesPopped = 0
counterStalePqEntries = 1
counterPqPushes = 2
counterUniqueStates = 3
counterWalkingRelaxations = 4
counterTransportRelaxations = 5
counterHeuristicEvaluations = 6
counterHeuristicUnreachable = 7
counterUnknownComponentPrunes = 8
counterNoReverseSeedPrunes = 9
counterBestBankUpdates = 10
counterBankDominated = 11
counterBankGlobalSuppressed = 12
counterBankBoundPQRekeys = 13
counterCount = 14

walkingEdge, transportEdge :: Int
walkingEdge = counterWalkingRelaxations
transportEdge = counterTransportRelaxations

bump :: Mutable.MVector s Int -> Int -> Int -> ST s ()
{-# INLINE bump #-}
bump counters index amount = when (amount /= 0) $ do
  current <- Mutable.read counters index
  Mutable.write counters index (current + amount)

readCounters :: Mutable.MVector s Int -> STRef s Int -> STRef s [TileBankGlobalObservation] -> ST s TileAStarCounters
readCounters counters bestBankRef bankTraceRef =
  TileAStarCounters
    <$> Mutable.read counters counterStatesPopped
    <*> Mutable.read counters counterStalePqEntries
    <*> Mutable.read counters counterPqPushes
    <*> Mutable.read counters counterUniqueStates
    <*> Mutable.read counters counterWalkingRelaxations
    <*> Mutable.read counters counterTransportRelaxations
    <*> Mutable.read counters counterHeuristicEvaluations
    <*> Mutable.read counters counterHeuristicUnreachable
    <*> Mutable.read counters counterUnknownComponentPrunes
    <*> Mutable.read counters counterNoReverseSeedPrunes
    <*> Mutable.read counters counterBestBankUpdates
    <*> readSTRef bestBankRef
    <*> Mutable.read counters counterBankDominated
    <*> Mutable.read counters counterBankGlobalSuppressed
    <*> Mutable.read counters counterBankBoundPQRekeys
    <*> readSTRef bankTraceRef

searchSpace :: TileAStar -> Tile -> PreparedTarget -> SearchSpace
searchSpace (TileAStar topology static) start prepared =
  SearchSpace base (staticSearchComponents static) (staticWalkingMasks static)
    (staticNorthNodes static) (staticSouthNodes static)
    extras extraComponents extraSites (Vector.length base + Vector.length extras)
 where
  base = staticSearchTiles static
  preparedExtras = preparedSearchExtraTiles prepared
  startPacked = unTile start
  addStart = binarySearchRaw startPacked base < 0 && binarySearchRaw startPacked preparedExtras < 0
  extras =
    if addStart
      then Vector.fromList (IntSet.toAscList (IntSet.insert startPacked (IntSet.fromList (Vector.toList preparedExtras))))
      else preparedExtras
  extraComponents = Boxed.fromList
    [ if packed == startPacked && addStart
        then Vector.fromList (structurallyReachablePointAttachments topology start)
        else preparedSearchExtraComponents prepared Boxed.! preparedIndex packed
    | packed <- Vector.toList extras
    ]
  extraSites = Vector.map (\packed -> if packed == startPacked && addStart
    then -1
    else preparedSearchExtraSites prepared Vector.! preparedIndex packed) extras
  preparedIndex packed = binarySearchRaw packed preparedExtras

searchNodeForRaw :: SearchSpace -> Int -> Int
{-# INLINE searchNodeForRaw #-}
searchNodeForRaw space packed =
  let base = searchBaseTiles space
      baseNode = binarySearchRaw packed base
   in if baseNode >= 0
        then baseNode
        else
          let extraNode = binarySearchRaw packed (searchExtraTiles space)
           in if extraNode < 0 then -1 else Vector.length base + extraNode

binarySearchRaw :: Int -> Vector.Vector Int -> Int
{-# INLINE binarySearchRaw #-}
binarySearchRaw needle values = go 0 (Vector.length values - 1)
 where
  go lo hi
    | lo > hi = -1
    | current == needle = mid
    | current < needle = go (mid + 1) hi
    | otherwise = go lo (mid - 1)
   where
    mid = (lo + hi) `div` 2
    current = values Vector.! mid

searchTileAt :: SearchSpace -> Int -> Int
searchTileAt space node
  | node < baseLength = searchBaseTiles space Vector.! node
  | otherwise = searchExtraTiles space Vector.! (node - baseLength)
 where
  baseLength = Vector.length (searchBaseTiles space)

walkNodes :: Monad m => Int -> Word8 -> Int32 -> Int32 -> (Int -> m ()) -> m ()
{-# INLINE walkNodes #-}
walkNodes node mask northRaw southRaw yield = do
  emit 6 (node - 1)
  emit 2 (node + 1)
  emit 4 south
  emit 0 north
  emit 5 (south - 1)
  emit 3 (south + 1)
  emit 7 (north - 1)
  emit 1 (north + 1)
 where
  north = fromIntegral northRaw
  south = fromIntegral southRaw
  emit bit next = when (testBit mask bit) (yield next)
