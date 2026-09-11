{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -ddump-stg-final #-}

module ShortestPath.Exact.TileAStar.Search
  ( search
  , walkingNeighborsRawDirect
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
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
  , searchExtraTiles :: Vector.Vector Int
  , searchExtraComponents :: Boxed.Vector (Vector.Vector Int)
  , searchExtraSites :: Vector.Vector Int
  , searchSize :: !Int
  }

search :: Bool -> TileAStar -> Query -> QueryTransportAvailability -> Heuristic -> (Route, TileAStarCounters, [(Tile, Bool)])
search trace astar@(TileAStar topology _) q availability heuristic = runST $ do
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
  space = searchSpace astar q heuristic
  stateCount = searchSize space * 2
  startNode = max 0 (nodeForPacked (unTile (queryStart q)))
  startState = stateId startNode False
  target = queryTarget q
  reachableBanks = Set.filter (not . null . structurallyReachablePointAttachments topology) (worldBanks world)
  bankGlobalRelevant = allowTransports q && bankPathEnabled q
  isBankCandidate state = bankGlobalRelevant && not (stateBanked state) && Set.member (stateTile state) reachableBanks
  initialStates = (startState, 0, -1, "") :
    [ (next, duration t + Map.findWithDefault 0 (transportType t) (transportPenalties q), 1, transportLabel t)
    | allowTransports q
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
                            suppressed = if allowTransports q && bankTransitionAvailable q reachableBanks banked tile then countDestinations True globals else 0
                        bump counters counterStatesPopped 1
                        bump counters counterBankGlobalSuppressed suppressed
                        when (allowTransports q && Set.member tile reachableBanks) $ do
                          observations <- readSTRef bankTraceRef
                          writeSTRef bankTraceRef (bankObservation currentBestBank cost state : observations)
                        relaxNeighbors counters best prevState prevKind prevLabel queue bestBankRef currentBestBank cost state
                        go exploredRef bankTraceRef counters best prevState prevKind prevLabel queue bestBankRef

  relaxNeighbors ::
    Mutable.MVector s Int -> Mutable.MVector s Int -> Mutable.MVector s Int -> Mutable.MVector s Int ->
    BoxedMutable.MVector s String -> MutableHeap s -> STRef s Int -> Int -> Int -> Int -> ST s ()
  relaxNeighbors counters best prevState prevKind prevLabel queue bestBankRef bestBank cost state = do
    walkingNeighborsRawDirect world tile relaxWalk
    when (bankTransitionAvailable q reachableBanks banked tile) $
      relaxEdge counters best prevState prevKind prevLabel queue bestBankRef cost state (state + 1) 0 "" transportEdge
    when (allowTransports q) $
      forM_ (localTransports banked tile) $ \transport ->
        when (transportType transport /= "VIRTUAL_WALL") (relaxTransport banked transport)
    when (allowTransports q && bankTransitionAvailable q reachableBanks banked tile && not dominatedBankGlobal) $
      forM_ (preparedGlobalTransports availability True) (relaxTransport True)
   where
    tile = stateTile state
    banked = stateBanked state
    dominatedBankGlobal = not banked && Set.member tile reachableBanks && cost > bestBank
    relaxWalk nextTile
      | isWalkable (worldCollision world) nextTile || usableOrigin banked nextTile =
          let next = stateForPacked (unTile nextTile) banked
           in when (next >= 0) (relaxEdge counters best prevState prevKind prevLabel queue bestBankRef cost state next 1 "" walkingEdge)
      | otherwise = pure ()
    relaxTransport nextBanked transport =
      case destination transport of
        Just dst ->
          let next = stateForPacked (unTile dst) nextBanked
              stepCost = duration transport + Map.findWithDefault 0 (transportType transport) (transportPenalties q)
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

  weightedHeuristic value = min maxBound (round (heuristicWeight q * fromIntegral value))
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
      [(dst, duration t + Map.findWithDefault 0 (transportType t) (transportPenalties q), transportLabel t) | t <- preparedGlobalTransports availability True, Just dst <- [destination t]]
   where
    tile = stateTile state
    banked = stateBanked state
    dominatedBankGlobal = not banked && cost > bestBank

  usableOrigin banked tile = allowTransports q && any ((/= "VIRTUAL_WALL") . transportType) (localTransports banked tile)

  localTransports banked tile = Map.findWithDefault [] tile
    (if banked then bankedLocalTransports availability else carriedLocalTransports availability)
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

searchSpace :: TileAStar -> Query -> Heuristic -> SearchSpace
searchSpace (TileAStar topology static) q heuristic =
  SearchSpace base (staticSearchComponents static) extras extraComponents extraSites (Vector.length base + Vector.length extras)
 where
  base = staticSearchTiles static
  endpoints =
    queryStart q : queryTarget q : Set.toList (worldBanks world) <>
      [ tile
      | t <- concat (Map.elems (worldTransports world)) <> worldGlobalTeleports world
      , Just tile <- [origin t] <> [destination t]
      ]
  extras =
    Vector.fromList
      [ packed
      | packed <- IntSet.toAscList (IntSet.fromList (map unTile endpoints))
      , binarySearchRaw packed base < 0
      ]
  extraComponents = Boxed.fromList
    [ Vector.fromList (structurallyReachablePointAttachments topology (Tile packed))
    | packed <- Vector.toList extras
    ]
  extraSites = Vector.map (\packed -> IntMap.findWithDefault (-1) packed (heuristicSiteIndex heuristic)) extras
  world = topologyWorld topology

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

searchNodeFor :: SearchSpace -> Tile -> Maybe Int
searchNodeFor space tile =
  let node = searchNodeForRaw space (unTile tile)
   in if node < 0 then Nothing else Just node

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

walkingNeighborsRawDirect :: Monad m => World -> Tile -> (Tile -> m ()) -> m ()
{-# INLINE walkingNeighborsRawDirect #-}
walkingNeighborsRawDirect world tile yield =
  if isWalkable cm tile
    then do
      emit westOpen west
      emit eastOpen east
      emit southOpen south
      emit northOpen north
      emit southWestOpen southWest
      emit southEastOpen southEast
      emit northWestOpen northWest
      emit northEastOpen northEast
      blockedOrigin west
      blockedOrigin east
      blockedOrigin south
      blockedOrigin north
    else do
      blockedExit True west
      blockedExit True east
      blockedExit True south
      blockedExit True north
      blockedExit southWestCardinals southWest
      blockedExit southEastCardinals southEast
      blockedExit northWestCardinals northWest
      blockedExit northEastCardinals northEast
 where
  cm = worldCollision world
  (x, y, p) = unpackTile tile
  west = packTile (x - 1) y p
  east = packTile (x + 1) y p
  south = packTile x (y - 1) p
  north = packTile x (y + 1) p
  southWest = packTile (x - 1) (y - 1) p
  southEast = packTile (x + 1) (y - 1) p
  northWest = packTile (x - 1) (y + 1) p
  northEast = packTile (x + 1) (y + 1) p
  northAt a b = collisionFlag cm a b p 0
  southAt a b = northAt a (b - 1)
  eastAt a b = collisionFlag cm a b p 1
  westAt a b = eastAt (a - 1) b
  westOpen = westAt x y
  eastOpen = eastAt x y
  southOpen = southAt x y
  northOpen = northAt x y
  southWestOpen = southOpen && westAt x (y - 1) && westOpen && southAt (x - 1) y
  southEastOpen = southOpen && eastAt x (y - 1) && eastOpen && southAt (x + 1) y
  northWestOpen = northOpen && westAt x (y + 1) && westOpen && northAt (x - 1) y
  northEastOpen = northOpen && eastAt x (y + 1) && eastOpen && northAt (x + 1) y
  southWestCardinals = isWalkable cm west && isWalkable cm south
  southEastCardinals = isWalkable cm east && isWalkable cm south
  northWestCardinals = isWalkable cm west && isWalkable cm north
  northEastCardinals = isWalkable cm east && isWalkable cm north
  emit allowed next = when allowed (yield next)
  blockedOrigin next = when (not (isWalkable cm next) && Map.member next (worldTransports world)) (yield next)
  blockedExit cardinals next = when (isWalkable cm next && cardinals) (yield next)
