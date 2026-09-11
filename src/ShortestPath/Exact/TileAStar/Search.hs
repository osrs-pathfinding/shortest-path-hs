module ShortestPath.Exact.TileAStar.Search
  ( search
  ) where

import Control.Monad (foldM, when)
import Control.Monad.ST (ST, runST)
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector.Mutable as BoxedMutable
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as Mutable

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
  , searchExtraTiles :: Vector.Vector Int
  , searchSize :: !Int
  }

search :: Bool -> TileAStar -> Query -> QueryTransportAvailability -> Heuristic -> (Route, TileAStarCounters, [(Tile, Bool)])
search trace astar@(TileAStar topology _) q availability heuristic = runST $ do
  best <- Mutable.replicate stateCount maxBound
  prevState <- Mutable.replicate stateCount maxBound
  prevStep <- BoxedMutable.replicate stateCount Nothing
  exploredRef <- newSTRef []
  bestBankRef <- newSTRef maxBound
  queue <- heapNew (min stateCount 262144)
  counters <- foldM (\counters (state, cost, step) -> do
        known <- Mutable.read best state
        if cost >= known
          then pure counters
          else do
            previousBank <- readSTRef bestBankRef
            when (isBankCandidate state && cost < previousBank) (writeSTRef bestBankRef cost)
            bestBank <- readSTRef bestBankRef
            let counters' = counters {tileBestBankCostUpdates = tileBestBankCostUpdates counters + if isBankCandidate state && cost < previousBank then 1 else 0}
            case effectiveHeuristic bestBank cost state of
              Nothing -> pure (countHeuristicPrune (stateTile state) counters')
              Just (h, dominated) -> do
                Mutable.write best state cost
                case step of
                  Nothing -> pure ()
                  Just value -> do
                    Mutable.write prevState state startState
                    BoxedMutable.write prevStep state (Just value)
                heapPush queue (addCostDefault maxBound cost (weightedHeuristic h)) state cost
                pure counters'
                  { tilePqPushes = tilePqPushes counters' + 1
                  , tileUniqueStatesReached = tileUniqueStatesReached counters' + if known == maxBound then 1 else 0
                  , tileHeuristicEvaluations = tileHeuristicEvaluations counters' + 1
                  , tileBankDominatedHeuristicEvaluations = tileBankDominatedHeuristicEvaluations counters' + if dominated then 1 else 0
                  }) emptyCounters initialStates
  go exploredRef best prevState prevStep queue bestBankRef counters
 where
  space = searchSpace astar q
  stateCount = searchSize space * 2
  startNode = maybe 0 id (nodeFor (queryStart q))
  startState = stateId startNode False
  target = queryTarget q
  reachableBanks = Set.filter (not . null . structurallyReachablePointAttachments topology) (worldBanks world)
  bankGlobalRelevant = allowTransports q && bankPathEnabled q
  isBankCandidate state = bankGlobalRelevant && not (stateBanked state) && Set.member (stateTile state) reachableBanks
  initialStates = (startState, 0, Nothing) :
    [ (next, stepCost, Just step)
    | allowTransports q
    , t <- preparedGlobalTransports availability False
    , Just (dst, stepCost, step) <- [preparedTransport q t]
    , Just next <- [stateFor dst False]
    ]

  go ::
    STRef s [(Tile, Bool)] ->
    Mutable.MVector s Int ->
    Mutable.MVector s Int ->
    BoxedMutable.MVector s (Maybe RouteStep) ->
    MutableHeap s ->
    STRef s Int ->
    TileAStarCounters ->
    ST s (Route, TileAStarCounters, [(Tile, Bool)])
  go exploredRef best prevState prevStep queue bestBankRef counters = do
    popped <- heapPop queue
    case popped of
      Nothing -> do
        explored <- reverse <$> readSTRef exploredRef
        finalBank <- readSTRef bestBankRef
        pure (Route maxBound (tileStatesPopped counters) [], counters {tileFinalBestBankCost = finalBank}, explored)
      Just (priority, state, cost) -> do
        known <- Mutable.read best state
        if cost /= known
          then go exploredRef best prevState prevStep queue bestBankRef counters {tileStalePqEntries = tileStalePqEntries counters + 1}
          else do
            bestBank <- readSTRef bestBankRef
            let effective = effectiveHeuristic bestBank cost state
            case effective of
              Nothing -> go exploredRef best prevState prevStep queue bestBankRef (countHeuristicPrune (stateTile state) counters)
              Just (h, dominated)
                | addCostDefault maxBound cost (weightedHeuristic h) > priority -> do
                    heapPush queue (addCostDefault maxBound cost (weightedHeuristic h)) state cost
                    go exploredRef best prevState prevStep queue bestBankRef counters
                      { tileBankBoundPQRekeys = tileBankBoundPQRekeys counters + 1
                      , tileBankDominatedHeuristicEvaluations = tileBankDominatedHeuristicEvaluations counters + if dominated then 1 else 0
                      }
                | otherwise -> do
                    let tile = stateTile state
                    when trace $ do
                      explored <- readSTRef exploredRef
                      writeSTRef exploredRef ((tile, stateBanked state) : explored)
                    if tile == target
                      then do
                        steps <- reconstructRouteSteps prevState prevStep state
                        explored <- reverse <$> readSTRef exploredRef
                        finalBank <- readSTRef bestBankRef
                        pure (Route cost (tileStatesPopped counters) steps, counters {tileFinalBestBankCost = finalBank}, explored)
                      else do
                        currentBestBank <- readSTRef bestBankRef
                        let (nextStates, suppressed, observation) = neighbors currentBestBank cost state
                            counters' = counters
                              { tileStatesPopped = tileStatesPopped counters + 1
                              , tileBankGlobalTransitionsSuppressed = tileBankGlobalTransitionsSuppressed counters + suppressed
                              , tileBankGlobalTrace = maybe (tileBankGlobalTrace counters) (: tileBankGlobalTrace counters) observation
                              }
                        counters'' <- foldM (relax best prevState prevStep queue bestBankRef cost state) counters' nextStates
                        go exploredRef best prevState prevStep queue bestBankRef counters''

  relax ::
    Mutable.MVector s Int -> Mutable.MVector s Int -> BoxedMutable.MVector s (Maybe RouteStep) ->
    MutableHeap s -> STRef s Int -> Int -> Int -> TileAStarCounters ->
    (Int, Int, RouteStep, EdgeKind) -> ST s TileAStarCounters
  relax best prevState prevStep queue bestBankRef cost state counters (next, stepCost, step, kind) =
    case addCost cost stepCost of
      Nothing -> pure counters
      Just newCost -> do
        known <- Mutable.read best next
        if newCost >= known
          then pure (countKind kind counters)
          else do
            previousBank <- readSTRef bestBankRef
            when (isBankCandidate next && newCost < previousBank) (writeSTRef bestBankRef newCost)
            let counters' = counters {tileBestBankCostUpdates = tileBestBankCostUpdates counters + if isBankCandidate next && newCost < previousBank then 1 else 0}
            bestBank <- readSTRef bestBankRef
            let effective = effectiveHeuristic bestBank newCost next
            case effective of
              Nothing -> pure (countKind kind (countHeuristicPrune (stateTile next) counters'))
              Just (h, dominated) -> do
                Mutable.write best next newCost
                Mutable.write prevState next state
                BoxedMutable.write prevStep next (Just step)
                let counted = countKind kind counters'
                    counters'' = counted
                      { tilePqPushes = tilePqPushes counted + 1
                      , tileUniqueStatesReached = tileUniqueStatesReached counted + if known == maxBound then 1 else 0
                      , tileHeuristicEvaluations = tileHeuristicEvaluations counted + 1
                      , tileBankDominatedHeuristicEvaluations = tileBankDominatedHeuristicEvaluations counted + if dominated then 1 else 0
                      }
                heapPush queue (addCostDefault maxBound newCost (weightedHeuristic h)) next newCost
                pure counters''

  countKind WalkingEdge counters = counters {tileWalkingRelaxations = tileWalkingRelaxations counters + 1}
  countKind TransportEdge counters = counters {tileTransportRelaxations = tileTransportRelaxations counters + 1}

  weightedHeuristic value = min maxBound (round (heuristicWeight q * fromIntegral value))
  effectiveHeuristic bestBank cost state =
    let unresolved = not (stateBanked state)
        dominated = bankGlobalRelevant && unresolved && cost > bestBank
        unbanked = heuristicAt topology heuristic (stateTile state) False
        resolved = heuristicAt topology heuristic (stateTile state) True
     in case if unresolved then unbanked else resolved of
      Nothing -> Nothing
      Just value -> Just (if dominated then maybe value (max value) resolved else value, dominated)
  countHeuristicPrune tile counters =
    counters
      { tileHeuristicUnreachable = tileHeuristicUnreachable counters + 1
      , tileUnknownComponentPrunes = tileUnknownComponentPrunes counters + if null (structurallyReachablePointAttachments topology tile) then 1 else 0
      , tileNoReverseSeedPrunes = tileNoReverseSeedPrunes counters + if null (structurallyReachablePointAttachments topology tile) then 0 else 1
      }

  neighbors bestBank cost state =
    (walk <> bank <> localTransports <> bankGlobalTransports, length suppressedBankGlobals, observation)
   where
    tile = stateTile state
    banked = stateBanked state
    walk =
      [ (next, 1, Walk t, WalkingEdge)
      | t <- walkingNeighborsRaw world tile
      , isWalkable (worldCollision world) t || usableOrigin banked t
      , Just next <- [stateFor t banked]
      ]
    bank =
      [ (next, 0, Walk tile, TransportEdge)
      | bankTransitionAvailable q reachableBanks banked tile
      , Just next <- [stateFor tile True]
      ]
    localTransports =
      if allowTransports q
        then transportEdges banked (preparedLocalTransportsAt availability banked tile)
        else []
    bankGlobalTransports =
      if dominatedBankGlobal then [] else suppressedBankGlobals
    suppressedBankGlobals =
      [ (next, stepCost, step, TransportEdge)
      | allowTransports q
      , bankTransitionAvailable q reachableBanks banked tile
      , (next, stepCost, step, _) <- transportEdges True (preparedGlobalTransports availability True)
      ]
    -- Keep equal-cost bank globals so the path establishing the bound remains materialized.
    dominatedBankGlobal = not banked && Set.member tile reachableBanks && cost > bestBank
    observation =
      if allowTransports q && Set.member tile reachableBanks
        then Just (TileBankGlobalObservation tile banked cost bestBank dominatedBankGlobal
          [(dst, stepCost, transportLabel t) | t <- preparedGlobalTransports availability True, Just (dst, stepCost, _) <- [preparedTransport q t]])
        else Nothing

  transportEdges banked transports =
    [ (next, stepCost, step, TransportEdge)
    | t <- transports
    , Just (dst, stepCost, step) <- [preparedTransport q t]
    , Just next <- [stateFor dst banked]
    ]

  usableOrigin banked tile = allowTransports q && not (null (preparedLocalTransportsAt availability banked tile))

  stateFor tile banked = flip stateId banked <$> nodeFor tile
  nodeFor tile = searchNodeFor space tile
  stateTile state = Tile (searchTileAt space (state `div` 2))
  stateBanked state = odd state
  world = topologyWorld topology

data EdgeKind = WalkingEdge | TransportEdge

emptyCounters :: TileAStarCounters
emptyCounters = TileAStarCounters
  { tileStatesPopped = 0
  , tileStalePqEntries = 0
  , tilePqPushes = 0
  , tileUniqueStatesReached = 0
  , tileWalkingRelaxations = 0
  , tileTransportRelaxations = 0
  , tileHeuristicEvaluations = 0
  , tileHeuristicUnreachable = 0
  , tileUnknownComponentPrunes = 0
  , tileNoReverseSeedPrunes = 0
  , tileBestBankCostUpdates = 0
  , tileFinalBestBankCost = maxBound
  , tileBankDominatedHeuristicEvaluations = 0
  , tileBankGlobalTransitionsSuppressed = 0
  , tileBankBoundPQRekeys = 0
  , tileBankGlobalTrace = []
  }

searchSpace :: TileAStar -> Query -> SearchSpace
searchSpace (TileAStar topology _) q =
  SearchSpace base extras (Vector.length base + Vector.length extras)
 where
  base = Vector.fromList (map fst (reachableComponentTiles topology))
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
      , binarySearch packed base == Nothing
      ]
  world = topologyWorld topology

searchNodeFor :: SearchSpace -> Tile -> Maybe Int
searchNodeFor space tile =
  case binarySearch packed (searchBaseTiles space) of
    Just ix -> Just ix
    Nothing -> (+ Vector.length (searchBaseTiles space)) <$> binarySearch packed (searchExtraTiles space)
 where
  packed = unTile tile

searchTileAt :: SearchSpace -> Int -> Int
searchTileAt space node
  | node < baseLength = searchBaseTiles space Vector.! node
  | otherwise = searchExtraTiles space Vector.! (node - baseLength)
 where
  baseLength = Vector.length (searchBaseTiles space)
