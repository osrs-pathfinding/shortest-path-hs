{-# OPTIONS_GHC -ddump-simpl -ddump-to-file #-}

module ShortestPath.Exact.TileAStar.Search
  ( search
  , walkingNeighborsRawDirect
  ) where

import Control.Monad (foldM, when)
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
  queue <- heapNew (min stateCount 262144)
  counters <- foldM (\counters (state, cost, kind, label) -> do
        known <- Mutable.read best state
        if cost >= known
          then pure counters
          else do
            previousBank <- readSTRef bestBankRef
            when (isBankCandidate state && cost < previousBank) (writeSTRef bestBankRef cost)
            bestBank <- readSTRef bestBankRef
            let counters' = counters {tileBestBankCostUpdates = tileBestBankCostUpdates counters + if isBankCandidate state && cost < previousBank then 1 else 0}
            case effectiveHeuristic bestBank cost state of
              Nothing -> pure (countHeuristicPrune state counters')
              Just (h, dominated) -> do
                Mutable.write best state cost
                when (kind >= 0) $ do
                  Mutable.write prevState state startState
                  Mutable.write prevKind state kind
                  BoxedMutable.write prevLabel state label
                heapPush queue (addCostDefault maxBound cost (weightedHeuristic h)) state cost
                pure counters'
                  { tilePqPushes = tilePqPushes counters' + 1
                  , tileUniqueStatesReached = tileUniqueStatesReached counters' + if known == maxBound then 1 else 0
                  , tileHeuristicEvaluations = tileHeuristicEvaluations counters' + 1
                  , tileBankDominatedHeuristicEvaluations = tileBankDominatedHeuristicEvaluations counters' + if dominated then 1 else 0
                  }) emptyCounters initialStates
  go exploredRef best prevState prevKind prevLabel queue bestBankRef counters
 where
  space = searchSpace astar q heuristic
  stateCount = searchSize space * 2
  startNode = maybe 0 id (nodeFor (queryStart q))
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
    , Just next <- [stateFor dst False]
    ]

  go ::
    STRef s [(Tile, Bool)] ->
    Mutable.MVector s Int ->
    Mutable.MVector s Int ->
    Mutable.MVector s Int ->
    BoxedMutable.MVector s String ->
    MutableHeap s ->
    STRef s Int ->
    TileAStarCounters ->
    ST s (Route, TileAStarCounters, [(Tile, Bool)])
  go exploredRef best prevState prevKind prevLabel queue bestBankRef counters = do
    popped <- heapPop queue
    case popped of
      Nothing -> do
        explored <- reverse <$> readSTRef exploredRef
        finalBank <- readSTRef bestBankRef
        pure (Route maxBound (tileStatesPopped counters) [], counters {tileFinalBestBankCost = finalBank}, explored)
      Just (priority, state, cost) -> do
        known <- Mutable.read best state
        if cost /= known
          then go exploredRef best prevState prevKind prevLabel queue bestBankRef counters {tileStalePqEntries = tileStalePqEntries counters + 1}
          else do
            bestBank <- readSTRef bestBankRef
            let effective = effectiveHeuristic bestBank cost state
            case effective of
              Nothing -> go exploredRef best prevState prevKind prevLabel queue bestBankRef (countHeuristicPrune state counters)
              Just (h, dominated)
                | addCostDefault maxBound cost (weightedHeuristic h) > priority -> do
                    heapPush queue (addCostDefault maxBound cost (weightedHeuristic h)) state cost
                    go exploredRef best prevState prevKind prevLabel queue bestBankRef counters
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
                        steps <- reconstructRouteSteps stateTile prevState prevKind prevLabel state
                        explored <- reverse <$> readSTRef exploredRef
                        finalBank <- readSTRef bestBankRef
                        pure (Route cost (tileStatesPopped counters) steps, counters {tileFinalBestBankCost = finalBank}, explored)
                      else do
                        currentBestBank <- readSTRef bestBankRef
                        let banked = stateBanked state
                            globals = preparedGlobalTransports availability True
                            suppressed = if allowTransports q && bankTransitionAvailable q reachableBanks banked tile then countDestinations True globals else 0
                            observation = bankObservation currentBestBank cost state
                            counters' = counters
                              { tileStatesPopped = tileStatesPopped counters + 1
                              , tileBankGlobalTransitionsSuppressed = tileBankGlobalTransitionsSuppressed counters + suppressed
                              , tileBankGlobalTrace = maybe (tileBankGlobalTrace counters) (: tileBankGlobalTrace counters) observation
                              }
                        counters'' <- relaxNeighbors best prevState prevKind prevLabel queue bestBankRef currentBestBank cost state counters'
                        go exploredRef best prevState prevKind prevLabel queue bestBankRef counters''

  relaxNeighbors ::
    Mutable.MVector s Int -> Mutable.MVector s Int -> Mutable.MVector s Int -> BoxedMutable.MVector s String ->
    MutableHeap s -> STRef s Int -> Int -> Int -> Int -> TileAStarCounters -> ST s TileAStarCounters
  relaxNeighbors best prevState prevKind prevLabel queue bestBankRef bestBank cost state counters = do
    walked <- foldM relaxWalk counters (walkingNeighborsRaw world tile)
    bankedCounters <-
      if bankTransitionAvailable q reachableBanks banked tile
        then maybe (pure walked) (\next -> relax best prevState prevKind prevLabel queue bestBankRef cost state walked next 0 "" TransportEdge) (stateFor tile True)
        else pure walked
    localCounters <-
      if allowTransports q
        then foldM (relaxTransport banked) bankedCounters (preparedLocalTransportsAt availability banked tile)
        else pure bankedCounters
    if allowTransports q && bankTransitionAvailable q reachableBanks banked tile && not dominatedBankGlobal
      then foldM (relaxTransport True) localCounters (preparedGlobalTransports availability True)
      else pure localCounters
   where
    tile = stateTile state
    banked = stateBanked state
    dominatedBankGlobal = not banked && Set.member tile reachableBanks && cost > bestBank
    relaxWalk current nextTile
      | isWalkable (worldCollision world) nextTile || usableOrigin banked nextTile =
          maybe (pure current) (\next -> relax best prevState prevKind prevLabel queue bestBankRef cost state current next 1 "" WalkingEdge) (stateFor nextTile banked)
      | otherwise = pure current
    relaxTransport nextBanked current transport =
      case destination transport of
        Just dst ->
          let stepCost = duration transport + Map.findWithDefault 0 (transportType transport) (transportPenalties q)
           in maybe (pure current) (\next -> relax best prevState prevKind prevLabel queue bestBankRef cost state current next stepCost (transportLabel transport) TransportEdge) (stateFor dst nextBanked)
        Nothing -> pure current

  relax ::
    Mutable.MVector s Int -> Mutable.MVector s Int -> Mutable.MVector s Int -> BoxedMutable.MVector s String ->
    MutableHeap s -> STRef s Int -> Int -> Int -> TileAStarCounters ->
    Int -> Int -> String -> EdgeKind -> ST s TileAStarCounters
  relax best prevState prevKind prevLabel queue bestBankRef cost state counters next stepCost label kind =
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
              Nothing -> pure (countKind kind (countHeuristicPrune next counters'))
              Just (h, dominated) -> do
                Mutable.write best next newCost
                Mutable.write prevState next state
                case kind of
                  WalkingEdge -> Mutable.write prevKind next 0
                  TransportEdge -> Mutable.write prevKind next 1 >> BoxedMutable.write prevLabel next label
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
        unbanked = heuristicAtSearchNode state False
        resolved = heuristicAtSearchNode state True
     in case if unresolved then unbanked else resolved of
      Nothing -> Nothing
      Just value -> Just (if dominated then maybe value (max value) resolved else value, dominated)
  heuristicAtSearchNode state banked
    | node < baseLength = heuristicAtComponent heuristic packed (searchBaseComponents space Vector.! node) banked
    | otherwise = heuristicAtResolved heuristic packed
        (searchExtraComponents space Boxed.! extra) (searchExtraSites space Vector.! extra) banked
   where
    node = state `div` 2
    packed = searchTileAt space node
    baseLength = Vector.length (searchBaseTiles space)
    extra = node - baseLength
  countHeuristicPrune state counters =
    let attached = nodeHasAttachment state
     in counters
      { tileHeuristicUnreachable = tileHeuristicUnreachable counters + 1
      , tileUnknownComponentPrunes = tileUnknownComponentPrunes counters + if attached then 0 else 1
      , tileNoReverseSeedPrunes = tileNoReverseSeedPrunes counters + if attached then 1 else 0
      }
  nodeHasAttachment state
    | node < baseLength = True
    | otherwise = not (Vector.null (searchExtraComponents space Boxed.! (node - baseLength)))
   where
    node = state `div` 2
    baseLength = Vector.length (searchBaseTiles space)

  countDestinations banked = foldl' (\count transport -> count + case destination transport >>= \dst -> stateFor dst banked of Nothing -> 0; Just _ -> 1) 0

  bankObservation bestBank cost state =
    if allowTransports q && Set.member tile reachableBanks
      then Just (TileBankGlobalObservation tile banked cost bestBank dominatedBankGlobal
        [(dst, duration t + Map.findWithDefault 0 (transportType t) (transportPenalties q), transportLabel t) | t <- preparedGlobalTransports availability True, Just dst <- [destination t]])
      else Nothing
   where
    tile = stateTile state
    banked = stateBanked state
    dominatedBankGlobal = not banked && cost > bestBank

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
      , binarySearch packed base == Nothing
      ]
  extraComponents = Boxed.fromList
    [ Vector.fromList (structurallyReachablePointAttachments topology (Tile packed))
    | packed <- Vector.toList extras
    ]
  extraSites = Vector.map (\packed -> IntMap.findWithDefault (-1) packed (heuristicSiteIndex heuristic)) extras
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
