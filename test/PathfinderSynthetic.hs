module Main (main) where

import Data.Bits (setBit)
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Vector

import ShortestPath.Exact.TileAStar
import ShortestPath.Exact.TileAStar.Debug
import ShortestPath.Exact.TileAStar.Heuristic
  ( Heuristic(..), heuristicAt, heuristicAtComponent, heuristicAtGeneratorsComponent
  , heuristicAtResolved, prepareHeuristic, prepareHeuristicProfiled, seedKey
  )
import ShortestPath.Exact.TileAStar.HeuristicScan
import ShortestPath.Exact.TileAStar.RelaxedGraph
import ShortestPath.Exact.TileAStar.ReverseSearch
import ShortestPath.Exact.TileAStar.Types
import ShortestPath.Exact.ReferenceDijkstra
import ShortestPath.Account
import ShortestPath.Pathfinder
import ShortestPath.Requirements
import ShortestPath.Tile
import ShortestPath.Topology
import ShortestPath.Transport
import ShortestPath.Wilderness
import ShortestPath.World

main :: IO ()
main = do
  let (world, tiles) = synthetic
      defaults = defaultQuery (tA0 tiles) (tA1 tiles)
  assert (not (Set.member "SEASONAL_TRANSPORTS" (enabledTransportTypes defaults)))
  assert (Set.member "TELEPORTATION_ITEM" (enabledTransportTypes defaults))
  tileAStar <- mustRight =<< buildTileAStarWithPolicy (syntheticPolicy (tA0 tiles)) world
  let reference = ReferenceDijkstra (tileTopology tileAStar)
  mapM_ (checkRoute reference tileAStar world) (cases tiles)
  checkHeuristicLookup tileAStar tiles
  checkPreparationLifetimes tileAStar tiles
  checkTargetOverlay tileAStar tiles
  let globalQuery = query (tA3 tiles) (tD1 tiles) (Set.singleton "SYNTHETIC_GLOBAL") False
      rawGlobalRoute = findRouteReferenceDijkstra reference globalQuery
      tileGlobalRoute = findRouteTileAStar tileAStar globalQuery
  assert (routeSteps rawGlobalRoute == [UseTransport "SYNTHETIC_GLOBAL" (tD1 tiles)])
  assert (routeSteps tileGlobalRoute == [UseTransport "SYNTHETIC_GLOBAL" (tD1 tiles)])
  checkReversePathDebug tileAStar tiles
  checkTransportOnlyEndpoint reference tileAStar tiles
  checkIntermediateTransportEndpoint reference tileAStar tiles
  checkHeuristicPruning tileAStar tiles
  checkInstrumentation tileAStar tiles
  checkMultiplePointAttachments
  checkManhattanGeneratorProvenance
  checkBankGlobalHub
  checkWildernessGlobals
  checkGeneratorScanKernel

checkWildernessGlobals :: IO ()
checkWildernessGlobals = do
  assertMsg "outside Wilderness was not AllGlobals" (globalCapabilityAt outside == AllGlobals)
  assertMsg "level 21-30 was not WildernessGlobals" (globalCapabilityAt mid == WildernessGlobals)
  assertMsg "above level 30 was not NoGlobals" (globalCapabilityAt deep == NoGlobals)
  astar <- mustRight =<< buildTileAStarWithPolicy (syntheticPolicy deep) world
  let reference = ReferenceDijkstra (tileTopology astar)
      run start target enabled = do
        let q = (defaultQuery start target)
              { enabledTransportTypes = Set.fromList enabled, bankPathEnabled = False }
            exact = findRouteReferenceDijkstra reference q
            fast = findRouteTileAStar astar q
        assertMsg ("Wilderness A*/Dijkstra mismatch: " <> show (start, target, routeCost fast, routeCost exact, routeSteps fast, routeSteps exact))
          (routeCost fast == routeCost exact)
        pure fast
  outsideRoute <- run outside ordinaryTarget ["WILD_20"]
  assertMsg "ordinary global was not immediately available outside Wilderness"
    (routeCost outsideRoute == 1 && routeSteps outsideRoute == [UseTransport "WILD_20" ordinaryTarget])
  midRestricted <- run mid restrictedTarget ["WILD_30"]
  midOrdinary <- run mid ordinaryTarget ["WILD_20"]
  assertMsg "level-30 global was unavailable in level 21-30 Wilderness" (routeCost midRestricted == 1)
  assertMsg "ordinary global was illegally available above level 20" (routeCost midOrdinary == 81)
  deepRestricted <- run deep restrictedTarget ["WILD_30"]
  deepOrdinary <- run deep ordinaryTarget ["WILD_20"]
  assertMsg "deep Wilderness did not activate level-30 globals at the boundary"
    (routeCost deepRestricted == 3 && take 2 (routeSteps deepRestricted) == [Walk deep30, Walk mid])
  assertMsg "deep Wilderness did not continue to level 20 for an ordinary global"
    (routeCost deepOrdinary == 83 && length (filter isWalk (routeSteps deepOrdinary)) == 82)
  localExit <- run deep ordinaryTarget ["WILD_EXIT", "WILD_20"]
  assertMsg "local transport destination did not activate globals"
    (routeCost localExit == 4 && routeSteps localExit == [UseTransport "WILD_EXIT" outside, UseTransport "WILD_20" ordinaryTarget])
  noGlobal <- run deep deepNeighbor ["WILD_20", "WILD_30"]
  assertMsg "search used a global when walking was optimal" (routeCost noGlobal == 1 && routeSteps noGlobal == [Walk deepNeighbor])

  competingAstar <- mustRight =<< buildTileAStarWithPolicy (syntheticPolicy deep) competingWorld
  let competingReference = ReferenceDijkstra (tileTopology competingAstar)
      competingQuery = (defaultQuery deep ordinaryTarget)
        { enabledTransportTypes = Set.fromList ["EXPENSIVE_EXIT", "CHEAP_STAGE", "CHEAP_EXIT", "WILD_20"]
        , bankPathEnabled = False
        }
      competingFast = findRouteTileAStar competingAstar competingQuery
      competingExact = findRouteReferenceDijkstra competingReference competingQuery
  assertMsg "first-discovered activation incorrectly won"
    (routeCost competingFast == 4 && routeCost competingExact == 4
      && routeSteps competingFast == [UseTransport "CHEAP_STAGE" cheapStage, UseTransport "CHEAP_EXIT" cheapOutside, UseTransport "WILD_20" ordinaryTarget])

  bankAstar <- mustRight =<< buildTileAStarWithPolicy (syntheticPolicy deep) bankWorld
  let bankReference = ReferenceDijkstra (tileTopology bankAstar)
      bankQuery = (defaultQuery deep ordinaryTarget)
        { enabledTransportTypes = Set.singleton "BANK_WILD_20"
        , requirementMode = ConfiguredRequirements (emptyAccountState {accountBank = Map.singleton "999" 1})
        }
      bankFast = findRouteTileAStar bankAstar bankQuery
      bankExact = findRouteReferenceDijkstra bankReference bankQuery
  assertMsg ("banked global Wilderness mismatch: " <> show (routeCost bankFast, routeCost bankExact, routeSteps bankFast))
    (routeCost bankFast == 83 && routeCost bankExact == 83
      && length (filter isWalk (routeSteps bankFast)) == 82
      && last (routeSteps bankFast) == UseTransport "BANK_WILD_20" ordinaryTarget)
 where
  outside = packTile 3000 3678 0
  mid = packTile 3000 3759 0
  deep30 = packTile 3000 3760 0
  deep = packTile 3000 3761 0
  deepNeighbor = packTile 3000 3762 0
  ordinaryTarget = packTile 500 500 0
  restrictedTarget = packTile 600 600 0
  cheapStage = packTile 3010 3800 0
  cheapOutside = packTile 3010 3600 0
  corridor = [packTile 3000 y 0 | y <- [3678 .. 3762]]
  ordinary = globalAt "WILD_20" ordinaryTarget 1 20 Nothing
  restricted = globalAt "WILD_30" restrictedTarget 1 30 Nothing
  exit = local "WILD_EXIT" deep outside 3
  world = withEmptySeparatorArtifact (World (collisionMap (ordinaryTarget : restrictedTarget : corridor))
    (Map.singleton deep [exit]) [ordinary, restricted] Set.empty Nothing)
  competingWorld = withEmptySeparatorArtifact (World
    (collisionMap [deep, cheapStage, cheapOutside, ordinaryTarget])
    (Map.fromList
      [ (deep, [local "EXPENSIVE_EXIT" deep outside 10, local "CHEAP_STAGE" deep cheapStage 2])
      , (cheapStage, [local "CHEAP_EXIT" cheapStage cheapOutside 1])
      ]) [ordinary] Set.empty Nothing)
  bankWorld = withEmptySeparatorArtifact (World (collisionMap (ordinaryTarget : corridor)) Map.empty
    [globalAt "BANK_WILD_20" ordinaryTarget 1 20 (Just (ItemOne (ItemTerm "999" 1)))]
    (Set.singleton deep) Nothing)
  isWalk (Walk _) = True
  isWalk _ = False

checkBankGlobalHub :: IO ()
checkBankGlobalHub = do
  astar <- mustRight =<< buildTileAStarWithPolicy
    (StructuralReachabilityPolicy [bankA, bankB] Set.empty) world
  let account = compileRoutingAccount astar (routingOptionsFromQuery enabledQuery)
      graph = compiledSiteGraph account
      spatialCount = Vector.length (siteTiles graph)
      hub = spatialCount
      hubState = stateId hub True
      bankNodes = map (siteNode graph) [bankA, bankB]
      destinationNodes = map (siteNode graph) [destinationA, destinationB]
      hubIncoming = siteReverseEdges graph Boxed.! hubState
      incidentEdges = sum
        [ length
            [ ()
            | (from, _, _) <- Vector.toList edges
            , from `div` 2 == hub || to `div` 2 == hub
            ]
        | (to, edges) <- zip [0 ..] (Boxed.toList (siteReverseEdges graph))
        ]
  assertMsg "bank globals did not create exactly one abstract capability node"
    (siteAbstractNodes graph == Boxed.singleton BankedGlobalTeleports)
  assertMsg "abstract node was inserted among coordinate-bearing sites"
    (hub == Vector.length (siteTiles graph)
      && Boxed.length (siteComponents graph) == spatialCount
      && hub `notElem` IntMap.elems (siteTileIndex graph))
  assertMsg "banks do not share the hub with zero-cost explicit ingress"
    (Set.fromList (Vector.toList hubIncoming)
      == Set.fromList [(stateId bank False, 0, True) | bank <- bankNodes])
  assertMsg "bank/global graph did not factor to B + distinct destinations"
    (incidentEdges == length bankNodes + length destinationNodes)
  mapM_ (checkDestinationEdge graph hubState)
    [(destinationA, 3), (destinationB, 5)]

  mapM_ (checkOldEquivalence astar account graph bankNodes)
    [(destinationA, 3), (destinationB, 5)]

  let noGlobals = compiledSiteGraph (compileRoutingAccount astar
        (routingOptionsFromQuery (enabledQuery {enabledTransportTypes = Set.singleton "MISSING"})))
      noBankingQuery = enabledQuery {bankPathEnabled = False}
      noBanking = compiledSiteGraph (compileRoutingAccount astar (routingOptionsFromQuery noBankingQuery))
      reference = ReferenceDijkstra (tileTopology astar)
  assertMsg "account with no banked globals received a hub" (Boxed.null (siteAbstractNodes noGlobals))
  assertMsg "bank-disabled account received a hub" (Boxed.null (siteAbstractNodes noBanking))
  assertMsg "banked global did not require the banking transition"
    (routeCost (findRouteTileAStar astar enabledQuery) == 3
      && routeCost (findRouteReferenceDijkstra reference enabledQuery) == 3
      && routeCost (findRouteTileAStar astar noBankingQuery) == maxBound)

  let bankTarget = targetOverlay astar account bankB
      bankDistances = reverseDijkstraUncounted graph bankTarget
  assertMsg "hub made disconnected banks mutually reachable"
    (bankDistances Vector.! stateId (siteNode graph bankA) False == maxBound)

  singleAstar <- mustRight =<< buildTileAStarWithPolicy (syntheticPolicy bankA)
    (world {worldBanks = Set.singleton bankA, worldGlobalTeleports = [teleportB]})
  let singleGraph = compiledSiteGraph (compileRoutingAccount singleAstar
        (routingOptionsFromQuery (enabledQuery {enabledTransportTypes = Set.singleton "GLOBAL_B"})))
      singleHub = Vector.length (siteTiles singleGraph)
      singleIncident = sum
        [ length [() | (from, _, _) <- Vector.toList edges, from `div` 2 == singleHub || to `div` 2 == singleHub]
        | (to, edges) <- zip [0 ..] (Boxed.toList (siteReverseEdges singleGraph))
        ]
  assertMsg "one-bank/one-global graph was not represented by two hub edges"
    (siteAbstractNodes singleGraph == Boxed.singleton BankedGlobalTeleports && singleIncident == 2)
 where
  bankA = packTile 300 300 0
  bankB = packTile 400 400 0
  destinationA = packTile 500 500 0
  destinationB = packTile 600 600 0
  bankItem = Just (ItemOne (ItemTerm "999" 1))
  teleportA7 = global "GLOBAL_A_SLOW" destinationA 7 bankItem
  teleportA3 = global "GLOBAL_A_FAST" destinationA 3 bankItem
  teleportB = global "GLOBAL_B" destinationB 5 bankItem
  world = withEmptySeparatorArtifact
    (World (collisionMap [bankA, bankB, destinationA, destinationB]) Map.empty
      [teleportA7, teleportA3, teleportB] (Set.fromList [bankA, bankB]) Nothing)
  enabledQuery = (defaultQuery bankA destinationA)
    { enabledTransportTypes = Set.fromList ["GLOBAL_A_SLOW", "GLOBAL_A_FAST", "GLOBAL_B"]
    , bankPathEnabled = True
    , requirementMode = ConfiguredRequirements
        (emptyAccountState {accountBank = Map.singleton "999" 1})
    }
  siteNode graph tile = IntMap.findWithDefault (error ("missing site " <> show tile))
    (unTile tile) (siteTileIndex graph)
  checkDestinationEdge graph hubState (tile, expectedCost) = do
    let edges = siteReverseEdges graph Boxed.! stateId (siteNode graph tile) True
        matching = [(cost, starts) | (from, cost, starts) <- Vector.toList edges, from == hubState]
    assertMsg ("wrong deduplicated hub edge for " <> show tile)
      (matching == [(expectedCost, False)])
    assertMsg ("old direct bank/global edge retained for " <> show tile)
      (all (\(from, _, _) -> from `div` 2 `notElem` map (siteNode graph) [bankA, bankB]) (Vector.toList edges))
  checkOldEquivalence astar account graph bankNodes (target, expectedCost) = do
    let spatialCount = Vector.length (siteTiles graph)
        oldDirect =
          [ (stateId bank False, stateId (siteNode graph destination) True, cost, True)
          | bank <- bankNodes
          , (destination, cost) <- [(destinationA, 7), (destinationA, 3), (destinationB, 5)]
          ]
        retained =
          [ (from, to, cost, starts)
          | to <- [0 .. spatialCount * 2 - 1]
          , (from, cost, starts) <- Vector.toList (siteReverseEdges graph Boxed.! to)
          , from `div` 2 < spatialCount
          ]
        oldEdges = retained <> oldDirect
        oldGraph = graph
          { siteAbstractNodes = Boxed.empty
          , siteReverseEdges = Boxed.generate (spatialCount * 2) $ \to -> Vector.fromList
              [(from, cost, starts) | (from, edgeTo, cost, starts) <- oldEdges, edgeTo == to]
          }
        overlay = targetOverlay astar account target
        oldDistances = reverseDijkstraUncounted oldGraph overlay
        newDistances = reverseDijkstraUncounted graph overlay
        (newManhattan, _) = reverseDijkstraManhattan graph overlay
        oldManhattan = reverseDijkstraManhattanUncounted oldGraph overlay
        hubState = stateId spatialCount True
        targetState = stateId (siteNode graph target) True
    assertMsg ("hub labels differ from complete bipartite graph for " <> show target)
      (Vector.take (spatialCount * 2) newDistances == oldDistances)
    assertMsg ("hub Manhattan labels differ from complete bipartite graph for " <> show target)
      (Vector.take (spatialCount * 2) (manhattanDistances newManhattan) == manhattanDistances oldManhattan)
    assertMsg ("hub generator origins differ from complete bipartite graph for " <> show target)
      (Vector.take (spatialCount * 2) (manhattanGeneratorOrigins newManhattan)
        == manhattanGeneratorOrigins oldManhattan)
    assertMsg ("hub generator weights differ from complete bipartite graph for " <> show target)
      (Vector.take (spatialCount * 2) (manhattanGeneratorWeights newManhattan)
        == manhattanGeneratorWeights oldManhattan)
    assertMsg "abstract destination leg introduced a generator boundary"
      (manhattanGeneratorOrigins newManhattan Vector.! hubState == targetState)
    mapM_ (\bank -> assertMsg "bank hub ingress misplaced generator provenance"
      (manhattanGeneratorOrigins newManhattan Vector.! stateId bank False == stateId bank False
        && manhattanGeneratorWeights newManhattan Vector.! stateId bank False == expectedCost * 2)) bankNodes
    assertMsg "already-banked bank state gained a bank-global transition"
      (newDistances Vector.! stateId (siteNode graph bankA) True == maxBound)

checkGeneratorScanKernel :: IO ()
checkGeneratorScanKernel = do
  mapM_ checkSize [0, 1, 2, 3, 4, 5, 7, 8, 15, 16, 31, 32, 612]
  let overflow = generatorScanFromVector (Vector.singleton (unTile (packTile 0 0 0), maxBound - 1))
  assertMsg "SIMD overflow fallback" (scanGeneratorsSimd overflow 2 2 == maxBound)
 where
  queries = [(0, 0), (100, 200), (32767, 32767), (12345, 23456)]
  checkSize size = do
    let generators = Vector.generate size $ \ix ->
          ( unTile (packTile ((ix * 101) `mod` 32768) ((ix * 211) `mod` 32768) 0)
          , (ix * 17) `mod` 100000
          )
        bucket = generatorScanFromVector generators
    mapM_ (checkQuery bucket) queries
  checkQuery bucket (x, y) = do
    let expected = scanGeneratorsScalar bucket x y
        context = "generator scan size=" <> show (generatorScanLength bucket) <> " query=" <> show (x, y)
    assertMsg ("SIMD mismatch: " <> context) (scanGeneratorsSimd bucket x y == expected)
    assertMsg ("selected mismatch: " <> context) (scanGenerators bucket x y == expected)

checkManhattanGeneratorProvenance :: IO ()
checkManhattanGeneratorProvenance = do
  checkOneEntry
  checkTwoEntries
 where
  componentTiles = [packTile 100 y 0 | y <- [100 .. 106]]
  a = packTile 100 100 0
  b = packTile 100 102 0
  midpoint = packTile 100 103 0
  c = packTile 100 104 0
  d = packTile 100 106 0
  target = packTile 200 200 0
  policy = syntheticPolicy a

  checkOneEntry = do
    astar <- mustRight =<< buildTileAStarWithPolicy policy
      (withEmptySeparatorArtifact (World (collisionMap componentTiles) Map.empty [] (Set.fromList [a, b, c, d]) Nothing))
    heuristic <- prepareManhattan astar (walkingQuery d a)
    let cid = componentId astar a
    assert (steinerCount astar > 0)
    assert (Vector.length (heuristicSeeds heuristic Boxed.! seedKey cid False) == 4)
    assert (Vector.length (heuristicGenerators heuristic Boxed.! seedKey cid False) == 1)
    assert (Vector.length (heuristicGenerators heuristic Boxed.! seedKey cid True) == 1)
    assert (heuristicGeneratorCount heuristic == 2)
    assert (heuristicMaxGeneratorsPerComponent heuristic == 2)
    assert ((heuristicGeneratorsPerComponentP50 heuristic, heuristicGeneratorsPerComponentP90 heuristic,
      heuristicGeneratorsPerComponentP95 heuristic, heuristicGeneratorsPerComponentP99 heuristic) == (2, 2, 2, 2))
    assert ((heuristicGeneratorSeedRatioP50 heuristic, heuristicGeneratorSeedRatioP90 heuristic,
      heuristicGeneratorSeedRatioP95 heuristic, heuristicGeneratorSeedRatioP99 heuristic,
      heuristicGeneratorSeedRatioMax heuristic) == (0.25, 0.25, 0.25, 0.25, 0.25))
    assertExact heuristic cid componentTiles

  checkTwoEntries = do
    let transports = Map.fromList
          [ (a, [local "GENERATOR_LEFT" a target 1])
          , (d, [local "GENERATOR_RIGHT" d target 1])
          ]
        world = withEmptySeparatorArtifact (World (collisionMap (target : componentTiles)) transports [] (Set.fromList [b, midpoint, c]) Nothing)
        routeQuery = query a target (Set.fromList ["GENERATOR_LEFT", "GENERATOR_RIGHT"]) False
    astar <- mustRight =<< buildTileAStarWithPolicy policy world
    heuristic <- prepareManhattan astar routeQuery
    let cid = componentId astar a
        unbanked = heuristicGenerators heuristic Boxed.! seedKey cid False
        banked = heuristicGenerators heuristic Boxed.! seedKey cid True
        cone x (generatorTile, weight) = weight + chebyshevPacked x generatorTile
    assert (steinerCount astar > 0)
    assert (Vector.length (heuristicSeeds heuristic Boxed.! seedKey cid False) > 2)
    assert (Vector.length unbanked == 2)
    let left = unbanked Vector.! 0
        right = unbanked Vector.! 1
    assert (Set.fromList (map fst (Vector.toList unbanked)) == Set.fromList (map unTile [a, d]))
    assert (Set.fromList (map fst (Vector.toList banked)) == Set.fromList (map unTile [a, d]))
    assert (heuristicGeneratorCount heuristic == 6)
    assert (heuristicMaxGeneratorsPerComponent heuristic == 4)
    assert (cone (unTile a) left < cone (unTile a) right)
    assert (cone (unTile d) right < cone (unTile d) left)
    assert (cone (unTile midpoint) left == cone (unTile midpoint) right)
    assertExact heuristic cid componentTiles

  prepareManhattan astar routeQuery =
    prepareHeuristicProfiled (TileAStarConfig SparseWalkingReverse True True) astar
      (compileRoutingAccount astar (routingOptionsFromQuery routeQuery)) (queryTarget routeQuery)
  componentId astar tile = maybe (error "missing generator-test component") id
    (componentOfTile (topologyNaturalComponents (tileTopology astar)) tile)
  steinerCount astar = let (_, count, _, _) = tileStaticStats astar in count
  assertExact heuristic cid = mapM_ (\tile -> mapM_ (\banked ->
    assert (heuristicAtComponent heuristic (unTile tile) cid banked
      == heuristicAtGeneratorsComponent heuristic (unTile tile) cid banked)) [False, True])

checkInstrumentation :: TileAStar -> Tiles -> IO ()
checkInstrumentation tileAStar tiles = do
  let routeQuery = walkingQuery (tA0 tiles) (tA1 tiles)
  (_, timings) <- findRouteProfiledTileAStar tileAStar routeQuery
  (_, cliqueTimings) <- findRouteProfiledTileAStarWithConfig
    (TileAStarConfig CliqueReverse False True) tileAStar routeQuery
  (_, sparseTimings) <- findRouteProfiledTileAStarWithConfig
    (TileAStarConfig SparseWalkingReverse True True) tileAStar routeQuery
  let forward = tileSearchCounters timings
      reverseCounters = tileReverseCounters cliqueTimings
  let forwardScans = (tileHeuristicCalls forward, tileHeuristicCandidatesScanned forward, tileHeuristicMaxCandidatesPerCall forward)
  assertMsg ("default heuristic scan counters: " <> show forwardScans) (forwardScans == (5, 5, 1))
  assert ((reverseStatesPopped reverseCounters, reverseEdgesRelaxed reverseCounters, reversePqPushes reverseCounters,
    reverseStalePqEntries reverseCounters, reversePqMaxSize reverseCounters) == (8, 24, 8, 0, 6))
  assert ((tileHeuristicSeedCount timings, tileHeuristicComponentCount timings, tileHeuristicMaxSeedsPerComponent timings) == (8, 1, 8))
  assert ((tileHeuristicSeedsPerComponentP50 timings, tileHeuristicSeedsPerComponentP90 timings,
    tileHeuristicSeedsPerComponentP95 timings, tileHeuristicSeedsPerComponentP99 timings) == (8, 8, 8, 8))
  let sparseReverse = tileReverseCounters sparseTimings
  assert ((reverseStatesPopped sparseReverse, reverseEdgesRelaxed sparseReverse, reversePqPushes sparseReverse,
    reverseStalePqEntries sparseReverse, reversePqMaxSize sparseReverse) == (24, 52, 26, 2, 6))
  assert (tileReverseCounters timings == TileReverseCounters 0 0 0 0 0 0 0 0 0 0 0)
  assert (tileHeuristicGeneratorCount timings == tileHeuristicGeneratorCount sparseTimings)

checkMultiplePointAttachments :: IO ()
checkMultiplePointAttachments = do
  let left = packTile 10 10 0
      point = packTile 11 10 0
      right = packTile 12 10 0
      dead = packTile 20 20 0
      bridge = local "SYNTHETIC_SHARED_POINT" point dead 1
      shared = local "SYNTHETIC_SHARED_POINT_2" point left 1
      world = withEmptySeparatorArtifact (World (collisionMap [left, right]) (Map.singleton point [bridge, shared]) [] Set.empty Nothing)
      routeQuery = query left right (Set.singleton "SYNTHETIC_SHARED_POINT") False
  tileAStar <- mustRight =<< buildTileAStarWithPolicy (syntheticPolicy left) world
  let reference = ReferenceDijkstra (tileTopology tileAStar)
      attachments = pointAttachments (tileTopology tileAStar) point
      referenceRoute = findRouteReferenceDijkstra reference routeQuery
      tileRoute = findRouteTileAStar tileAStar routeQuery
      account = compileRoutingAccount tileAStar (routingOptionsFromQuery routeQuery)
      heuristic = prepareHeuristic tileAStar account (queryTarget routeQuery)
      overlay = targetOverlay tileAStar account point
      graph = compiledSiteGraph account
  assertMsg ("attachments: " <> show attachments) (length attachments == 2)
  assertMsg ("reference route: " <> show referenceRoute) (routeCost referenceRoute == 2)
  assertMsg ("tile route: " <> show tileRoute) (routeCost tileRoute == 2)
  assertMsg "reverse shared-point route" (routeCost (findRouteTileAStar tileAStar (query right left (Set.singleton "SYNTHETIC_SHARED_POINT") False)) == 2)
  assertMsg "multi-component target attachments were lost"
    (Vector.length (targetComponents overlay) == 2 && Vector.length (targetAttachmentSites overlay) == 2)
  let (manhattan, _) = reverseDijkstraManhattan graph overlay
  assertReverseLabelsEqual graph overlay (fst (reverseDijkstra graph overlay)) (halveDistances (manhattanDistances manhattan))
  assertResolvedHeuristic tileAStar heuristic point

checkHeuristicLookup :: TileAStar -> Tiles -> IO ()
checkHeuristicLookup tileAStar tiles = do
  let routeQuery = query (tA3 tiles) (tC0 tiles)
        (Set.fromList ["SYNTHETIC_BOAT", "SYNTHETIC_RING", "SYNTHETIC_X_1", "SYNTHETIC_X_2", "SYNTHETIC_DEAD_END"]) True
      heuristic = prepareHeuristic tileAStar (compileRoutingAccount tileAStar (routingOptionsFromQuery routeQuery)) (queryTarget routeQuery)
      samples = [tA0 tiles, tA1 tiles, tE0 tiles, tD1 tiles, tX tiles, tY tiles, tUnknown tiles]
  assertMsg "start leaked into target heuristic graph"
    (IntMap.notMember (unTile (queryStart routeQuery)) (heuristicSiteIndex heuristic))
  mapM_ (assertResolvedHeuristic tileAStar heuristic) samples
  mapM_ (assertComponentHeuristic tileAStar heuristic) [tA0 tiles, tA1 tiles, tD1 tiles]

checkPreparationLifetimes :: TileAStar -> Tiles -> IO ()
checkPreparationLifetimes tileAStar tiles = do
  let base = query (tA0 tiles) (tD1 tiles) (Set.singleton "SYNTHETIC_GLOBAL") False
      account = compileRoutingAccount tileAStar (routingOptionsFromQuery base)
      target = prepareTarget tileAStar account (queryTarget base)
      fromA0 = searchPrepared tileAStar account target (tA0 tiles) (searchOptionsFromQuery base)
      moved = base {queryStart = tA3 tiles}
      fromA3 = searchPrepared tileAStar account target (tA3 tiles) (searchOptionsFromQuery moved)
      otherTarget = prepareTarget tileAStar account (tA25 tiles)
      relevant = compileRoutingAccount tileAStar (routingOptionsFromQuery (withoutInventory base))
      irrelevantAccount = syntheticAccount {accountBank = Map.insert "unrelated" 1 (accountBank syntheticAccount)}
      irrelevantQuery = base {requirementMode = ConfiguredRequirements irrelevantAccount}
      irrelevant = compileRoutingAccount tileAStar (routingOptionsFromQuery irrelevantQuery)
  (_, warmTimings) <- searchPreparedProfiled tileAStar account target (tA0 tiles) (searchOptionsFromQuery base)
  assertMsg "prepared search disagrees with one-shot start A0" (fromA0 == findRouteTileAStar tileAStar base)
  assertMsg "prepared target was not reusable from a moved start" (fromA3 == findRouteTileAStar tileAStar moved)
  assertMsg "target preparation retained the wrong target" (preparedTargetTile otherTarget == tA25 tiles)
  assertMsg "prepared search repeated setup work"
    (tileAccountPrepareMilliseconds warmTimings == 0
      && tileTargetPrepareMilliseconds warmTimings == 0
      && tileReverseDijkstraMilliseconds warmTimings == 0)
  assertMsg "relevant account change kept routing fingerprint"
    (compiledRoutingFingerprint relevant /= compiledRoutingFingerprint account)
  assertMsg "irrelevant account change altered routing fingerprint"
    (compiledRoutingFingerprint irrelevant == compiledRoutingFingerprint account)

checkTargetOverlay :: TileAStar -> Tiles -> IO ()
checkTargetOverlay astar tiles = do
  let routeQuery = walkingQuery (tA0 tiles) (tA3 tiles)
      account = compileRoutingAccount astar (routingOptionsFromQuery routeQuery)
      graph = compiledSiteGraph account
      overlay = targetOverlay astar account (tA3 tiles)
      staticOverlay = targetOverlay astar account (tD1 tiles)
      blockedOverlay = targetOverlay astar account (tUnknown tiles)
      attachments = targetAttachmentSites overlay
      attachmentIds = Vector.map fst attachments
      (manhattan, counters) = reverseDijkstraManhattan graph overlay
      staticManhattan = reverseDijkstraManhattanUncounted graph staticOverlay
      blockedDistances = reverseDijkstraUncounted graph blockedOverlay
      clique = fst (reverseDijkstra graph overlay)
      sparse = halveDistances (manhattanDistances manhattan)
  assertMsg "dynamic target was inserted into static graph"
    (IntMap.notMember (targetPacked overlay) (siteTileIndex graph))
  assertMsg "ordinary target did not receive a synthetic reverse site"
    (targetSynthetic overlay && targetSite overlay == routingNodeCount graph)
  assertMsg "static target received a redundant synthetic reverse site"
    (not (targetSynthetic staticOverlay) && targetSite staticOverlay < Vector.length (siteTiles graph))
  assertMsg "blocked transport target was not retained as a static site"
    (not (targetSynthetic blockedOverlay) && Vector.null (targetComponents blockedOverlay))
  assertMsg "target attachment set was too small for the shared component"
    (Vector.length attachments >= 3)
  assertMsg "target attachment sites were not deduplicated"
    (Set.size (Set.fromList (Vector.toList attachmentIds)) == Vector.length attachmentIds)
  Vector.forM_ attachments $ \(site, cost) ->
    assertMsg "target attachment cost mismatch"
      (cost == chebyshevPacked (targetPacked overlay) (siteTiles graph Vector.! site))
  assertReverseLabelsEqual graph overlay clique sparse
  mapM_ (\banked -> do
    let targetState = stateId (targetSite overlay) banked
    assertMsg "target layer lost generator provenance"
      (manhattanGeneratorOrigins manhattan Vector.! targetState == targetState)
    Vector.forM_ attachmentIds $ \site ->
      assertMsg "walking target attachment changed generator provenance"
        (manhattanGeneratorOrigins manhattan Vector.! stateId site banked == targetState)) [False, True]
  mapM_ (\banked -> do
    let staticState = stateId (targetSite staticOverlay) banked
        blockedState = stateId (targetSite blockedOverlay) banked
    assertMsg "static target layer changed generator provenance"
      (manhattanGeneratorOrigins staticManhattan Vector.! staticState == staticState)
    assertMsg "blocked target layer was not seeded"
      (blockedDistances Vector.! blockedState == 0)) [False, True]
  assertMsg "static sites still scanned component lists for the query target"
    (reverseTotalSitesScanned counters == Vector.length attachments * 2)

assertResolvedHeuristic :: TileAStar -> Heuristic -> Tile -> IO ()
assertResolvedHeuristic tileAStar heuristic tile =
  mapM_ check [False, True]
 where
  topology = tileTopology tileAStar
  packed = unTile tile
  attachments = Vector.fromList (structurallyReachablePointAttachments topology tile)
  site = IntMap.findWithDefault (-1) packed (heuristicSiteIndex heuristic)
  check banked = assertMsg ("resolved heuristic mismatch at " <> show tile <> " banked=" <> show banked)
    (heuristicAt topology heuristic tile banked == heuristicAtResolved heuristic packed attachments site banked)

assertComponentHeuristic :: TileAStar -> Heuristic -> Tile -> IO ()
assertComponentHeuristic tileAStar heuristic tile =
  case componentOfTile (topologyNaturalComponents topology) tile of
    Nothing -> fail ("missing component for " <> show tile)
    Just cid -> mapM_ (\banked -> assertMsg ("component heuristic mismatch at " <> show tile <> " banked=" <> show banked)
      (heuristicAt topology heuristic tile banked == heuristicAtComponent heuristic (unTile tile) cid banked)) [False, True]
 where
  topology = tileTopology tileAStar

collisionMap :: [Tile] -> CollisionMap
collisionMap tiles = CollisionMap (Map.fromList [(region, bytes region) | region <- Set.toList (Set.fromList (map tileRegion tiles))])
 where
  tileRegion tile = let (x, y, _) = unpackTile tile in (x `div` 64, y `div` 64)
  bytes region = BL.pack [byteAt region ix | ix <- [0 .. 8191]]
  byteAt region ix = foldr set 0 [bit | tile <- tiles, tileRegion tile == region, let bit = collisionBit tile, bit `div` 8 == ix]
  set bit value = setBit value (bit `mod` 8)
  collisionBit tile = ((plane * 4096 + (y `mod` 64) * 64 + (x `mod` 64)) * 2)
   where (x, y, plane) = unpackTile tile

syntheticPolicy :: Tile -> StructuralReachabilityPolicy
syntheticPolicy seed = StructuralReachabilityPolicy [seed] Set.empty

mustRight :: Show e => Either e a -> IO a
mustRight = either (fail . show) pure

synthetic :: (World, Tiles)
synthetic =
  ( withEmptySeparatorArtifact (World (CollisionMap (Map.singleton (1, 1) collisionBytes)) transports globals banks Nothing)
  , Tiles a0 a1 a3 a8 a25 b1 c0 d0 d1 e0 s0 s2 unknown xSite ySite
  )
 where
  a0 = packTile 100 100 0
  a1 = packTile 101 100 0
  a3 = packTile 103 100 0
  a8 = packTile 108 100 0
  a25 = packTile 125 100 0
  s0 = packTile 100 99 0
  s1 = packTile 101 99 0
  s2 = packTile 102 99 0
  b0 = packTile 103 99 0
  b1 = packTile 104 99 0
  c0 = packTile 110 110 0
  c1 = packTile 111 110 0
  d0 = packTile 110 111 0
  d1 = packTile 111 111 0
  e0 = packTile 109 110 0
  unknown = packTile 200 200 0
  xSite = packTile 210 210 0
  ySite = packTile 220 220 0
  allEdges =
    [(packTile x 100 0, 1) | x <- [100 .. 124]]
      <> [(s0, 0), (s0, 1), (s1, 1), (s2, 1), (b0, 1)]
      <> [(c0, 1), (c0, 0), (d0, 0), (d1, 0)]
  collisionBytes = BL.pack [byteAt i | i <- [0 .. 8191]]
  byteAt i = foldr setBitIf 0 [bitIndex | (bit, bitIndex) <- flags, bit `div` 8 == i]
  setBitIf bitIndex byte = setBit byte (bitIndex `mod` 8)
  flags = [(flagOffset tile flag, flagOffset tile flag `mod` 8) | (tile, flag) <- allEdges]
  flagOffset tile flag =
    let (x, y, p) = unpackTile tile
     in ((p * 4096 + (y - 64) * 64 + (x - 64)) * 2 + flag)
  transports = Map.fromListWith (<>)
    [ (a0, [ local "SYNTHETIC_DIRECT" a0 a1 10
           , local "SYNTHETIC_LONG" a0 d1 20
           , localReq "SYNTHETIC_BANK_LOCAL_AT_BANK" a0 d1 5 (ItemOne (ItemTerm "999" 1))
           , local "SYNTHETIC_UNKNOWN" a0 unknown 1
           , local "SYNTHETIC_X_1" a0 xSite 5
           , local "SYNTHETIC_DEAD_END" a0 ySite 1
           ])
    , (xSite, [local "SYNTHETIC_X_2" xSite c0 7])
    , (b1, [local "SYNTHETIC_BOAT" b1 c0 2])
    , (c1, [local "SYNTHETIC_RETURN" c1 a25 1])
    , (e0, [local "SYNTHETIC_RING" e0 c0 2, localReq "SYNTHETIC_BANK_LOCAL" e0 d1 5 (ItemOne (ItemTerm "999" 1))])
    ]
  globals =
    [ global "SYNTHETIC_GLOBAL" d1 4 (Just (ItemOne (ItemTerm "13393" 1)))
    , global "SYNTHETIC_BANK_GLOBAL" d1 3 (Just (ItemOne (ItemTerm "999" 1)))
    ]
  banks = Set.singleton a0

data Tiles = Tiles
  { tA0 :: Tile, tA1 :: Tile, tA3 :: Tile, tA8 :: Tile, tA25 :: Tile
  , tB1 :: Tile, tC0 :: Tile, tD0 :: Tile, tD1 :: Tile, tE0 :: Tile
  , tS0 :: Tile, tS2 :: Tile, tUnknown :: Tile, tX :: Tile, tY :: Tile
  }

local :: String -> Tile -> Tile -> Int -> Transport
local kind from to cost = Transport kind (Just from) (Just to) cost kind "" False Nothing [] Nothing [] [] [] "synthetic"

localReq :: String -> Tile -> Tile -> Int -> ItemExpr -> Transport
localReq kind from to cost itemReq = Transport kind (Just from) (Just to) cost kind "" False Nothing [] (Just itemReq) [] [] [] "synthetic"

global :: String -> Tile -> Int -> Maybe ItemExpr -> Transport
global kind to cost itemReq = Transport kind Nothing (Just to) cost kind "" False Nothing [] itemReq [] [] [] "synthetic"

globalAt :: String -> Tile -> Int -> Int -> Maybe ItemExpr -> Transport
globalAt kind to cost wilderness itemReq =
  (global kind to cost itemReq) {maxWildernessLevel = Just wilderness}

data Case = Case String Query Expect
data Expect = Reachable | Unreachable

cases :: Tiles -> [Case]
cases t =
  [ Case "same tile" (query (tA0 t) (tA0 t) Set.empty True) Reachable
  , Case "same leaf direct" (query (tA3 t) (tA8 t) Set.empty False) Reachable
  , Case "separator start target" (query (tS0 t) (tS2 t) Set.empty False) Reachable
  , Case "cross-region walking" (query (tA0 t) (tB1 t) Set.empty False) Reachable
  , Case "directed local transport" (query (tB1 t) (tC0 t) (Set.singleton "SYNTHETIC_BOAT") False) Reachable
  , Case "global teleport" (query (tA0 t) (tD1 t) (Set.singleton "SYNTHETIC_GLOBAL") False) Reachable
  , Case "missing inventory blocks global teleport" (withoutInventory (query (tA0 t) (tD1 t) (Set.singleton "SYNTHETIC_GLOBAL") False)) Unreachable
  , Case "default bank supplies missing global item" (withoutInventory (query (tA0 t) (tD1 t) (Set.singleton "SYNTHETIC_BANK_GLOBAL") True)) Reachable
  , Case "custom bank blocks missing global item" (withoutBank (withoutInventory (query (tA0 t) (tD1 t) (Set.singleton "SYNTHETIC_BANK_GLOBAL") True))) Unreachable
  , Case "blocked transport origin attachment" (query (tC0 t) (tE0 t) (Set.singleton "SYNTHETIC_RING") False) Reachable
  , Case "blocked transport destination exit" (walkingQuery (tE0 t) (tC0 t)) Reachable
  , Case "banking enabled" (query (tA0 t) (tA3 t) Set.empty True) Reachable
  , Case "leaf re-entry" (query (tA0 t) (tA25 t) (Set.fromList ["SYNTHETIC_BOAT", "SYNTHETIC_RETURN"]) False) Reachable
  , Case "unreachable by walking" (walkingQuery (tA0 t) (tD0 t)) Unreachable
  ]

query :: Tile -> Tile -> Set.Set String -> Bool -> Query
query start target enabled bank =
  (defaultQuery start target)
    { enabledTransportTypes = enabled
    , bankPathEnabled = bank
    , requirementMode = ConfiguredRequirements syntheticAccount
    }

syntheticAccount :: AccountState
syntheticAccount = emptyAccountState { accountInventory = Map.singleton "13393" 1, accountBank = Map.singleton "999" 1 }

withoutInventory :: Query -> Query
withoutInventory q = q { requirementMode = ConfiguredRequirements (syntheticAccount { accountInventory = Map.empty }) }

withoutBank :: Query -> Query
withoutBank q = q { requirementMode = ConfiguredRequirements (syntheticAccount { accountInventory = Map.empty, accountBank = Map.empty }) }

walkingQuery :: Tile -> Tile -> Query
walkingQuery start target = (query start target Set.empty False) { allowTransports = False }

checkRoute :: ReferenceDijkstra -> TileAStar -> World -> Case -> IO ()
checkRoute raw tileAStar world (Case name q expectation) = do
  let flat = findRouteReferenceDijkstra raw q
      tile = findRouteTileAStar tileAStar q
  (sparse, _) <- findRouteProfiledTileAStarWithConfig sparseConfig tileAStar q
  case expectation of
    Reachable -> do
      assert (routeCost flat < maxBound)
      assert (routeCost tile == routeCost flat)
      assert (routeCost sparse == routeCost flat)
      assert (concreteCost world q (routeSteps tile) == routeCost tile)
    Unreachable -> do
      assert (routeCost flat == maxBound)
      assert (routeCost tile == maxBound)
      assert (routeCost sparse == maxBound)
      assert (null (routeSteps tile))
  putStrLn (name <> ": " <> show (routeCost flat) <> " / " <> show (routeCost tile))
 where
  sparseConfig = TileAStarConfig SparseWalkingReverse True False

checkReversePathDebug :: TileAStar -> Tiles -> IO ()
checkReversePathDebug tileAStar tiles = do
  let initialQuery = query (tA0 tiles) (tD1 tiles) (Set.singleton "SYNTHETIC_GLOBAL") False
      initialDebug = reversePathDebug tileAStar initialQuery
  assertMsg ("initial global leaked: " <> show initialDebug) (all ((/= "SYNTHETIC_GLOBAL") . reverseEdgeLabel) (concatMap reverseStatePath (reverseDebugStates initialDebug)))
  let decline = reversePathDebug tileAStar (withoutInventory (query (tA0 tiles) (tD1 tiles) (Set.singleton "SYNTHETIC_BANK_LOCAL_AT_BANK") True))
      (declineUnbanked, declineBanked) = twoStates decline
  assertMsg ("decline unbanked: " <> show declineUnbanked) (map reverseEdgeType (reverseStatePath declineUnbanked) == ["bank", "transport"])
  assertMsg ("decline transitions: " <> show (reverseStatePath declineUnbanked)) (map transition (reverseStatePath declineUnbanked) == [(False, True), (True, True)])
  assertMsg ("decline banked: " <> show declineBanked) (map reverseEdgeType (reverseStatePath declineBanked) == ["transport"])
  assert (map transition (reverseStatePath declineBanked) == [(True, True)])
  let mixed = reversePathDebug tileAStar (withoutInventory (query (tA0 tiles) (tD1 tiles) (Set.singleton "SYNTHETIC_BANK_GLOBAL") True))
      (mixedUnbanked, mixedBanked) = twoStates mixed
  assertMsg ("mixed unbanked value: " <> show mixedUnbanked) (reverseStateDistance mixedUnbanked == reverseStateHeuristic mixedUnbanked)
  assertMsg ("mixed banked: " <> show mixedBanked) (reverseStateUnreachable mixedBanked)
  assertMsg ("mixed unbanked path: " <> show (reverseStatePath mixedUnbanked)) (map reverseEdgeType (reverseStatePath mixedUnbanked) == ["transport"])
  assertMsg ("mixed transitions: " <> show (reverseStatePath mixedUnbanked)) (map transition (reverseStatePath mixedUnbanked) == [(False, True)])
  let oneMissing = reversePathDebug tileAStar (withoutInventory (query (tE0 tiles) (tD1 tiles) (Set.singleton "SYNTHETIC_BANK_LOCAL") False))
      (missingUnbanked, missingBanked) = twoStates oneMissing
  assertMsg ("missing unbanked: " <> show missingUnbanked) (reverseStateUnreachable missingUnbanked)
  assertMsg ("missing banked: " <> show missingBanked) (not (reverseStateUnreachable missingBanked))
  assert (map reverseEdgeType (reverseStatePath missingBanked) == ["transport"])
 where
  transition edge = (reverseEdgeFromBanked edge, reverseEdgeToBanked edge)
  twoStates debug =
    case reverseDebugStates debug of
      [unbanked, banked] -> (unbanked, banked)
      states -> error ("expected two reverse debug states, got " <> show (length states))

checkHeuristicPruning :: TileAStar -> Tiles -> IO ()
checkHeuristicPruning tileAStar tiles = do
  let unknownQuery = (query (tA0 tiles) (tA25 tiles) (Set.singleton "SYNTHETIC_UNKNOWN") False)
      noSeedQuery = walkingQuery (tA0 tiles) (tD0 tiles)
  (unknownRoute, unknownTimings, _) <- findRouteProfiledTileAStarWithTrace False tileAStar unknownQuery
  (_, noSeedTimings, _) <- findRouteProfiledTileAStarWithTrace False tileAStar noSeedQuery
  let unknownCounters = tileSearchCounters unknownTimings
      noSeedCounters = tileSearchCounters noSeedTimings
  assert (routeCost unknownRoute == 25)
  assert (tileUnknownComponentPrunes unknownCounters > 0)
  assert (tileNoReverseSeedPrunes noSeedCounters > 0)

checkTransportOnlyEndpoint :: ReferenceDijkstra -> TileAStar -> Tiles -> IO ()
checkTransportOnlyEndpoint raw tileAStar tiles = do
  let targetQuery = query (tA0 tiles) (tUnknown tiles) (Set.singleton "SYNTHETIC_UNKNOWN") False
      rawRoute = findRouteReferenceDijkstra raw targetQuery
      tileRoute = findRouteTileAStar tileAStar targetQuery
      expected = [UseTransport "SYNTHETIC_UNKNOWN" (tUnknown tiles)]
  assert (routeCost rawRoute == 1)
  assert (routeCost tileRoute == 1)
  assert (routeSteps tileRoute == expected)

checkIntermediateTransportEndpoint :: ReferenceDijkstra -> TileAStar -> Tiles -> IO ()
checkIntermediateTransportEndpoint raw tileAStar tiles = do
  let enabled = Set.fromList ["SYNTHETIC_X_1", "SYNTHETIC_X_2", "SYNTHETIC_DEAD_END"]
      routeQuery = query (tA0 tiles) (tC0 tiles) enabled False
      rawRoute = findRouteReferenceDijkstra raw routeQuery
      tileRoute = findRouteTileAStar tileAStar routeQuery
      expected = [ UseTransport "SYNTHETIC_X_1" (tX tiles)
                 , UseTransport "SYNTHETIC_X_2" (tC0 tiles)
                 ]
      xDebug = reversePathDebug tileAStar (query (tX tiles) (tC0 tiles) enabled False)
      yDebug = reversePathDebug tileAStar (query (tY tiles) (tC0 tiles) enabled False)
  assert (routeCost rawRoute == 12)
  assert (routeSteps rawRoute == expected)
  assert (routeCost tileRoute == 12)
  assert (routeSteps tileRoute == expected)
  assert (reverseStateDistance (unbankedState xDebug) == 7)
  assert (reverseStateHeuristic (unbankedState xDebug) == 7)
  assert (reverseStateUnreachable (unbankedState yDebug))
 where
  unbankedState debug =
    case reverseDebugStates debug of
      [state, _] -> state
      states -> error ("expected two reverse states, got " <> show (length states))

concreteCost :: World -> Query -> [RouteStep] -> Int
concreteCost world q = snd . foldl step (queryStart q, 0)
 where
  step (current, total) routeStep =
    case routeStep of
      Walk next
        | next == current -> (next, total)
        | next `elem` walkingNeighborsRaw world current -> (next, total + 1)
        | otherwise -> error ("illegal reconstructed walk: " <> coordinateText current <> " -> " <> coordinateText next)
      UseTransport name next ->
        let costs = [duration t | t <- allTransports world, label t == name, destination t == Just next]
         in (next, total + must "transport step" (listHead costs))
  label transport = if null (displayInfo transport) then transportType transport else displayInfo transport

allTransports :: World -> [Transport]
allTransports world = concat (Map.elems (worldTransports world)) <> worldGlobalTeleports world

listHead :: [a] -> Maybe a
listHead [] = Nothing
listHead (x:_) = Just x

must :: String -> Maybe a -> a
must name = maybe (error name) id

assert :: Bool -> IO ()
assert True = pure ()
assert False = fail "synthetic pathfinder assertion failed"

assertMsg :: String -> Bool -> IO ()
assertMsg _ True = pure ()
assertMsg message False = fail message
