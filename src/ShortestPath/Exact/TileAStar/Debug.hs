module ShortestPath.Exact.TileAStar.Debug
  ( renderComponentTiles
  , renderHeuristicTiles
  , renderHeuristicTilesWithTransform
  , reversePathDebug
  ) where

import Control.Exception (evaluate)
import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Boxed
import qualified Data.Vector.Mutable as BoxedMutable
import qualified Data.Vector.Storable as Storable
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as Mutable
import Data.Word (Word8)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Storable (pokeByteOff)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.Printf (printf)

import ShortestPath.Exact.TileAStar.Heuristic
import ShortestPath.Exact.TileAStar.HeuristicScan (forceGeneratorScan)
import ShortestPath.Exact.TileAStar.Preprocessing
import ShortestPath.Exact.TileAStar.RelaxedGraph
import ShortestPath.Exact.TileAStar.ReverseSearch
import ShortestPath.Exact.TileAStar.Types
import ShortestPath.Internal.DistanceTransform
import ShortestPath.Internal.MutableHeap
import ShortestPath.Internal.RgbaTile
import ShortestPath.Internal.Timing
import ShortestPath.Pathfinder
import ShortestPath.Tile
import ShortestPath.Topology
import ShortestPath.Transport
import ShortestPath.World

reversePathDebug :: TileAStar -> Query -> ReversePathDebug
reversePathDebug astar q =
  ReversePathDebug (queryStart q) (queryTarget q) (map stateDebug [False, True])
 where
  availability = compiledTransportAvailability account
  account = compileRoutingAccount astar (routingOptionsFromQuery q)
  graph = compiledSiteGraph account
  overlay = targetOverlay astar account (queryTarget q)
  distances = reverseDijkstraUncounted graph overlay
  heuristic = heuristicFromDistances components graph overlay distances 0 0 emptyReverseCounters
  stateDebug banked =
    case bestSource banked of
      Nothing -> ReversePathState banked (Just source) maxBound 0 True []
      Just (node, distance, approach) ->
        let route = approachEdge banked node approach <> map (shiftCumulative approach) (forwardPath (stateId node banked))
         in ReversePathState banked (Just source) distance (maybe 0 id (heuristicAt topology heuristic source banked)) False route
  source = queryStart q
  bestSource banked = foldl choose Nothing candidates
   where
    exact = [(node, distances Vector.! stateId node banked, 0) | Just node <- [sourceSite]]
    attached =
      [ (node, total, walking)
      | cid <- routingPointAttachments topology source
      , node <- Vector.toList (siteComponentSiteIds graph Boxed.! cid)
      , let walking = chebyshevPacked (unTile source) (siteTiles graph Vector.! node)
            total = addCostDefault maxBound walking (distances Vector.! stateId node banked)
      ]
    directTarget =
      [ (targetSite overlay, walking, walking)
      | targetSynthetic overlay
      , Vector.any (`Vector.elem` targetComponents overlay) (Vector.fromList (routingPointAttachments topology source))
      , let walking = chebyshevPacked (unTile source) (targetPacked overlay)
      ]
    candidates = filter (\(_, total, _) -> total /= maxBound) (exact <> attached <> directTarget)
    sourceSite
      | unTile source == targetPacked overlay = Just (targetSite overlay)
      | otherwise = IntMap.lookup (unTile source) (siteTileIndex graph)
    choose Nothing candidate = Just candidate
    choose current@(Just (_, best, _)) candidate@(_, cost, _)
      | cost < best = Just candidate
      | otherwise = current
  approachEdge banked node cost
    | cost == 0 = []
    | otherwise = [ReversePathEdge source site banked banked "component-walk" "component walk" cost cost]
   where
    site = Tile (siteTile node)
  shiftCumulative approachCost edge = edge {reverseEdgeCumulativeCost = approachCost + reverseEdgeCumulativeCost edge}
  forwardPath sourceState = runST $ do
    best <- Mutable.replicate stateCount maxBound
    prevState <- Mutable.replicate stateCount maxBound
    prevEdge <- BoxedMutable.replicate stateCount Nothing
    queue <- heapNew (max 262144 (stateCount * 16))
    Mutable.write best sourceState 0
    heapPush queue 0 sourceState 0
    let go = do
          popped <- heapPop queue
          case popped of
            Nothing -> pure []
            Just (_, state, cost) -> do
              known <- Mutable.read best state
              if cost /= known
                then go
                else if stateIsTarget state
                  then reconstruct best prevState prevEdge state
                  else mapM_ (relax best prevState prevEdge queue cost state) (debugNeighbors state) >> go
    go
  staticSiteCount = Vector.length (siteTiles graph)
  stateCount = (staticSiteCount + if targetSynthetic overlay then 1 else 0) * 2
  stateIsTarget state = state `div` 2 == targetSite overlay
  siteTile node
    | targetSynthetic overlay && node == targetSite overlay = targetPacked overlay
    | otherwise = siteTiles graph Vector.! node
  relax best prevState prevEdge queue cost state edge =
    case addCost cost (debugEdgeCost edge) of
      Nothing -> pure ()
      Just newCost -> do
        known <- Mutable.read best (debugEdgeToState edge)
        when (newCost < known) $ do
          Mutable.write best (debugEdgeToState edge) newCost
          Mutable.write prevState (debugEdgeToState edge) state
          BoxedMutable.write prevEdge (debugEdgeToState edge) (Just edge)
          heapPush queue newCost (debugEdgeToState edge) newCost
  reconstruct best prevState prevEdge state = reverse <$> collect state
   where
    collect current = do
      edge <- BoxedMutable.read prevEdge current
      case edge of
        Nothing -> pure []
        Just debugEdge -> do
          total <- Mutable.read best current
          let public = ReversePathEdge
                (debugEdgeFromTile debugEdge) (debugEdgeToTile debugEdge)
                (debugEdgeFromBanked debugEdge) (debugEdgeToBanked debugEdge)
                (debugEdgeKind debugEdge) (debugEdgeLabel debugEdge)
                (debugEdgeCost debugEdge) total
          previous <- Mutable.read prevState current
          (public :) <$> collect previous

  debugNeighbors state = walkingEdges <> transportEdges <> bankEdges <> bankGlobalEdges
   where
    node = state `div` 2
    banked = odd state
    tile = Tile (siteTile node)
    walkingAttachments
      | targetSynthetic overlay && node == targetSite overlay = Vector.toList (targetAttachmentSites overlay)
      | otherwise =
          [ (other, chebyshevPacked (siteTile node) (siteTile other))
          | other <- Vector.toList (attachedSites graph node)
          , other /= node
          ] <> targetEdge
    targetEdge =
      [ (targetSite overlay, chebyshevPacked (siteTile node) (targetPacked overlay))
      | node < staticSiteCount
      , targetSynthetic overlay
      , Vector.any (`Vector.elem` targetComponents overlay) (siteComponents graph Boxed.! node)
      ]
    walkingEdges =
      [ DebugEdge (stateId other banked) tile (Tile (siteTile other)) banked banked "component-walk" "component walk" edgeCost
      | (other, edgeCost) <- walkingAttachments
      ]
    transportEdges =
      [ DebugEdge (stateId to banked) tile dst banked banked "transport" (transportLabel t) stepCost
      | node < staticSiteCount
      , allowTransports q
      , t <- preparedLocalTransportsAt availability banked tile
      , Just (dst, stepCost, _) <- [preparedTransport q t]
      , Just to <- [IntMap.lookup (unTile dst) (siteTileIndex graph)]
      ]
    bankEdges =
      [ DebugEdge (stateId node True) tile tile False True "bank" "bank" 0
      | node < staticSiteCount
      , bankTransitionAvailable q reachableBanks banked tile
      ]
    bankGlobalEdges =
      [ DebugEdge (stateId to True) tile dst False True "transport" (transportLabel t) stepCost
      | node < staticSiteCount
      , allowTransports q
      , bankTransitionAvailable q reachableBanks banked tile
      , t <- preparedGlobalTransports availability True
      , Just (dst, stepCost, _) <- [preparedTransport q t]
      , Just to <- [IntMap.lookup (unTile dst) (siteTileIndex graph)]
      ]
  reachableBanks = Set.filter (not . null . routingPointAttachments topology) (worldBanks world)
  topology = tileTopology astar
  world = topologyWorld topology
  components = topologyRoutingComponents topology

data DebugEdge = DebugEdge
  { debugEdgeToState :: !Int
  , debugEdgeFromTile :: !Tile
  , debugEdgeToTile :: !Tile
  , debugEdgeFromBanked :: !Bool
  , debugEdgeToBanked :: !Bool
  , debugEdgeKind :: String
  , debugEdgeLabel :: String
  , debugEdgeCost :: !Int
  }

renderHeuristicTiles :: TileAStar -> Query -> FilePath -> String -> IO HeuristicRender
renderHeuristicTiles = renderHeuristicTilesWithTransform False

renderHeuristicTilesWithTransform :: Bool -> TileAStar -> Query -> FilePath -> String -> IO HeuristicRender
renderHeuristicTilesWithTransform useCTransform astar q outputRoot urlRoot = do
  createDirectoryIfMissing True outputRoot
  prepared <- mapM (prepareLayer useCTransform) [("no-bank", "Banking disabled", False), ("bank", "Banking enabled", True)]
  let values = concatMap (map snd . layerPointsPrepared) prepared <> concatMap (map snd . layerSeedPointsPrepared) prepared
      minimumValue = minimumDefault 0 values
      maximumValue = maximumDefault 0 values
  layers <- concat <$> mapM (renderPreparedLayers minimumValue maximumValue) prepared
  pure (HeuristicRender imageTileSize layers)
 where
  components = topologyRoutingComponents (tileTopology astar)
  groups = componentTileGroups components
  transform useC box seeds =
    (if useC then chebyshevTransformC else chebyshevTransform) box seeds
  prepareLayer useC (key, title, banking) = do
    let q' = q {bankPathEnabled = banking}
    let account = compileRoutingAccount astar (routingOptionsFromQuery q')
    (heuristic, heuristicMs) <- timedIO forceHeuristic (prepareHeuristicProfiled defaultTileAStarConfig astar account (queryTarget q'))
    (points, transformMs) <- timedIO forcePointList (pure (layerPoints (transform useC) groups heuristic False))
    pure (key, title, banking, heuristicMs, transformMs, points, seedPoints heuristic banking, heuristic)
  layerPointsPrepared (_, _, _, _, _, points, _, _) = points
  layerSeedPointsPrepared (_, _, _, _, _, _, points, _) = points
  seedPoints heuristic banking =
    [ (unTile (packTile (x + dx) (y + dy) plane), value)
    | (cid, _) <- Boxed.toList (Boxed.indexed groups)
    , (packed, value) <- Vector.toList (heuristicSeeds heuristic Boxed.! seedKey cid banking)
    , let (x, y, plane) = unpackTile (Tile packed)
    , dx <- [-4 .. 4]
    , dy <- [-4 .. 4]
    ]
  renderPreparedLayers minimumValue maximumValue (key, title, banking, heuristicMs, transformMs, points, seeds, heuristic) = do
    normal <- renderLayer minimumValue maximumValue (key, title, banking, heuristicMs, transformMs, points)
    seedsLayer <- renderLayerWithSeeds minimumValue maximumValue (key <> "-seeds", title <> " seeds", banking, heuristicMs, transformMs, seeds) (actualSeeds heuristic banking)
    pure [normal, seedsLayer]
  actualSeeds heuristic banking =
    [ (Tile packed, value)
    | (cid, _) <- Boxed.toList (Boxed.indexed groups)
    , (packed, value) <- Vector.toList (heuristicSeeds heuristic Boxed.! seedKey cid banking)
    ]
  renderLayer minimumValue maximumValue (key, title, banking, heuristicMs, transformMs, points) = do
    renderLayerWithSeeds minimumValue maximumValue (key, title, banking, heuristicMs, transformMs, points) []
  renderLayerWithSeeds minimumValue maximumValue (key, title, banking, heuristicMs, transformMs, points) seedsForLayer = do
    let layerDir = outputRoot </> key
        layerUrl = urlRoot <> "/" <> key
    createDirectoryIfMissing True layerDir
    (tiles, writeMs) <- timedIO (evaluate . length) (writeHeuristicImageTiles key minimumValue maximumValue layerDir layerUrl points)
    pure (HeuristicLayer key title banking minimumValue maximumValue heuristicMs transformMs writeMs seedsForLayer tiles)

renderComponentTiles :: TileAStar -> FilePath -> String -> IO HeuristicRender
renderComponentTiles astar outputRoot urlRoot = do
  createDirectoryIfMissing True outputRoot
  allLayer <- renderLayer "reachable-components" "Reachable components" allPoints
  largestLayer <- renderLayer "largest-component" ("Largest component " <> show largestCid) largestPoints
  interestingLayer <- renderMarkers "largest-interesting" "Largest component entry/exit/bank tiles" interestingPoints
  pure (HeuristicRender imageTileSize [largestLayer, interestingLayer, allLayer])
 where
  groups = componentTileGroups components
  (largestCid, largestTiles) = Boxed.ifoldl' pickLargest (0, Vector.empty) groups
  transports = concat (Map.elems (worldTransports world)) <> worldGlobalTeleports world
  originTiles = Set.fromList [tile | transport <- transports, Just tile <- [origin transport]]
  destinationTiles = Set.fromList [tile | transport <- transports, Just tile <- [destination transport]]
  touchesLargest tile = largestCid `elem` pointAttachments topology tile
  interestingTiles = Set.filter touchesLargest (originTiles <> destinationTiles <> worldBanks world)
  allPoints =
    [ (packed, cid)
    | (cid, packedTiles) <- Boxed.toList (Boxed.indexed groups)
    , packed <- Vector.toList packedTiles
    ]
  largestPoints = [(packed, largestCid) | packed <- Vector.toList largestTiles]
  interestingPoints = IntMap.toList (IntMap.fromListWith max
    [ (unTile (packTile (x + dx) (y + dy) plane), interestingKind tile)
    | tile <- Set.toList interestingTiles
    , let (x, y, plane) = unpackTile tile
    , dx <- [-4 .. 4]
    , dy <- [-4 .. 4]
    ])
  interestingKind tile =
    (if Set.member tile originTiles then 1 else 0)
      + (if Set.member tile destinationTiles then 2 else 0)
      + (if Set.member tile (worldBanks world) then 4 else 0)
  pickLargest best@(_, bestTiles) cid packedTiles
    | Vector.length packedTiles > Vector.length bestTiles = (cid, packedTiles)
    | otherwise = best
  renderLayer key title points = do
    let values = map snd points
        layerDir = outputRoot </> key
        layerUrl = urlRoot <> "/" <> key
    createDirectoryIfMissing True layerDir
    (tiles, writeMs) <- timedIO (evaluate . length) (writeComponentImageTiles layerDir layerUrl points)
    pure (HeuristicLayer key title False (minimumDefault 0 values) (maximumDefault 0 values) 0 0 writeMs [] tiles)
  renderMarkers key title points = do
    let values = map snd points
        layerDir = outputRoot </> key
        layerUrl = urlRoot <> "/" <> key
    createDirectoryIfMissing True layerDir
    (tiles, writeMs) <- timedIO (evaluate . length) (writeMarkerImageTiles layerDir layerUrl points)
    pure (HeuristicLayer key title False (minimumDefault 0 values) (maximumDefault 0 values) 0 0 writeMs [] tiles)
  topology = tileTopology astar
  world = topologyWorld topology
  components = topologyRoutingComponents topology

layerPoints :: (Box -> [(Tile, Int)] -> Vector.Vector Int) -> Boxed.Vector (Vector.Vector Int) -> Heuristic -> Bool -> [(Int, Int)]
layerPoints transform groups heuristic banked =
  concatMap componentPoints (filter (not . Vector.null . snd) (Boxed.toList (Boxed.indexed groups)))
 where
  componentPoints (cid, packedTiles) =
    let seeds = heuristicSeeds heuristic Boxed.! seedKey cid banked
     in if Vector.null seeds
          then []
          else
            let box = componentBox packedTiles
                field = transform box [(Tile packed, cost) | (packed, cost) <- Vector.toList seeds]
             in [ (packed, value)
                | packed <- Vector.toList packedTiles
                , Just ix <- [offset box (Tile packed)]
                , let value = field Vector.! ix
                , value /= maxBound
                ]

writeHeuristicImageTiles :: String -> Int -> Int -> FilePath -> String -> [(Int, Int)] -> IO [HeuristicTile]
writeHeuristicImageTiles key minimumValue maximumValue layerDir layerUrl points = do
  let grouped = IntMap.toList (foldl addPoint IntMap.empty points)
  mapM writeTile grouped
 where
  addPoint tiles (packed, value) =
    let (x, y, plane) = unpackTile (Tile packed)
        tx = x `div` imageTileSize
        ty = y `div` imageTileSize
     in IntMap.insertWith (++) (imageTileKey plane tx ty) [(x, y, value)] tiles

  writeTile (encoded, tilePoints) = do
    let (plane, tx, ty) = decodeImageTileKey encoded
        values = map (\(_, _, value) -> value) tilePoints
        tileMin = minimumDefault 0 values
        tileMax = maximumDefault 0 values
        fileName = printf "%d_%d_%d.rgba" plane tx ty
        filePath = layerDir </> fileName
        url = layerUrl <> "/" <> fileName
    pixels <- imagePixels key minimumValue maximumValue tx ty tilePoints
    BS.writeFile filePath pixels
    pure (HeuristicTile url plane tx ty tileMin tileMax)

writeComponentImageTiles :: FilePath -> String -> [(Int, Int)] -> IO [HeuristicTile]
writeComponentImageTiles = writeColorImageTiles componentRgb 230

writeMarkerImageTiles :: FilePath -> String -> [(Int, Int)] -> IO [HeuristicTile]
writeMarkerImageTiles = writeColorImageTiles markerRgb 255

writeColorImageTiles :: (Int -> (Word8, Word8, Word8)) -> Word8 -> FilePath -> String -> [(Int, Int)] -> IO [HeuristicTile]
writeColorImageTiles color alpha layerDir layerUrl points = do
  let grouped = IntMap.toList (foldl addPoint IntMap.empty points)
  mapM writeTile grouped
 where
  addPoint tiles (packed, cid) =
    let (x, y, plane) = unpackTile (Tile packed)
        tx = x `div` imageTileSize
        ty = y `div` imageTileSize
     in IntMap.insertWith (++) (imageTileKey plane tx ty) [(x, y, cid)] tiles

  writeTile (encoded, tilePoints) = do
    let (plane, tx, ty) = decodeImageTileKey encoded
        values = map (\(_, _, cid) -> cid) tilePoints
        fileName = printf "%d_%d_%d.rgba" plane tx ty
        filePath = layerDir </> fileName
        url = layerUrl <> "/" <> fileName
    pixels <- colorPixels color alpha tx ty tilePoints
    BS.writeFile filePath pixels
    pure (HeuristicTile url plane tx ty (minimumDefault 0 values) (maximumDefault 0 values))

colorPixels :: (Int -> (Word8, Word8, Word8)) -> Word8 -> Int -> Int -> [(Int, Int, Int)] -> IO BS.ByteString
colorPixels color alpha tx ty points =
  BSI.create (imageTileSize * imageTileSize * 4) $ \pixels -> do
    fillBytes pixels 0 (imageTileSize * imageTileSize * 4)
    forM_ points $ \(x, y, value) -> do
      let localX = x - tx * imageTileSize
          localY = y - ty * imageTileSize
          row = imageTileSize - 1 - localY
          ix = (row * imageTileSize + localX) * 4
          (r, g, b) = color value
      pokeByteOff pixels ix r
      pokeByteOff pixels (ix + 1) g
      pokeByteOff pixels (ix + 2) b
      pokeByteOff pixels (ix + 3) alpha

componentRgb :: Int -> (Word8, Word8, Word8)
componentRgb cid =
  (channel 16, channel 8, channel 0)
 where
  h = cid * 1103515245 + 12345
  channel shift = fromIntegral (72 + ((h `shiftR` shift) .&. 159))

markerRgb :: Int -> (Word8, Word8, Word8)
markerRgb kind =
  case kind of
    1 -> (34, 197, 94)
    2 -> (239, 68, 68)
    3 -> (217, 70, 239)
    4 -> (250, 204, 21)
    5 -> (20, 184, 166)
    6 -> (249, 115, 22)
    _ -> (255, 255, 255)

imagePixels :: String -> Int -> Int -> Int -> Int -> [(Int, Int, Int)] -> IO BS.ByteString
imagePixels key minimumValue maximumValue tx ty points =
  BSI.create (imageTileSize * imageTileSize * 4) $ \pixels -> do
    rgbaTile xs ys values tx ty minimumValue maximumValue bankLayer pixels
 where
  xs = Storable.fromList [x | (x, _, _) <- points]
  ys = Storable.fromList [y | (_, y, _) <- points]
  values = Storable.fromList [value | (_, _, value) <- points]
  bankLayer = if key == "bank" then 1 else 0

imageTileSize :: Int
imageTileSize = 256

imageTileKey :: Int -> Int -> Int -> Int
imageTileKey plane tx ty = (plane `shiftL` 58) .|. ((tx .&. tileCoordMask) `shiftL` 29) .|. (ty .&. tileCoordMask)

decodeImageTileKey :: Int -> (Int, Int, Int)
decodeImageTileKey encoded = (encoded `shiftR` 58, (encoded `shiftR` 29) .&. tileCoordMask, encoded .&. tileCoordMask)

tileCoordMask :: Int
tileCoordMask = (1 `shiftL` 29) - 1


minimumDefault :: Ord a => a -> [a] -> a
minimumDefault fallback [] = fallback
minimumDefault _ values = minimum values

maximumDefault :: Ord a => a -> [a] -> a
maximumDefault fallback [] = fallback
maximumDefault _ values = maximum values

forceHeuristic :: Heuristic -> IO Int
forceHeuristic heuristic =
  evaluate
    ( Boxed.foldl' (\total seeds -> total + Vector.length seeds) 0 (heuristicSeeds heuristic)
        + Boxed.foldl' (\total scan -> total + forceGeneratorScan scan) 0 (heuristicGeneratorScans heuristic)
        + round (heuristicReverseMilliseconds heuristic)
        + round (heuristicSeedTableMilliseconds heuristic)
        + reverseStatesPopped (heuristicReverseCounters heuristic)
    )

forcePointList :: [(Int, Int)] -> IO Int
forcePointList points = evaluate (foldl' (\total (packed, value) -> total + packed + value) 0 points)
