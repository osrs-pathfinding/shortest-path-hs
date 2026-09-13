{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM, forM_)
import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (intercalate, sort, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as Vector
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs, lookupEnv)
import System.IO (IOMode(WriteMode), hFlush, hPutStrLn, stdout, withFile)
import System.Process (callProcess)
import Text.Printf (printf)
import Text.Read (readMaybe)

import ShortestPath.Tile
import ShortestPath.Separator
import ShortestPath.Topology
  ( NaturalComponents(..), StructuralReachability(..), WorldTopology(..)
  , naturalComponents, productionStructuralReachabilityPolicy
  , walkingTopologyIdentity, withEmptySeparatorArtifact, worldTopologyFromComponents
  )
import ShortestPath.Transport
import ShortestPath.Tsv
import ShortestPath.World

targetSize :: Int
targetSize = 50000

refinedTargetSize, refinedMinimumSize, refinedMaximumCheapSeparator :: Int
refinedTargetSize = 50000
refinedMinimumSize = 1000
refinedMaximumCheapSeparator = 10

refinedImbalances :: [Int]
refinedImbalances = [20, 40, 60, 80, 90, 95]

data Assignment = Assignment Int Int String
data KAssignment = KAssignment Int Int String String Int
data Split = Split Int String Int Int Int Int Int Int Int Int
data KSplit = KSplit Int String Int Int Int Int Int Int [Int]
data Result = Result Int [Int] [Assignment] [Split] [KAssignment] [KSplit]
data KCandidate = KCandidate Int [Int] [Int] [Int]
data KRefinement = KRefinement Int String Int Int (Maybe KCandidate) [KCandidate]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["generate-artifact", output, maximumText, minimumText, maximumSeparatorText, imbalanceText, seedText]
      | Just maximumSize <- readMaybe maximumText
      , Just minimumSize <- readMaybe minimumText
      , Just maximumSeparator <- readMaybe maximumSeparatorText
      , Just imbalance <- readMaybe imbalanceText
      , Just seed <- readMaybe seedText
      , maximumSize >= 2 * minimumSize
      , minimumSize > 0
      , maximumSeparator >= 0
      , imbalance >= 0 -> generateArtifact output (SeparatorConfig maximumSize minimumSize maximumSeparator imbalance "strong" seed)
    ["integrate"] -> integrate
    ["refine-kahip"] -> refineKahip refinedTargetSize refinedMinimumSize refinedMaximumCheapSeparator
    ["refine-kahip", targetText, minimumText]
      | Just target <- readMaybe targetText
      , Just minimumSize <- readMaybe minimumText
      , target > minimumSize
      , minimumSize > 0 -> refineKahip target minimumSize (-1)
    ["refine-kahip", targetText, minimumText, cheapSeparatorText]
      | Just target <- readMaybe targetText
      , Just minimumSize <- readMaybe minimumText
      , Just cheapSeparator <- readMaybe cheapSeparatorText
      , target > minimumSize
      , minimumSize > 0
      , cheapSeparator >= 0 -> refineKahip target minimumSize cheapSeparator
    [] -> partition
    _ -> putStrLn "usage: metis-partition generate-artifact OUTPUT MAXIMUM-SIZE MINIMUM-CHILD MAXIMUM-SEPARATOR IMBALANCE SEED | [integrate | refine-kahip [maximum-size minimum-child [maximum-cheap-separator]]]"

generateArtifact :: FilePath -> SeparatorConfig -> IO ()
generateArtifact output config = do
  createDirectoryIfMissing True "out/kahip-separators"
  world <- timed "load world without separators" loadConfiguredWorld
  natural <- timed "authoritative natural walking components" (naturalComponents world)
  let grouped = Map.toAscList (Map.fromListWith (<>)
        [(cid, [packed]) | (packed, cid) <- zip (Vector.toList (componentOwnerTiles natural)) (Vector.toList (componentOwnerIds natural))])
      baselineWorld = withEmptySeparatorArtifact world
  baseline <- either (fail . show) pure (worldTopologyFromComponents productionStructuralReachabilityPolicy baselineWorld natural)
  let reachable = structurallyReachableIds (topologyStructuralReachability baseline)
      aboveThreshold = filter ((> separatorMaximumComponentSize config) . length . snd) grouped
      selected = separatorCandidates (separatorMaximumComponentSize config) reachable grouped
      totalTiles = sum (map (length . snd) selected)
  putFlush ("natural_components_total=" <> show (length grouped))
  putFlush ("structurally_reachable_components=" <> show (IntSet.size reachable))
  putFlush ("components_above_size_threshold=" <> show (length aboveThreshold))
  putFlush ("reachable_components_above_size_threshold=" <> show (length selected))
  putFlush ("kahip_top_level_components_attempted=" <> show (length selected))
  putFlush ("kahip_tiles_considered=" <> show totalTiles)
  putFlush ("KaHIP top-level components: " <> intercalate ", " [show cid <> " (" <> show (length tiles) <> ")" | (cid, tiles) <- selected])
  assignments <- fmap IntMap.unions (forM selected $ \(cid, tiles) -> do
    putFlush ("partitioning natural component " <> show cid <> " (" <> show (length tiles) <> " tiles)")
    partitionArtifactComponent world config cid "1" 0 tiles)
  let cuts = Set.toAscList (Set.fromList
        [ canonicalCut (Tile from) to
        | (from, fromRegion) <- IntMap.toAscList assignments
        , to <- walkingNeighborsRaw world (Tile from)
        , from < unTile to
        , Just toRegion <- [IntMap.lookup (unTile to) assignments]
        , fromRegion /= toRegion
        ])
      artifact = SeparatorArtifact separatorArtifactVersion (walkingTopologyIdentity world) config cuts
  BL.writeFile output (encode artifact)
  BL.writeFile (output <> ".diagnostics.json") (encode (object
    [ "natural_components_total" .= length grouped
    , "structurally_reachable_components" .= IntSet.size reachable
    , "components_above_size_threshold" .= length aboveThreshold
    , "reachable_components_above_size_threshold" .= length selected
    , "kahip_top_level_components_attempted" .= length selected
    , "kahip_tiles_considered" .= totalTiles
    , "top_level_components" .= [object ["component_id" .= cid, "tile_count" .= length tiles] | (cid, tiles) <- selected]
    ]))
  putStrLn ("wrote " <> output <> " with " <> show (length cuts) <> " cut walking edges and generation diagnostics")

partitionArtifactComponent :: World -> SeparatorConfig -> Int -> String -> Int -> [Int] -> IO (IntMap.IntMap String)
partitionArtifactComponent world config cid label level tiles
  | length tiles <= separatorMaximumComponentSize config = pure (owned ("leaf-" <> show cid <> "-" <> label) tiles)
  | length tiles < 2 * separatorMinimumChildSize config = reject "cannot meet minimum child size"
  | otherwise = do
      let stem = "out/kahip-separators/component-" <> show cid <> "-" <> label
          graph = stem <> ".graph"
          result = stem <> ".separator"
      writeGraph world tiles graph
      callProcess "node_separator"
        [ graph
        , "--output_filename=" <> result
        , "--seed=" <> show (separatorRandomSeed config)
        , "--imbalance=" <> show (separatorImbalance config)
        , "--preconfiguration=" <> separatorPreconfiguration config
        ]
      parts <- readParts result (length tiles) [0, 1, 2]
      let a = [tile | (tile, 0) <- zip tiles parts]
          b = [tile | (tile, 1) <- zip tiles parts]
          separator = [tile | (tile, 2) <- zip tiles parts]
      case separatorRejection config (length a) (length b) (length separator) of
        Just reason -> reject reason
        Nothing -> do
          left <- partitionArtifactComponent world config cid (label <> "a") (level + 1) a
          right <- partitionArtifactComponent world config cid (label <> "b") (level + 1) b
          pure (IntMap.unions [owned ("separator-" <> show cid <> "-" <> label <> "-" <> show level) separator, left, right])
 where
  owned region = IntMap.fromList . map (, region)
  reject reason = do
    putFlush ("reject component " <> show cid <> " region " <> label <> " (" <> show (length tiles) <> " tiles): " <> reason)
    pure (owned ("leaf-" <> show cid <> "-" <> label) tiles)

partition :: IO ()
partition = do
  createDirectoryIfMissing True "out/metis"
  (world, components, owner) <- loadPartitionInputs
  let reachable = reachableComponents world owner
      selected = [(cid, tiles) | (cid, tiles) <- components, IntSet.member cid reachable, length tiles > targetSize, cid /= 4162]
  putFlush ("selected components: " <> show (length selected))
  results <- forM selected $ \(cid, tiles) -> do
    putFlush ("partitioning component " <> show cid <> " (" <> show (length tiles) <> " tiles)")
    partitionComponent world cid tiles
  validateResults owner selected results
  writeFile "out/metis/partitions.csv" (assignmentCsv results)
  writeFile "out/metis/cut-edges.csv" (cutEdgesCsv world results)
  writeFile "out/metis/kahip-separators.csv" (separatorCsv results)
  writeFile "out/metis/kahip-partitions.csv" (kahipCsv results)
  writeFile "metis-partition-report.md" (report results)
  putStrLn "wrote partition and complete KaHIP assignment outputs"

integrate :: IO ()
integrate = do
  createDirectoryIfMissing True "out/metis"
  (world, components, owner) <- loadPartitionInputs
  let reachable = reachableComponents world owner
      raw = [(cid, ns) | (cid, ns) <- components, IntSet.member cid reachable]
      selectedIds = Set.fromList [cid | (cid, ns) <- raw, length ns > targetSize, cid /= 4162]
  manualRows <- baselineRows "out/components.csv"
  kahip <- fmap concat (forM [(cid, ns) | (cid, ns) <- raw, Set.member cid selectedIds] $ \(cid, ns) -> do
    putFlush ("reconstructing KaHIP assignments for component " <> show cid)
    reuseKahip cid "1" 0 ns)
--  validateAssignments owner raw selectedIds metis kahip
  let interesting = interestingTiles world
      completeKahipAssignments = completeKahip raw selectedIds kahip
      kahipLike = instanceLikeKahip completeKahipAssignments
      kahipRows = regionRows world interesting kahipLike
      kahipEdges = regionEdges world (kahipOwners completeKahipAssignments)
      kahipLinks = regionLinks kahipEdges
      summary = censusJson raw mempty mempty kahipRows kahipLinks (baselineRegionRows manualRows) kahip
  timed "write kahip-components.csv" (writeFile "out/metis/kahip-components.csv" (rowsCsv kahipLinks kahipRows))
  timed "write kahip-region-graph.csv" (writeFile "out/metis/kahip-region-graph.csv" (regionGraphCsv kahipEdges))
  timed "write kahip-partitions.csv" (writeFile "out/metis/kahip-partitions.csv" (kahipAssignmentsCsv kahip))
  timed "write partition-census.json" (BL.writeFile "out/metis/partition-census.json" (encode summary))
  timed "write automatic-partition-census.md" (writeFile "automatic-partition-census.md" (censusMarkdown world raw mempty mempty kahipRows kahipLinks manualRows mempty kahip))
  putStrLn "wrote automatic partition census outputs"

refineKahip :: Int -> Int -> Int -> IO ()
refineKahip maximumSize minimumSize maximumCheapSeparator = do
  createDirectoryIfMissing True "out/metis"
  (world, components, owner) <- loadPartitionInputs
  let reachable = reachableComponents world owner
      selected = [(cid, ns) | (cid, ns) <- components, IntSet.member cid reachable, length ns > targetSize]
  current <- fmap concat (forM selected $ \(cid, ns) -> reuseKahip cid "1" 0 ns)
  let existingSeparators = [assignment | assignment@(KAssignment _ _ _ "separator" _) <- current]
      currentLeaves = Map.fromListWith (<>)
        [ ((cid, region, level), [n])
        | KAssignment cid n region "leaf" level <- current
        ]
  let outputStem = "kahip-refine-max" <> show maximumSize <> "-min" <> show minimumSize <> "-cheap" <> show maximumCheapSeparator
  putFlush ("refining " <> show (Map.size currentLeaves) <> " existing KaHIP leaves with maximum=" <> show maximumSize <> " minimum=" <> show minimumSize <> " cheap-separator=" <> show maximumCheapSeparator)
  refined <- forM (Map.toAscList currentLeaves) $ \((cid, region, level), ns) -> do
    putFlush ("refining component " <> show cid <> " region " <> region <> " (" <> show (length ns) <> " tiles)")
    refineKahipLeaf world maximumSize minimumSize maximumCheapSeparator cid region level ns
  let assignments = existingSeparators <> concat [xs | (xs, _) <- refined]
      refinements = concat [xs | (_, xs) <- refined]
  validateKahipOnly owner selected assignments
  timed "write refinement partitions" (writeFile ("out/metis/" <> outputStem <> "-partitions.csv") (kahipAssignmentsCsv assignments))
  timed "write refinement candidates" (writeFile ("out/metis/" <> outputStem <> "-candidates.csv") (refinementCsv minimumSize refinements))
  timed "write refinement report" (writeFile (outputStem <> "-report.md") (refinementReport maximumSize minimumSize maximumCheapSeparator current assignments refinements))
  putStrLn ("wrote isolated KaHIP refinement outputs with prefix " <> outputStem <> "; current hierarchy assignments are unchanged")

loadPartitionInputs :: IO (World, [(Int, [Int])], IntMap.IntMap Int)
loadPartitionInputs = do
  world <- timed "load world" loadConfiguredWorld
  walkable <- timed "enumerate walkable tiles" (enumerateWalkable (worldCollision world))
  (rawComponents, rawOwner) <- timed "raw walking components" (componentsOf world walkable)
  threshold <- envInt "COLLAPSE_SMALL_DOOR_COMPONENTS" 0
  if threshold <= 0
    then pure (world, rawComponents, rawOwner)
    else do
      let (components, owner) = collapseSmallDoorComponents world threshold rawComponents rawOwner
      putFlush ("collapsed door-connected components below " <> show threshold <> " tiles: " <> show (length rawComponents) <> " -> " <> show (length components))
      pure (world, components, owner)

loadConfiguredWorld :: IO World
loadConfiguredWorld = do
  paths <- configuredSourcePaths
  world <- loadWorldWithoutSeparators paths
  doorFile <- lookupEnv "DOOR_TRANSPORTS_TSV"
  doors <- maybe (pure []) (\path -> map (doorTransport path) <$> readRows path) doorFile
  dropDitch <- envFlag "DROP_WILDERNESS_DITCH"
  let withDoors = appendTransports world doors
      filtered = if dropDitch then filterLocalTransports (not . isWildernessDitch) withDoors else withDoors
  putFlush ("configured transports: doors=" <> show (length doors) <> " drop-ditch=" <> show dropDitch)
  pure filtered

configuredSourcePaths :: IO SourcePaths
configuredSourcePaths = do
  resources <- lookupEnv "SPM_RESOURCES_DIR"
  collision <- lookupEnv "SPM_COLLISION_ZIP"
  bank <- lookupEnv "SPM_BANK_FILE"
  pure defaultSourcePaths
    { resourcesDir = maybe (resourcesDir defaultSourcePaths) id resources
    , collisionZip = maybe (collisionZip defaultSourcePaths) id collision
    , bankFile = maybe (bankFile defaultSourcePaths) id bank
    }

doorTransport :: FilePath -> Row -> Transport
doorTransport sourcePath row =
  Transport "DOOR" (parseTileField (field "Origin" row)) (parseTileField (field "Destination" row)) 1 (field "Display info" row) (field "menuOption menuTarget objectID" row) False Nothing [] Nothing [] [] [] sourcePath

appendTransports :: World -> [Transport] -> World
appendTransports world transports =
  world { worldTransports = Map.fromListWith (<>) (transportRows world <> [(originTile, [transport]) | transport <- transports, Just originTile <- [origin transport]]) }

filterLocalTransports :: (Transport -> Bool) -> World -> World
filterLocalTransports keep world =
  world { worldTransports = Map.fromListWith (<>) [(originTile, [transport]) | transports <- Map.elems (worldTransports world), transport <- transports, keep transport, Just originTile <- [origin transport]] }

transportRows :: World -> [(Tile, [Transport])]
transportRows world = [(originTile, [transport]) | transports <- Map.elems (worldTransports world), transport <- transports, Just originTile <- [origin transport]]

isWildernessDitch :: Transport -> Bool
isWildernessDitch transport = displayInfo transport == "Cross Wilderness Ditch" || objectInfo transport == "Cross Wilderness Ditch 23271"

collapseSmallDoorComponents :: World -> Int -> [(Int, [Int])] -> IntMap.IntMap Int -> ([(Int, [Int])], IntMap.IntMap Int)
collapseSmallDoorComponents world threshold components owner =
  (Map.toAscList grouped, IntMap.map (\cid -> Map.findWithDefault cid cid roots) owner)
 where
  sizes = Map.fromList [(cid, length tiles) | (cid, tiles) <- components]
  doorEdges =
    [ (a, b)
    | transports <- Map.elems (worldTransports world)
    , transport <- transports
    , transportType transport == "DOOR"
    , Just originTile <- [origin transport]
    , Just destinationTile <- [destination transport]
    , Just a <- [attachedComponent world owner originTile]
    , Just b <- [attachedComponent world owner destinationTile]
    , a /= b
    , Map.findWithDefault 0 a sizes < threshold || Map.findWithDefault 0 b sizes < threshold
    ]
  adjacency = Map.fromListWith (<>) ([(a, [b]) | (a, b) <- doorEdges] <> [(b, [a]) | (a, b) <- doorEdges])
  roots = Map.fromList [(cid, root) | group <- doorGroups, let root = minimum group, cid <- group]
  doorGroups = connectedGroups (Set.fromList (concatMap (\(a, b) -> [a, b]) doorEdges)) adjacency
  grouped = Map.fromListWith (<>) [(Map.findWithDefault cid cid roots, tiles) | (cid, tiles) <- components]

attachedComponent :: World -> IntMap.IntMap Int -> Tile -> Maybe Int
attachedComponent world owner tile =
  case IntSet.toList (IntSet.fromList [cid | candidate <- tile : walkingNeighborsRaw world tile, Just cid <- [IntMap.lookup (unTile candidate) owner]]) of
    cid:_ -> Just cid
    [] -> Nothing

connectedGroups :: Set.Set Int -> Map.Map Int [Int] -> [[Int]]
connectedGroups nodes adjacency = go nodes []
 where
  go remaining groups = case Set.minView remaining of
    Nothing -> groups
    Just (start, rest) ->
      let group = flood Set.empty [start]
       in go (foldr Set.delete rest group) (group : groups)
  flood seen [] = Set.toList seen
  flood seen (cid:queue)
    | Set.member cid seen = flood seen queue
    | otherwise = flood (Set.insert cid seen) (Map.findWithDefault [] cid adjacency <> queue)

envFlag :: String -> IO Bool
envFlag name = maybe False (`elem` ["1", "true", "yes", "on"]) <$> lookupEnv name

envInt :: String -> Int -> IO Int
envInt name fallback = maybe fallback (maybe fallback id . readMaybe) <$> lookupEnv name

refineKahipLeaf :: World -> Int -> Int -> Int -> Int -> String -> Int -> [Int] -> IO ([KAssignment], [KRefinement])
refineKahipLeaf world maximumSize minimumSize maximumCheapSeparator cid label level ns
  | length ns < 2 * minimumSize = pure ([KAssignment cid n label "leaf" level | n <- ns], [])
  | otherwise = do
      let graph = "out/metis/kahip-refine-" <> show cid <> "-" <> label <> ".graph"
      graphExists <- doesFileExist graph
      if graphExists then pure () else timed ("write " <> graph) (writeGraph world ns graph)
      candidates <- forM refinedImbalances (runCandidate graph)
      let feasible = filter candidateFeasible candidates
          acceptable (KCandidate _ _ _ separator) = length ns > maximumSize || length separator <= maximumCheapSeparator
          chosen = case sortOn candidateScore (filter acceptable feasible) of
            [] -> Nothing
            first : _ -> Just first
          refinement = KRefinement cid label level (length ns) chosen candidates
      case chosen of
        Nothing -> do
          putFlush ("  no acceptable split: children must be >= " <> show minimumSize <> " and regions <= " <> show maximumSize <> " require separator <= " <> show maximumCheapSeparator)
          pure ([KAssignment cid n label "leaf" level | n <- ns], [refinement])
        Just (KCandidate imbalance a b s) -> do
          putFlush ("  chose imbalance=" <> show imbalance <> " sides=" <> show (length a) <> "/" <> show (length b) <> " separator=" <> show (length s))
          let childPrefix = label <> "i" <> show imbalance
          (aa, ar) <- refineKahipLeaf world maximumSize minimumSize maximumCheapSeparator cid (childPrefix <> "a") (level + 1) a
          (bb, br) <- refineKahipLeaf world maximumSize minimumSize maximumCheapSeparator cid (childPrefix <> "b") (level + 1) b
          let separators = [KAssignment cid n label "separator" level | n <- s]
          pure (separators <> aa <> bb, refinement : ar <> br)
 where
  runCandidate graph imbalance = do
    let output = graph <> ".i" <> show imbalance <> ".s42.separator"
    exists <- doesFileExist output
    if exists
      then putFlush ("  reuse imbalance=" <> show imbalance)
      else timed ("KaHIP imbalance=" <> show imbalance) $ callProcess "node_separator"
        [graph, "--output_filename=" <> output, "--seed=42", "--imbalance=" <> show imbalance, "--preconfiguration=strong"]
    parts <- readParts output (length ns) [0, 1, 2]
    pure (KCandidate imbalance [n | (n, 0) <- zip ns parts] [n | (n, 1) <- zip ns parts] [n | (n, 2) <- zip ns parts])
  candidateFeasible (KCandidate _ a b _) = length a >= minimumSize && length b >= minimumSize
  candidateScore (KCandidate imbalance a b s) = (length s, max (length a) (length b), abs (length a - length b), imbalance)

validateKahipOnly :: IntMap.IntMap Int -> [(Int, [Int])] -> [KAssignment] -> IO ()
validateKahipOnly owner selected assignments = do
  let expected = IntMap.fromList [(n, cid) | (cid, ns) <- selected, n <- ns]
      actualRows = [(n, cid) | KAssignment cid n _ _ _ <- assignments, IntMap.lookup n owner == Just cid]
      actual = IntMap.fromList actualRows
      unique = length actualRows == IntMap.size actual
      kinds = Set.fromList [kind | KAssignment _ _ _ kind _ <- assignments]
      valid = length assignments == IntMap.size expected && unique && actual == expected && kinds == Set.fromList ["leaf", "separator"]
  if valid
    then putFlush "refined KaHIP assignment invariants: pass"
    else fail ("refined KaHIP assignment invariant failed: expected=" <> show (IntMap.size expected) <> " assignments=" <> show (length assignments) <> " unique=" <> show unique <> " coverage=" <> show (actual == expected) <> " kinds=" <> show kinds)

refinementCsv :: Int -> [KRefinement] -> String
refinementCsv minimumSize refinements = unlines
  ("component,region,level,parent_tiles,imbalance,side_a,side_b,separator,valid,chosen" : concatMap rows refinements)
 where
  rows (KRefinement cid region level parent chosen candidates) =
    [ intercalate ","
        [ show cid, region, show level, show parent, show imbalance, show (length a), show (length b), show (length s)
        , show (length a >= minimumSize && length b >= minimumSize)
        , show (maybe False (sameCandidate candidate) chosen)
        ]
    | candidate@(KCandidate imbalance a b s) <- candidates
    ]
  sameCandidate (KCandidate a _ _ _) (KCandidate b _ _ _) = a == b

refinementReport :: Int -> Int -> Int -> [KAssignment] -> [KAssignment] -> [KRefinement] -> String
refinementReport maximumSize minimumSize maximumCheapSeparator before after refinements = unlines
  [ "# KaHIP Opportunistic Refinement"
  , ""
  , "Existing 50k KaHIP leaves were preserved and subdivided independently. Each split tried imbalance " <> intercalate "/" (map show refinedImbalances) <> " with strong quality and seed 42. Regions above the maximum accept the best feasible split; smaller regions split only when the separator is cheap enough. Selection minimises separator size before considering balance."
  , ""
  , "- Maximum leaf size: " <> show maximumSize
  , "- Minimum accepted child size: " <> show minimumSize
  , "- Maximum opportunistic separator size: " <> show maximumCheapSeparator
  , "- Leaves before/after: " <> show (length beforeSizes) <> "/" <> show (length afterSizes)
  , "- Leaf size min/median/p95/max: " <> sizeStats afterSizes
  , "- Leaves above maximum: " <> show (length (filter (> maximumSize) afterSizes))
  , "- Separator tiles before/after: " <> show beforeSeparators <> "/" <> show afterSeparators
  , "- Added separator tiles: " <> show (afterSeparators - beforeSeparators)
  , ""
  , "| component | split | parent | chosen imbalance | side A | side B | separator | candidates (imbalance:a/b/s) |"
  , "|---:|---|---:|---:|---:|---:|---:|---|"
  , unlines (map splitRow refinements)
  , "## Leaves per component"
  , ""
  , "| component | leaves | min | median | max |"
  , "|---:|---:|---:|---:|---:|"
  , unlines ["| " <> intercalate " | " [show cid, show (length sizes), show (minimum sizes), show (percentile 0.5 sizes), show (maximum sizes)] <> " |" | (cid, sizes) <- Map.toAscList perComponent]
  ]
 where
  beforeSizes = leafSizes before
  afterSizes = leafSizes after
  leafSizes assignments = Map.elems (Map.fromListWith (+) [((cid, region), 1 :: Int) | KAssignment cid _ region "leaf" _ <- assignments])
  beforeSeparators = length [() | KAssignment _ _ _ "separator" _ <- before]
  afterSeparators = length [() | KAssignment _ _ _ "separator" _ <- after]
  sizeStats [] = "0/0/0/0"
  sizeStats sizes = intercalate "/" (map show [minimum sizes, percentile 0.5 sizes, percentile 0.95 sizes, maximum sizes])
  splitRow (KRefinement cid region _ parent chosen candidates) =
    let chosenColumns = case chosen of
          Nothing -> ["-", "-", "-", "-"]
          Just (KCandidate imbalance a b s) -> map show [imbalance, length a, length b, length s]
        candidateText = intercalate "; " [show imbalance <> ":" <> show (length a) <> "/" <> show (length b) <> "/" <> show (length s) | KCandidate imbalance a b s <- candidates]
     in "| " <> intercalate " | " ([show cid, region, show parent] <> chosenColumns <> [candidateText]) <> " |"
  perComponent = Map.fromListWith (<>) [(cid, [size]) | ((cid, _), size) <- Map.toList (Map.fromListWith (+) [((c, r), 1 :: Int) | KAssignment c _ r "leaf" _ <- after])]

partitionComponent :: World -> Int -> [Int] -> IO Result
partitionComponent world cid tiles = do
--  (metis, splits) <- goMetis "1" tiles
  (kahip, ksplits) <- goKahip "1" 0 tiles
  pure (Result cid tiles [] [] kahip ksplits)
 where
  goMetis label ns
    | length ns <= targetSize = pure ([Assignment cid n label | n <- ns], [])
    | otherwise = do
        let graph = "out/metis/component-" <> show cid <> "-" <> label <> ".graph"
        writeGraph world ns graph
        callProcess "gpmetis" [graph, "2"]
        parts <- readParts (graph <> ".part.2") (length ns) [0, 1]
        let a = [n | (n, 0) <- zip ns parts]
            b = [n | (n, 1) <- zip ns parts]
            (cut, ba, bb) = splitMetrics world a b
        (aa, sa) <- goMetis (label <> "a") a
        (ab, sb) <- goMetis (label <> "b") b
        pure (aa <> ab, Split cid label (length ns) (length a) (length b) cut ba bb (interestingCount world a) (interestingCount world b) : sa <> sb)

  goKahip label level ns
    | length ns <= targetSize = pure ([KAssignment cid n label "leaf" level | n <- ns], [])
    | otherwise = do
        let graph = "out/metis/kahip-" <> show cid <> "-" <> label <> ".graph"
            output = graph <> ".separator"
        writeGraph world ns graph
        callProcess "node_separator" [graph, "--output_filename=" <> output, "--seed=42", "--imbalance=20", "--preconfiguration=strong"]
        parts <- readParts output (length ns) [0, 1, 2]
        let a = [n | (n, 0) <- zip ns parts]
            b = [n | (n, 1) <- zip ns parts]
            s = [n | (n, 2) <- zip ns parts]
            current = [KAssignment cid n label "separator" level | n <- s]
            split = KSplit cid label (length a) (length b) (length s) (interestingCount world a) (interestingCount world b) (interestingCount world s) s
        if null a || null b
          then pure (current <> [KAssignment cid n label "leaf" (level + 1) | n <- a <> b], [split])
          else do
            (aa, sa) <- goKahip (label <> "a") (level + 1) a
            (ab, sb) <- goKahip (label <> "b") (level + 1) b
            pure (current <> aa <> ab, split : sa <> sb)

readParts :: FilePath -> Int -> [Int] -> IO [Int]
readParts path expected allowed = do
  contents <- readFile path
  xs <- mapM (readMaybePart allowed) (lines contents)
  if length xs == expected then pure xs else fail ("partition output length mismatch: " <> path)
 where
  readMaybePart choices s = case readMaybe s of
    Just n | n `elem` choices -> pure n
    _ -> fail ("invalid partition value in " <> path)

writeGraph :: World -> [Int] -> FilePath -> IO ()
writeGraph world ns path = do
  let ids = IntMap.fromList (zip ns [1 :: Int ..])
      neighbours n = [i | m <- map unTile (walkingNeighborsRaw world (Tile n)), Just i <- [IntMap.lookup m ids]]
      edges = sum (map (length . neighbours) ns) `div` 2
  withFile path WriteMode $ \h -> do
    hPutStrLn h (show (length ns) <> " " <> show edges)
    forM_ ns (hPutStrLn h . unwords . map show . neighbours)

validateResults :: IntMap.IntMap Int -> [(Int, [Int])] -> [Result] -> IO ()
validateResults owner selected results = validateAssignments owner selected (Set.fromList (map fst selected))
   (concat [k | Result _ _ _ _ k _ <- results])
  >> putFlush "assignment invariants: pass"

validateAssignments :: IntMap.IntMap Int -> [(Int, [Int])] -> Set.Set Int -> [KAssignment] -> IO ()
validateAssignments owner raw selected kahip = do
  let expected = IntMap.fromList [(n, cid) | (cid, ns) <- raw, Set.member cid selected, n <- ns]
      checkKahip = [(n, cid) | KAssignment cid n _ _ _ <- kahip, IntMap.lookup n owner == Just cid]
      unique xs = length xs == Set.size (Set.fromList (map fst xs))
      kinds = Set.fromList [k | KAssignment _ _ _ k _ <- kahip]
      kahipOk = length kahip == IntMap.size expected && length checkKahip == length kahip && unique checkKahip && IntMap.fromList checkKahip == expected && kinds == Set.fromList ["leaf", "separator"]
  unless kahipOk (fail ("KaHIP assignment invariant failed: expected=" <> show (IntMap.size expected) <> " assignments=" <> show (length kahip) <> " owned=" <> show (length checkKahip) <> " unique=" <> show (unique checkKahip) <> " coverage=" <> show (IntMap.fromList checkKahip == expected) <> " kinds=" <> show kinds))
 where
  unless True _ = pure ()
  unless False action = action

readAssignments :: FilePath -> IO [Assignment]
readAssignments path = map parse . drop 1 . lines <$> readFile path
 where
  parse line = case splitOn ',' line of
    [component, x, y, plane, region] -> Assignment (read component) (unTile (packTile (read x) (read y) (read plane))) region
    _ -> error ("invalid METIS assignment: " <> line)

reuseKahip :: Int -> String -> Int -> [Int] -> IO [KAssignment]
reuseKahip cid label level ns
  | length ns <= targetSize = pure [KAssignment cid n label "leaf" level | n <- ns]
  | otherwise = do
      let output = "out/metis/kahip-" <> show cid <> "-" <> label <> ".graph.separator"
      parts <- readParts output (length ns) [0, 1, 2]
      let a = [n | (n, 0) <- zip ns parts]
          b = [n | (n, 1) <- zip ns parts]
          separators = [KAssignment cid n label "separator" level | (n, 2) <- zip ns parts]
      if null a || null b
        then pure (separators <> [KAssignment cid n label "leaf" (level + 1) | n <- a <> b])
        else do
          aa <- reuseKahip cid (label <> "a") (level + 1) a
          bb <- reuseKahip cid (label <> "b") (level + 1) b
          pure (separators <> aa <> bb)

completeMetis :: [(Int, [Int])] -> Set.Set Int -> [Assignment] -> [Assignment]
completeMetis raw selected xs = xs <> [Assignment cid n ("raw-" <> show cid) | (cid, ns) <- raw, Set.notMember cid selected, n <- ns]

completeKahip :: [(Int, [Int])] -> Set.Set Int -> [KAssignment] -> [KAssignment]
completeKahip raw selected xs = xs <> [KAssignment cid n ("raw-" <> show cid) "leaf" 0 | (cid, ns) <- raw, Set.notMember cid selected, n <- ns]

enumerateWalkable :: CollisionMap -> IO IntSet.IntSet
enumerateWalkable cm = pure . IntSet.fromList $ [unTile tile | ((rx, ry), _) <- Map.toList (collisionRegions cm), p <- [0 .. 3], x <- [0 .. 63], y <- [0 .. 63], let tile = packTile (rx * 64 + x) (ry * 64 + y) p, isWalkable cm tile]

componentsOf :: World -> IntSet.IntSet -> IO ([(Int, [Int])], IntMap.IntMap Int)
componentsOf world tiles = go 1 tiles [] IntMap.empty
 where
  go cid remaining done owner = case IntSet.minView remaining of
    Nothing -> pure (reverse done, owner)
    Just (start, rest) -> do
      let (found, remaining') = flood (IntSet.singleton start) rest []
          owner' = foldr (`IntMap.insert` cid) owner found
      if length found > 10000 || cid `mod` 1000 == 0 then putFlush ("  component " <> show cid <> ": " <> show (length found) <> " tiles") else pure ()
      go (cid + 1) remaining' ((cid, found) : done) owner'
  flood pending remaining found = case IntSet.minView pending of
    Nothing -> (found, remaining)
    Just (n, pending') ->
      let ns = [m | m <- map unTile (walkingNeighborsRaw world (Tile n)), IntSet.member m remaining]
       in flood (IntSet.union pending' (IntSet.fromList ns)) (foldr IntSet.delete remaining ns) (n : found)

reachableComponents :: World -> IntMap.IntMap Int -> IntSet.IntSet
reachableComponents world owner = case IntMap.lookup (unTile (packTile 3221 3218 0)) owner of
  Nothing -> IntSet.empty
  Just start -> closure (IntSet.singleton start) [start]
 where
  locals = [(a, b) | t <- concat (Map.elems (worldTransports world)), transportType t /= "VIRTUAL_WALL", Just o <- [origin t], Just d <- [destination t], Just a <- [IntMap.lookup (unTile o) owner], Just b <- [IntMap.lookup (unTile d) owner]]
  globals = [b | t <- worldGlobalTeleports world, Just d <- [destination t], Just b <- [IntMap.lookup (unTile d) owner]]
  closure seen [] = seen
  closure seen (cid:rest) = let fresh = filter (`IntSet.notMember` seen) ([b | (a, b) <- locals, a == cid] <> globals) in closure (foldr IntSet.insert seen fresh) (fresh <> rest)

interestingTiles :: World -> Set.Set Tile
interestingTiles world = Set.unions [worldBanks world, Set.fromList [t | tr <- localTransports world, Just t <- [origin tr, destination tr]], Set.fromList [t | tr <- worldGlobalTeleports world, Just t <- [destination tr]]]

localTransports :: World -> [Transport]
localTransports world = filter ((/= "VIRTUAL_WALL") . transportType) (concat (Map.elems (worldTransports world)))

interestingCount :: World -> [Int] -> Int
interestingCount world ns = Set.size (Set.fromList [Tile n | n <- ns, Set.member (Tile n) (interestingTiles world)])

splitMetrics :: World -> [Int] -> [Int] -> (Int, Int, Int)
splitMetrics world a b = (length cuts, Set.size (Set.fromList (map fst cuts)), Set.size (Set.fromList (map snd cuts)))
 where
  bs = IntSet.fromList b
  cuts = [(n, m) | n <- a, m <- map unTile (walkingNeighborsRaw world (Tile n)), IntSet.member m bs]

assignmentCsv :: [Result] -> String
assignmentCsv rs = unlines ("component,tile_x,tile_y,plane,region" : [show cid <> "," <> show x <> "," <> show y <> "," <> show p <> "," <> label | Result _ _ as _ _ _ <- rs, Assignment cid n label <- as, let (x, y, p) = unpackTile (Tile n)])

kahipCsv :: [Result] -> String
kahipCsv rs = kahipAssignmentsCsv [a | Result _ _ _ _ assignments _ <- rs, a <- assignments]

kahipAssignmentsCsv :: [KAssignment] -> String
kahipAssignmentsCsv assignments = unlines ("component,x,y,plane,region,kind,level" : [show cid <> "," <> show x <> "," <> show y <> "," <> show p <> "," <> region <> "," <> kind <> "," <> show level | KAssignment cid n region kind level <- assignments, let (x, y, p) = unpackTile (Tile n)])

separatorCsv :: [Result] -> String
separatorCsv rs = unlines ("component,x,y,plane,level" : [show cid <> "," <> show x <> "," <> show y <> "," <> show p <> "," <> label | Result _ _ _ _ _ ks <- rs, KSplit cid label _ _ _ _ _ _ ns <- ks, n <- ns, let (x, y, p) = unpackTile (Tile n)])

cutEdgesCsv :: World -> [Result] -> String
cutEdgesCsv world rs = unlines ("component,ax,ay,ap,bx,by,bp,region_a,region_b" : [show cid <> "," <> show ax <> "," <> show ay <> "," <> show ap <> "," <> show bx <> "," <> show by <> "," <> show bp <> "," <> la <> "," <> lb | Result _ _ as _ _ _ <- rs, Assignment cid n la <- as, m <- map unTile (walkingNeighborsRaw world (Tile n)), n < m, Just lb <- [IntMap.lookup m labels], la /= lb, let (ax, ay, ap) = unpackTile (Tile n), let (bx, by, bp) = unpackTile (Tile m)])
 where labels = IntMap.fromList [(n, label) | Result _ _ as _ _ _ <- rs, Assignment _ n label <- as]

data RegionRow = RegionRow Int String Int Int Int Int Int Int Int Int Int Int Double

regionSize :: RegionRow -> Int
regionSize (RegionRow _ _ n _ _ _ _ _ _ _ _ _ _) = n

regionRows :: World -> Set.Set Tile -> [AssignmentLike] -> [RegionRow]
regionRows world interesting rows = [row cid label ns | ((cid, label), ns) <- Map.toList groups]
 where
  groups = Map.fromListWith (<>) [((cid, label), [n]) | AssignmentLike cid n label <- rows]
  row cid label ns = let cs = map (unpackTile . Tile) ns; xs = [x | (x, _, _) <- cs]; ys = [y | (_, y, _) <- cs]; count s = length [() | n <- ns, Set.member (Tile n) s]; i = count interesting in RegionRow cid label (length ns) (minimum xs) (minimum ys) (maximum xs) (maximum ys) (count (worldBanks world)) (count (Set.fromList [t | tr <- localTransports world, Just t <- [origin tr]])) (count (Set.fromList [t | tr <- localTransports world, Just t <- [destination tr]])) (count (Set.fromList [t | tr <- worldGlobalTeleports world, Just t <- [destination tr]])) i (fromIntegral i / fromIntegral (length ns))

data AssignmentLike = AssignmentLike Int Int String

type RegionKey = (Int, String)

data RegionEdge = RegionEdge String RegionKey RegionKey Int

data RegionLinks = RegionLinks Int Int Int Int (Set.Set RegionKey) (Set.Set RegionKey)

instanceLikeMetis :: [Assignment] -> [AssignmentLike]
instanceLikeMetis = map (\(Assignment c n r) -> AssignmentLike c n r)

instanceLikeKahip :: [KAssignment] -> [AssignmentLike]
instanceLikeKahip xs = [AssignmentLike c n r | KAssignment c n r "leaf" _ <- xs]

assignmentOwners :: [AssignmentLike] -> IntMap.IntMap RegionKey
assignmentOwners = IntMap.fromList . map (\(AssignmentLike cid n region) -> (n, (cid, region)))

kahipOwners :: [KAssignment] -> IntMap.IntMap RegionKey
kahipOwners = IntMap.fromList . map owner
 where
  owner (KAssignment cid n region kind _) = (n, (cid, if kind == "separator" then "separator-" <> region else region))

regionEdges :: World -> IntMap.IntMap RegionKey -> [RegionEdge]
regionEdges world owners = [RegionEdge kind a b count | ((kind, a, b), count) <- Map.toList aggregated]
 where
  aggregated = Map.fromListWith (+) (walking <> transports)
  walking = concat [[(("INTERFACE", a, b), 1 :: Int), (("INTERFACE", b, a), 1)] | (a, b) <- cutWalking]
  cutWalking =
    [ (a, b)
    | (n, a) <- IntMap.toList owners
    , m <- map unTile (walkingNeighborsRaw world (Tile n))
    , n < m
    , Just b <- [IntMap.lookup m owners]
    , a /= b
    ]
  transports =
    [ (("TRANSPORT", a, b), 1 :: Int)
    | tr <- localTransports world
    , Just o <- [origin tr]
    , Just d <- [destination tr]
    , Just a <- [IntMap.lookup (unTile o) owners]
    , Just b <- [IntMap.lookup (unTile d) owners]
    , a /= b
    ]

regionLinks :: [RegionEdge] -> Map.Map RegionKey RegionLinks
regionLinks = Map.fromListWith combine . concatMap links
 where
  links (RegionEdge kind a b count) = [(a, outgoing kind b count), (b, incoming kind a count)]
  outgoing "TRANSPORT" b n = RegionLinks n 0 0 0 (Set.singleton b) Set.empty
  outgoing _ b n = RegionLinks 0 0 n 0 (Set.singleton b) Set.empty
  incoming "TRANSPORT" a n = RegionLinks 0 n 0 0 Set.empty (Set.singleton a)
  incoming _ a n = RegionLinks 0 0 0 n Set.empty (Set.singleton a)
  combine (RegionLinks ao ai aio aii ado adi) (RegionLinks bo bi bio bii bdo bdi) = RegionLinks (ao + bo) (ai + bi) (aio + bio) (aii + bii) (Set.union ado bdo) (Set.union adi bdi)

emptyLinks :: RegionLinks
emptyLinks = RegionLinks 0 0 0 0 Set.empty Set.empty

rowsCsv :: Map.Map RegionKey RegionLinks -> [RegionRow] -> String
rowsCsv links rows = unlines ("component,region,tiles,min_x,min_y,max_x,max_y,banks,origins,destinations,global_destinations,interesting_tiles,density,transport_out,transport_in,interface_out,interface_in,distinct_out_regions,distinct_in_regions" : map row rows)
 where
  row (RegionRow cid label tiles lx ly hx hy banks origins dests globals interesting density) =
    let RegionLinks transportOut transportIn interfaceOut interfaceIn distinctOut distinctIn = Map.findWithDefault emptyLinks (cid, label) links
     in intercalate "," [show cid, label, show tiles, show lx, show ly, show hx, show hy, show banks, show origins, show dests, show globals, show interesting, printf "%.8f" density, show transportOut, show transportIn, show interfaceOut, show interfaceIn, show (Set.size distinctOut), show (Set.size distinctIn)]

regionGraphCsv :: [RegionEdge] -> String
regionGraphCsv edges = unlines ("kind,from_component,from_region,to_component,to_region,edges" : [intercalate "," [kind, show fc, fr, show tc, tr, show count] | RegionEdge kind (fc, fr) (tc, tr) count <- edges])

data Baseline = Baseline Int Int Int Int Int Int Int Int Int Int Int Double

baselineRows :: FilePath -> IO [Baseline]
baselineRows path = do
  exists <- readFile path
  pure [parse (splitOn ',' line) | line <- drop 1 (lines exists), not (null line)]
 where
  parse [c, t, lx, ly, hx, hy, _, b, o, d, g, i, den] = Baseline (read c) (read t) (read lx) (read ly) (read hx) (read hy) (read b) (read o) (read d) (read g) (read i) (read den)
  parse _ = error "invalid baseline component row"

censusJson :: [(Int, [Int])] -> [RegionRow] -> Map.Map RegionKey RegionLinks -> [RegionRow] -> Map.Map RegionKey RegionLinks -> [RegionRow] -> [KAssignment] -> Value
censusJson raw metis metisLinks kahip kahipLinks manual assignments =
  object
    [ "rawReachableTiles" .= sum (map (length . snd) raw)
    , "manual" .= methodJson (sum (map regionSize manual)) Map.empty manual
    , "metis" .= methodJson (sum (map regionSize metis)) metisLinks metis
    , "kahip" .= methodJson (sum (map regionSize kahip) + separatorCount) kahipLinks kahip
    , "kahipSeparators" .= separatorJson assignments
    ]
 where
  separatorCount = length [() | KAssignment _ _ _ "separator" _ <- assignments]
  methodJson represented links rows = object ["regions" .= length rows, "representedTiles" .= represented, "leafTiles" .= sum (map regionSize rows), "sizes" .= statsJson (map regionSize rows), "regionDetails" .= map (rowJson links) rows]
  rowJson links (RegionRow c r n lx ly hx hy b o d g i den) = object ["component" .= c, "region" .= r, "tiles" .= n, "minX" .= lx, "minY" .= ly, "maxX" .= hx, "maxY" .= hy, "banks" .= b, "origins" .= o, "destinations" .= d, "globalDestinations" .= g, "interestingTiles" .= i, "density" .= den, "connectivity" .= linksJson (Map.findWithDefault emptyLinks (c, r) links)]
  linksJson (RegionLinks transportOut transportIn interfaceOut interfaceIn distinctOut distinctIn) = object ["transportOut" .= transportOut, "transportIn" .= transportIn, "interfaceOut" .= interfaceOut, "interfaceIn" .= interfaceIn, "distinctOutRegions" .= Set.size distinctOut, "distinctInRegions" .= Set.size distinctIn]
  separatorJson xs = object ["tiles" .= separatorCount, "levels" .= Map.fromListWith (+) [(show l, 1 :: Int) | KAssignment _ _ _ "separator" l <- xs]]

statsJson :: [Int] -> Value
statsJson xs = object ["min" .= minimumOrZero xs, "median" .= percentile 0.5 xs, "p90" .= percentile 0.9 xs, "p95" .= percentile 0.95 xs, "p99" .= percentile 0.99 xs, "max" .= maximumOrZero xs, "singleton" .= count (== 1), "le10" .= count (<= 10), "le100" .= count (<= 100), "le1000" .= count (<= 1000), "le20000" .= count (<= 20000), "le50000" .= count (<= 50000)]
 where
  count p = length (filter p xs)
  minimumOrZero [] = 0
  minimumOrZero ys = minimum ys
  maximumOrZero [] = 0
  maximumOrZero ys = maximum ys

baselineRegionRows :: [Baseline] -> [RegionRow]
baselineRegionRows = map (\(Baseline c n lx ly hx hy b o d g i den) -> RegionRow c ("manual-" <> show c) n lx ly hx hy b o d g i den)

censusMarkdown :: World -> [(Int, [Int])] -> [RegionRow] -> Map.Map RegionKey RegionLinks -> [RegionRow] -> Map.Map RegionKey RegionLinks -> [Baseline] -> [Assignment] -> [KAssignment] -> String
censusMarkdown world raw metis metisLinks kahip kahipLinks manual metisAssignments kahipAssignments = unlines ["# Automatic Partition Census", "", "Raw reachable walkable tiles: " <> show rawTiles, "", "| method | regions | represented tiles | min | median | p90 | p95 | p99 | max | singletons | <=10 | <=100 | <=1000 | <=20000 | <=50000 |", "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|", methodLine "Manual baseline" (sum manualSizes) manualSizes, methodLine "METIS" (sum metisSizes) metisSizes, methodLine "KaHIP leaf regions" (sum kahipSizes + separatorTotal) kahipSizes, "", "KaHIP separator tiles: " <> show separatorTotal, "", separatorLevelTable, "", "## Largest regions", "", largestTable "METIS" metisLinks metis, "", largestTable "KaHIP" kahipLinks kahip, "", connectivityTable "METIS" metisLinks metis, "", connectivityTable "KaHIP" kahipLinks kahip, "", "## Partitioned source components", "", perSource selectedRaw metis kahip, "", "Transport in/out counts are expanded directed real transport edges crossing regions. Interface in/out counts are directed walking adjacencies crossing METIS regions or joining KaHIP leaves to separator groups.", "", "The manual baseline is read from the existing `out/components.csv`; KaHIP separator tiles are represented interface tiles, not search regions."]
 where
  rawTiles = sum (map (length . snd) raw)
  manualSizes = map baselineSize manual
  metisSizes = map regionSize metis
  kahipSizes = map regionSize kahip
  separatorTotal = length [() | KAssignment _ _ _ "separator" _ <- kahipAssignments]
  selectedRaw = filter ((> targetSize) . length . snd) raw
  methodLine name represented xs = let s = statsList xs in "| " <> name <> " | " <> intercalate " | " (map show [length xs, represented] <> map show s) <> " |"
  statsList xs = [minimumOrZero xs, percentile 0.5 xs, percentile 0.9 xs, percentile 0.95 xs, percentile 0.99 xs, maximumOrZero xs, length (filter (== 1) xs), length (filter (<= 10) xs), length (filter (<= 100) xs), length (filter (<= 1000) xs), length (filter (<= 20000) xs), length (filter (<= 50000) xs)]
  baselineSize (Baseline _ n _ _ _ _ _ _ _ _ _ _) = n
  largestTable name links rows = unlines (["### " <> name, "", "| component | region | tiles | bbox | banks | origins | destinations | global destinations | interesting | density | out | in |", "|---:|---|---:|---|---:|---:|---:|---:|---:|---:|---:|---:|"] <> ["| " <> intercalate " | " [show c, r, show n, printf "%d,%d..%d,%d" lx ly hx hy, show b, show o, show d, show g, show i, printf "%.6f" den, show (transportOut + interfaceOut), show (transportIn + interfaceIn)] <> " |" | RegionRow c r n lx ly hx hy b o d g i den <- take 20 (sortOn (negate . regionSize) rows), let RegionLinks transportOut transportIn interfaceOut interfaceIn _ _ = Map.findWithDefault emptyLinks (c, r) links])
  connectivityTable name links rows = unlines (["## " <> name <> " region connectivity", "", "- Leaf regions with no incoming crossing edge: " <> show (length [() | row <- selectedRows, totalIn row == 0]), "- Leaf regions with no outgoing crossing edge: " <> show (length [() | row <- selectedRows, totalOut row == 0]), "", "| component | region | tiles | transport out | transport in | interface out | interface in | total out | total in | distinct out regions | distinct in regions |", "|---:|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|"] <> map connectivityRow (sortOn (\row -> (rowComponent row, rowRegion row)) selectedRows))
   where
    selectedRows = filter ((`Set.member` selectedIds) . rowComponent) rows
    selectedIds = Set.fromList (map fst selectedRaw)
    linksFor row = Map.findWithDefault emptyLinks (rowComponent row, rowRegion row) links
    totalOut row = let RegionLinks transportOut _ interfaceOut _ _ _ = linksFor row in transportOut + interfaceOut
    totalIn row = let RegionLinks _ transportIn _ interfaceIn _ _ = linksFor row in transportIn + interfaceIn
    connectivityRow row = let RegionLinks transportOut transportIn interfaceOut interfaceIn distinctOut distinctIn = linksFor row in "| " <> intercalate " | " [show (rowComponent row), rowRegion row, show (regionSize row), show transportOut, show transportIn, show interfaceOut, show interfaceIn, show (transportOut + interfaceOut), show (transportIn + interfaceIn), show (Set.size distinctOut), show (Set.size distinctIn)] <> " |"
  perSource rawRows a b = unlines (["| component | raw tiles | METIS regions | METIS min/max | cut edges | boundary tiles | KaHIP regions | KaHIP min/max | separator tiles |", "|---:|---:|---:|---:|---:|---:|---:|---:|---:|"] <> ["| " <> intercalate " | " [show cid, show (length ns), show (length ma), minMax ma, show cuts, show boundaries, show (length ka), minMax ka, show separators] <> " |" | (cid, ns) <- rawRows, let ma = filter ((== cid) . rowComponent) a, let ka = filter ((== cid) . rowComponent) b, let (cuts, boundaries) = metisInterface cid, let separators = length [() | KAssignment c _ _ "separator" _ <- kahipAssignments, c == cid]])
  metisInterface cid = (length edges, Set.size (Set.fromList [n | (a, b) <- edges, n <- [a, b]]))
   where
    labels = IntMap.fromList [(n, label) | Assignment c n label <- metisAssignments, c == cid]
    edges = [(n, m) | (n, label) <- IntMap.toList labels, m <- map unTile (walkingNeighborsRaw world (Tile n)), n < m, Just other <- [IntMap.lookup m labels], label /= other]
  separatorLevelTable = unlines (["## KaHIP separators by recursion level", "", "| level | tiles |", "|---:|---:|"] <> ["| " <> show level <> " | " <> show count <> " |" | (level, count) <- Map.toList (Map.fromListWith (+) [(level, 1 :: Int) | KAssignment _ _ _ "separator" level <- kahipAssignments])])
  rowComponent (RegionRow c _ _ _ _ _ _ _ _ _ _ _ _) = c
  rowRegion (RegionRow _ r _ _ _ _ _ _ _ _ _ _ _) = r
  minMax [] = "-"
  minMax xs = show (minimum (map regionSize xs)) <> "/" <> show (maximum (map regionSize xs))
  minimumOrZero [] = 0
  minimumOrZero xs = minimum xs
  maximumOrZero [] = 0
  maximumOrZero xs = maximum xs

report :: [Result] -> String
report rs = unlines ("# METIS and KaHIP Walking-Graph Partitioning" : "" : "METIS (`gpmetis`) and KaHIP (`node_separator --preconfiguration=strong --imbalance=20 --seed=42`) use raw walking-only graphs. Recursive leaves stop at 50,000 tiles." : "" : concatMap one rs)
 where
  one (Result cid _ assignments splits _ ks) = ["## Component " <> show cid, "", "### METIS", "", "| split | parent | child A | child B | cut edges | boundary A | boundary B | interesting A | interesting B |", "|---|---:|---:|---:|---:|---:|---:|---:|---:|"] <> map splitRow splits <> ["", "METIS leaves: " <> show (Set.size (Set.fromList [label | Assignment _ _ label <- assignments])), "", "### KaHIP node separators", "", "| split | side A | side B | separator | interesting A | interesting B | interesting S | separator/min side |", "|---|---:|---:|---:|---:|---:|---:|---:|"] <> map ksplitRow ks <> [""]
  splitRow (Split _ label parent a b cut ba bb ia ib) = "| " <> intercalate " | " (label : map show [parent, a, b, cut, ba, bb, ia, ib]) <> " |"
  ksplitRow (KSplit _ label a b s ia ib is _) = "| " <> intercalate " | " (label : map show [a, b, s, ia, ib, is] <> [printf "%.6f" (fromIntegral s / fromIntegral (max 1 (min a b)) :: Double)]) <> " |"

splitOn :: Char -> String -> [String]
splitOn delimiter = foldr step [""]
 where
  step c (part:parts) | c == delimiter = "" : part : parts
                       | otherwise = (c : part) : parts
  step _ [] = []

percentile :: Double -> [Int] -> Int
percentile _ [] = 0
percentile q xs = sort xs !! min (length xs - 1) (floor (q * fromIntegral (length xs - 1)))

timed :: String -> IO a -> IO a
timed label action = putFlush ("start: " <> label) >> action >>= \result -> putFlush ("done: " <> label) >> pure result

putFlush :: String -> IO ()
putFlush message = putStrLn message >> hFlush stdout
