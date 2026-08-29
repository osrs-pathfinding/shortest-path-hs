{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM)
import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (intercalate, isInfixOf, sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust)
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed.Mutable as MV
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Text.Printf (printf)

import ShortestPath.Tile
import ShortestPath.Transport
import ShortestPath.Tsv
import ShortestPath.World

data Component = Component
  { compId :: Int
  , compSize :: Int
  , minX :: Int
  , minY :: Int
  , maxX :: Int
  , maxY :: Int
  , compPlane :: Int
  }
  deriving stock (Eq, Show)

data Family = Family
  { familyName :: String
  , category :: String
  , rawRows :: Int
  , logicalCount :: Int
  , distinctOrigins :: Int
  , distinctDestinations :: Int
  , expandedEdges :: Int
  , bidirectionalPairs :: Int
  , minCost :: Maybe Int
  , medianCost :: Maybe Int
  , p95Cost :: Maybe Int
  , maxCost :: Maybe Int
  , originsOnly :: Int
  , destinationsOnly :: Int
  , fixedRows :: Int
  }
  deriving stock (Eq, Show)

data ComponentCounts = ComponentCounts
  { bankCounts :: IntMap.IntMap Int
  , originCounts :: IntMap.IntMap Int
  , destinationCounts :: IntMap.IntMap Int
  , globalDestinationCounts :: IntMap.IntMap Int
  , interestingCounts :: IntMap.IntMap Int
  , interestingTiles :: Set.Set Tile
  }
  deriving stock (Eq, Show)

main :: IO ()
main = do
  hFlush stdout
  args <- getArgs
  case args of
    ["synthetic"] -> syntheticSmoke
    "tile":coords -> inspectTiles coords
    [] -> osrsCensus
    _ -> putStrLn "usage: graph-census [synthetic|tile x y plane ...]"

inspectTiles :: [String] -> IO ()
inspectTiles args =
  case parseTiles args of
    Just tiles -> do
      world <- timed "load world" (loadWorld defaultSourcePaths)
      walkable <- timed "enumerate walkable tiles" (enumerateWalkable (worldCollision world))
      (components, tileComp) <- timed "connected components" (componentsOf (map (unTile) . walkingNeighbors world . Tile) walkable)
      mapM_ (printTile walkable tileComp components) tiles
    Nothing -> putStrLn "usage: graph-census tile x y plane ..."
 where
  parseTiles [] = Just []
  parseTiles (sx:sy:sp:rest) = do
    x <- readMaybe sx
    y <- readMaybe sy
    p <- readMaybe sp
    more <- parseTiles rest
    pure ((x, y, p) : more)
  parseTiles _ = Nothing
  printTile walkable tileComp components (x, y, p) = do
    let tile = packTile x y p
        cid = IntMap.lookup (unTile tile) tileComp
    putStrLn ("tile: " <> show x <> " " <> show y <> " " <> show p)
    putStrLn ("  walkable: " <> show (IntSet.member (unTile tile) walkable))
    putStrLn ("  component: " <> maybe "" show cid)
    mapM_ (putStrLn . ("  component details: " <>) . show) [c | c <- components, Just (compId c) == cid]

osrsCensus :: IO ()
osrsCensus = do
  createDirectoryIfMissing True "out"
  world <- timed "load world" (loadWorld defaultSourcePaths)
  walkable <- timed "enumerate walkable tiles" (enumerateWalkable (worldCollision world))
  putFlush ("walkable tiles: " <> show (IntSet.size walkable))
  (components, tileComp) <- timed "connected components" (componentsOf (map (unTile) . walkingNeighbors world . Tile) walkable)
  families <- timed "transport families" (transportFamilies defaultSourcePaths world)
  let reachableIds = reachableFromLumbridge world tileComp
      reachableComponents = filter (flip IntSet.member reachableIds . compId) components
      reachableWalkable = IntSet.filter (\tile -> maybe False (`IntSet.member` reachableIds) (IntMap.lookup tile tileComp)) walkable
  putFlush ("reachable-from-lumbridge components: " <> show (length reachableComponents))
  putFlush ("reachable-from-lumbridge walkable tiles: " <> show (IntSet.size reachableWalkable))
  let counts = componentCounts world tileComp
  timed "write components.csv" (writeFile "out/components.csv" (componentsCsv counts reachableComponents))
  timed "write transport-families.csv" (writeFile "out/transport-families.csv" (familiesCsv families))
  timed "write component-graph.csv" (writeFile "out/component-graph.csv" (componentGraphCsv reachableIds tileComp (allTransports world)))
  timed "write component-shapes.json" (BL.writeFile "out/component-shapes.json" (encode (componentShapesJson reachableWalkable tileComp counts reachableComponents)))
  timed "write wall bitmaps" (writeWallBitmaps world walkable tileComp)
  timed "write graph-census.json" (BL.writeFile "out/graph-census.json" (encode (jsonReport world reachableWalkable reachableComponents reachableIds tileComp counts families)))
  timed "write graph-census.md" (writeFile "graph-census.md" (markdownReport world walkable reachableWalkable components reachableComponents tileComp counts families))
  putStrLn "wrote graph-census.md, out/graph-census.json, out/components.csv, out/component-shapes.json, out/transport-families.csv, out/component-graph.csv"

syntheticSmoke :: IO ()
syntheticSmoke = do
  let block ox oy = [packTile x y 0 | x <- [ox .. ox + 1], y <- [oy .. oy + 1]]
      groups = [block 0 0, block 70 0, block 0 70, block 70 70]
      tiles = IntSet.fromList (map unTile (concat groups))
      neighborMap = Map.fromListWith (<>) [(unTile a, [unTile b]) | g <- groups, a <- g, b <- g, a /= b, chebyshev2 a b == Just 1]
      transport a b label = Transport label (Just a) (Just b) 3 label "" False Nothing [] Nothing [] [] [] "synthetic"
      ts =
        [ transport (packTile 1 1 0) (packTile 70 0 0) "boat"
        , transport (packTile 71 1 0) (packTile 0 70 0) "ladder"
        , transport (packTile 70 71 0) (packTile 0 0 0) "teleport"
        ]
      world = World (CollisionMap Map.empty) (Map.fromListWith (<>) [(o, [t]) | t <- ts, Just o <- [origin t]]) [] (Set.singleton (packTile 0 0 0))
  (components, tileComp) <- componentsOf (\n -> Map.findWithDefault [] n neighborMap) tiles
  let graphEdges = lines (componentGraphCsv (IntSet.fromList (map compId components)) tileComp (allTransports world))
  if length components == 4 && sort (map compSize components) == [4,4,4,4] && length graphEdges == 4
    then putStrLn "synthetic smoke: pass (4 components, 3 crossing transports)"
    else fail ("synthetic smoke failed: " <> show (components, graphEdges))

enumerateWalkable :: CollisionMap -> IO IntSet.IntSet
enumerateWalkable cm = go 1 IntSet.empty (Map.toList (collisionRegions cm))
 where
  total = Map.size (collisionRegions cm)
  go _ acc [] = pure acc
  go i acc (((rx, ry), _):rest) = do
    let acc' = foldl' addTile acc [packTile (rx * 64 + lx) (ry * 64 + ly) p | p <- [0 .. 3], lx <- [0 .. 63], ly <- [0 .. 63]]
    if i `mod` 100 == 0 || i == total
      then putFlush (printf "  regions %d/%d, walkable so far %d" i total (IntSet.size acc'))
      else pure ()
    go (i + 1) acc' rest
  addTile acc tile = if isWalkable cm tile && not (isVirtualWallTile tile) then IntSet.insert (unTile tile) acc else acc

writeWallBitmaps :: World -> IntSet.IntSet -> IntMap.IntMap Int -> IO ()
writeWallBitmaps world walkable tileComp = do
  createDirectoryIfMissing True "out/virtual-walls"
  mapM_ (\(n, wall) -> do
    let (sx, sy, _) = unpackTile (wallStart wall)
        (ex, ey, _) = unpackTile (wallEnd wall)
        loX = min sx ex - 12
        hiX = max sx ex + 12
        loY = min sy ey - 12
        hiY = max sy ey + 12
        path = "out/virtual-walls/wall-" <> show n <> ".ppm"
    writeFile path (ppm wall loX hiX loY hiY)) (zip [1 :: Int ..] virtualWalls)
 where
  endpoints = Set.fromList [t | tr <- allTransports world, t <- catMaybes [origin tr, destination tr], transportType tr == "VIRTUAL_WALL"]
  ppm wall loX hiX loY hiY =
    unlines
      ( ["P3", show (hiX - loX + 1) <> " " <> show (hiY - loY + 1), "255"]
          <> [pixel wall x y | y <- reverse [loY .. hiY], x <- [loX .. hiX]]
      )
  pixel wall x y =
    let tile = packTile x y 0
        rgb =
          if isWallTile wall tile then (220, 40, 40)
          else if Set.member tile endpoints then (255, 220, 40)
          else if not (IntSet.member (unTile tile) walkable)
            then (0, 0, 0)
            else case IntMap.lookup (unTile tile) tileComp of
              Nothing -> (90, 90, 90)
              Just cid -> componentColor cid
     in let (r, g, b) = rgb in unwords (map show [r, g, b])
  isWallTile wall tile = tile `elem` wallTiles wall
  wallTiles wall
    | sx == ex = [packTile sx y 0 | y <- [min sy ey .. max sy ey]]
    | sy == ey = [packTile x sy 0 | x <- [min sx ex .. max sx ex]]
    | otherwise = [packTile x (sy - (x - sx)) 0 | x <- [min sx ex .. max sx ex]]
   where
    (sx, sy, _) = unpackTile (wallStart wall)
    (ex, ey, _) = unpackTile (wallEnd wall)
  componentColor cid =
    (40 + (cid * 83 `mod` 190), 40 + (cid * 47 `mod` 190), 40 + (cid * 19 `mod` 190))

componentsOf :: (Int -> [Int]) -> IntSet.IntSet -> IO ([Component], IntMap.IntMap Int)
componentsOf neighborsFor tiles = do
  queue <- MV.new (IntSet.size tiles)
  go queue 1 [] IntMap.empty tiles
 where
  go queue cid comps owner remaining =
    case IntSet.minView remaining of
      Nothing -> pure (reverse comps, owner)
      Just (start, rest) -> do
        MV.write queue 0 start
        (comp, owner', rest') <- flood queue cid rest owner 0 1 0 maxBound maxBound minBound minBound 0
        if compSize comp >= 10000 || cid `mod` 100 == 0
          then putFlush (printf "  component %d: %d tiles, remaining %d" cid (compSize comp) (IntSet.size rest'))
          else pure ()
        go queue (cid + 1) (comp : comps) owner' rest'

  flood queue cid remaining owner readIx writeIx size loX loY hiX hiY plane
    | readIx == writeIx = pure (Component cid size loX loY hiX hiY plane, owner, remaining)
    | otherwise = do
        packed <- MV.read queue readIx
        let tile = Tile packed
            (x, y, p) = unpackTile tile
            owner' = IntMap.insert packed cid owner
            size' = size + 1
            loX' = min loX x
            loY' = min loY y
            hiX' = max hiX x
            hiY' = max hiY y
            ns = [n | n <- neighborsFor packed, IntSet.member n remaining]
            remaining' = foldr IntSet.delete remaining ns
        writeMany queue writeIx ns
        flood queue cid remaining' owner' (readIx + 1) (writeIx + length ns) size' loX' loY' hiX' hiY' p

writeMany :: MV.IOVector Int -> Int -> [Int] -> IO ()
writeMany _ _ [] = pure ()
writeMany queue i (x:xs) = MV.write queue i x >> writeMany queue (i + 1) xs

transportFamilies :: SourcePaths -> World -> IO [Family]
transportFamilies paths world = forM transportTypes family
 where
  expandedByType name = filter ((== name) . transportType) (allTransports world)
  family tt = do
    rows <- readRows (resourcesDir paths </> "transports" </> ttFile tt)
    let origins = map (parseTileField . field "Origin") rows
        dests = map (parseTileField . field "Destination") rows
        originOnlyCount = length [() | (Just _, Nothing) <- zip origins dests]
        destOnlyCount = length [() | (Nothing, Just _) <- zip origins dests]
        fixedCount = length [() | (Just _, Just _) <- zip origins dests]
        expanded = expandedByType (ttName tt)
        costs = sort (map duration expanded)
        hasOriginFanout = any (> 1) (Map.elems (Map.fromListWith (+) [(o, 1 :: Int) | (Just o, Just _) <- zip origins dests]))
        cat = classify tt originOnlyCount destOnlyCount fixedCount hasOriginFanout
        logical = logicalSize cat originOnlyCount destOnlyCount fixedCount
    pure
      Family
        { familyName = ttName tt
        , category = cat
        , rawRows = length rows
        , logicalCount = logical
        , distinctOrigins = Set.size (Set.fromList (catMaybes (map origin expanded)))
        , distinctDestinations = Set.size (Set.fromList (catMaybes (map destination expanded)))
        , expandedEdges = length expanded
        , bidirectionalPairs = countBidirectional expanded
        , minCost = listToMaybe costs
        , medianCost = percentile 0.50 costs
        , p95Cost = percentile 0.95 costs
        , maxCost = lastMaybe costs
        , originsOnly = originOnlyCount
        , destinationsOnly = destOnlyCount
        , fixedRows = fixedCount
        }

classify :: TransportType -> Int -> Int -> Int -> Bool -> String
classify tt os ds fixed hasOriginFanout
  | os > 0 && ds > 0 = "multiple origins -> multiple destinations / hub network"
  | ttIsTeleport tt && ds > 1 = "global or broad-origin -> multiple destinations"
  | ttIsTeleport tt && ds == 1 = "global or broad-origin -> fixed destination"
  | fixed > 0 && hasOriginFanout = "fixed origin -> multiple destinations"
  | fixed > 0 = "fixed origin -> fixed destination"
  | otherwise = "other/unclassified"

logicalSize :: String -> Int -> Int -> Int -> Int
logicalSize cat os ds fixed
  | "hub network" `isInfixOf` cat = 1
  | "global" `isInfixOf` cat = ds
  | otherwise = fixed + os + ds

countBidirectional :: [Transport] -> Int
countBidirectional ts =
  length
    [ ()
    | t <- ts
    , Just a <- [origin t]
    , Just b <- [destination t]
    , a < b
    , Set.member (b, a, transportType t) pairs
    ]
 where
  pairs = Set.fromList [(a, b, transportType t) | t <- ts, Just a <- [origin t], Just b <- [destination t]]

allTransports :: World -> [Transport]
allTransports world = concat (Map.elems (worldTransports world)) <> worldGlobalTeleports world

componentCounts :: World -> IntMap.IntMap Int -> ComponentCounts
componentCounts world tileComp =
  ComponentCounts
    { bankCounts = countsFor banks
    , originCounts = countsFor origins
    , destinationCounts = countsFor destinations
    , globalDestinationCounts = countsFor globalDests
    , interestingCounts = countsFor interesting
    , interestingTiles = interesting
    }
 where
  banks = worldBanks world
  origins = Map.keysSet (worldTransports world)
  destinations = Set.fromList (catMaybes (map destination (concat (Map.elems (worldTransports world)))))
  globalDests = Set.fromList (catMaybes (map destination (worldGlobalTeleports world)))
  interesting = Set.unions [banks, origins, destinations, globalDests]
  countsFor = foldr add IntMap.empty . Set.toList
  add tile = case IntMap.lookup (unTile tile) tileComp of
    Nothing -> id
    Just cid -> IntMap.insertWith (+) cid 1

reachableFromLumbridge :: World -> IntMap.IntMap Int -> IntSet.IntSet
reachableFromLumbridge world tileComp =
  case IntMap.lookup (unTile (packTile 3221 3218 0)) tileComp of
    Nothing -> IntSet.empty
    Just start -> go IntSet.empty [start]
 where
  localEdges =
    IntMap.fromListWith
      (<>)
      [ (a, [b])
      | t <- concat (Map.elems (worldTransports world))
      , Just o <- [origin t]
      , Just d <- [destination t]
      , Just a <- [IntMap.lookup (unTile o) tileComp]
      , Just b <- [IntMap.lookup (unTile d) tileComp]
      ]
  globalDestinations =
    IntSet.fromList
      [ cid
      | t <- worldGlobalTeleports world
      , Just d <- [destination t]
      , Just cid <- [IntMap.lookup (unTile d) tileComp]
      ]
  go seen [] = seen
  go seen (cid:rest)
    | IntSet.member cid seen = go seen rest
    | otherwise =
        let next = IntMap.findWithDefault [] cid localEdges <> IntSet.toList globalDestinations
         in go (IntSet.insert cid seen) (next <> rest)

componentsCsv :: ComponentCounts -> [Component] -> String
componentsCsv counts comps =
  unlines
    ( "component,tiles,min_x,min_y,max_x,max_y,plane,banks,origins,destinations,global_destinations,interesting_tiles,density"
        : map row comps
    )
 where
  countIn getter cid = IntMap.findWithDefault 0 cid (getter counts)
  row c =
    csv
      [ show (compId c), show (compSize c), show (minX c), show (minY c), show (maxX c), show (maxY c), show (compPlane c)
      , show (countIn bankCounts (compId c)), show (countIn originCounts (compId c)), show (countIn destinationCounts (compId c))
      , show (countIn globalDestinationCounts (compId c)), show (countIn interestingCounts (compId c))
      , printf "%.8f" (fromIntegral (countIn interestingCounts (compId c)) / fromIntegral (compSize c) :: Double)
      ]

familiesCsv :: [Family] -> String
familiesCsv families =
  unlines
    ( "family,category,raw_rows,logical_count,origins,destinations,expanded_edges,bidirectional_pairs,min_cost,median_cost,p95_cost,max_cost,origin_only_rows,destination_only_rows,fixed_rows"
        : map row families
    )
 where
  row f =
    csv
      [ familyName f, category f, show (rawRows f), show (logicalCount f), show (distinctOrigins f), show (distinctDestinations f)
      , show (expandedEdges f), show (bidirectionalPairs f), showMaybe (minCost f), showMaybe (medianCost f), showMaybe (p95Cost f)
      , showMaybe (maxCost f), show (originsOnly f), show (destinationsOnly f), show (fixedRows f)
      ]

componentGraphCsv :: IntSet.IntSet -> IntMap.IntMap Int -> [Transport] -> String
componentGraphCsv allowed tileComp ts =
  unlines ("from_component,to_component,edges" : map row (Map.toList edges))
 where
  edges =
    Map.fromListWith (+)
      [ ((a, b), 1 :: Int)
      | t <- ts
      , Just o <- [origin t]
      , Just d <- [destination t]
      , Just a <- [IntMap.lookup (unTile o) tileComp]
      , Just b <- [IntMap.lookup (unTile d) tileComp]
      , IntSet.member a allowed
      , IntSet.member b allowed
      , a /= b
      ]
  row ((a, b), n) = csv [show a, show b, show n]

markdownReport :: World -> IntSet.IntSet -> IntSet.IntSet -> [Component] -> [Component] -> IntMap.IntMap Int -> ComponentCounts -> [Family] -> String
markdownReport world rawWalkable walkable rawComps comps tileComp counts families =
  unlines
    [ "# OSRS Pathfinding Graph Census"
    , ""
    , "## Headline totals"
    , ""
    , "- Scope: components reachable from Lumbridge (`3221 3218 0`) via directed transports, including broad/global teleports."
    , "- Raw walkable tiles before reachability filter: " <> show (IntSet.size rawWalkable)
    , "- Raw walking components before reachability filter: " <> show (length rawComps)
    , "- Excluded disconnected components: " <> show (length rawComps - length comps)
    , "- Walkable tiles: " <> show (IntSet.size walkable)
    , "- Walking components: " <> show (length comps)
    , "- Singleton components: " <> show (length (filter ((== 1) . compSize) comps))
    , "- Components <= 10 tiles: " <> show (small 10)
    , "- Components <= 100 tiles: " <> show (small 100)
    , "- Components <= 1,000 tiles: " <> show (small 1000)
    , "- Expanded transport edges: " <> show (length transports)
    , "- Distinct interesting tiles: " <> show (Set.size (interestingTiles counts))
    , "- Components with no interesting tiles: " <> show noInteresting
    , ""
    , "## Component size distribution"
    , ""
    , "| metric | tiles |"
    , "| --- | ---: |"
    , tableRows [["min", stat 0], ["median", stat 0.50], ["p90", stat 0.90], ["p95", stat 0.95], ["p99", stat 0.99]]
    , ""
    , "## Largest components"
    , ""
    , "| component | tiles | bbox | banks | origins | destinations | global destinations | interesting tiles | density |"
    , "| ---: | ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: |"
    , tableRows (map componentRow largest)
    , ""
    , "## Transport categories"
    , ""
    , "| category | logical families/actions | origins | destinations | expanded edges | bidirectional pairs | median cost | p95 cost | max cost |"
    , "| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |"
    , tableRows (map categoryRow (Map.toList byCategory))
    , ""
    , "## Hub/network transports"
    , ""
    , "| family | entrances | exits | expanded edges | walking components touched |"
    , "| --- | ---: | ---: | ---: | ---: |"
    , tableRows (map hubRow hubs)
    , ""
    , "## Global teleports"
    , ""
    , "- Logical global teleport actions: " <> show (length globals)
    , "- Distinct destination tiles: " <> show (Set.size globalDests)
    , "- Duplicate actions landing on an already-seen tile: " <> show (length globals - Set.size globalDests)
    , "- Destination walking components: " <> show (Set.size globalDestComps)
    , "- With item requirements: " <> show (length (filter (isJust . items) globals))
    , "- With quest requirements: " <> show (length (filter (not . null . quests) globals))
    , "- With var requirements: " <> show (length (filter (\t -> not (null (varbits t) && null (varPlayers t))) globals))
    , "- Banking availability is not derivable from TSV alone; TSV only records item requirements."
    , ""
    , "| component | global destinations |"
    , "| ---: | ---: |"
    , tableRows (map (\(cid, n) -> [show cid, show n]) (take 20 (sortOn (negate . snd) (IntMap.toList (globalDestinationCounts counts)))))
    , ""
    , "## Component-level transport graph"
    , ""
    , "- Same-component local transports: " <> show sameComponent
    , "- Cross-component local transports: " <> show crossComponent
    , "- Components with no incoming inter-component transport: " <> show noIncoming
    , "- Components with no outgoing inter-component transport: " <> show noOutgoing
    , "- Largest weakly-connected component-graph region: " <> show largestWeak
    , "- In-degree median/p95/max: " <> degreeStats incoming
    , "- Out-degree median/p95/max: " <> degreeStats outgoing
    , ""
    , "## Largest-component neighbours"
    , ""
    , "| component | distinct outgoing components | distinct incoming components |"
    , "| ---: | ---: | ---: |"
    , tableRows (map largestNeighbourRow (take 20 largest))
    , ""
    , "## High-degree components"
    , ""
    , "| component | out-degree | in-degree |"
    , "| ---: | ---: | ---: |"
    , tableRows (map degreeRow highDegree)
    , ""
    , "## Notes"
    , ""
    , "- The census uses current resource files directly from `/home/matt/shortest-path/src/main/resources`."
    , "- Hub classification is based on TSV permutation shape: rows with origin-only plus destination-only entries."
    , "- `graph-census.json` and CSV files under `out/` preserve the measured details for later analysis."
    ]
 where
  transports = allTransports world
  sizes = sort (map compSize comps)
  small n = length (filter ((<= n) . compSize) comps)
  stat q = maybe "" show (percentile q sizes)
  largest = take 50 (sortOn (negate . compSize) comps)
  byCategory = Map.fromListWith combineCategory [(category f, f) | f <- families]
  hubs = filter (isInfixOf "hub network" . category) families
  globals = worldGlobalTeleports world
  globalDests = Set.fromList (catMaybes (map destination globals))
  globalDestComps = Set.fromList (catMaybes [IntMap.lookup (unTile t) tileComp | t <- Set.toList globalDests])
  countIn getter cid = IntMap.findWithDefault 0 cid (getter counts)
  noInteresting = length [c | c <- comps, countIn interestingCounts (compId c) == 0]
  componentRow c =
    [ show (compId c), show (compSize c), bbox c, show (countIn bankCounts (compId c)), show (countIn originCounts (compId c))
    , show (countIn destinationCounts (compId c)), show (countIn globalDestinationCounts (compId c)), show (countIn interestingCounts (compId c))
    , printf "%.6f" (fromIntegral (countIn interestingCounts (compId c)) / fromIntegral (compSize c) :: Double)
    ]
  categoryRow (cat, f) =
    [ cat, show (logicalCount f), show (distinctOrigins f), show (distinctDestinations f), show (expandedEdges f)
    , show (bidirectionalPairs f), showMaybe (medianCost f), showMaybe (p95Cost f), showMaybe (maxCost f)
    ]
  hubRow f =
    [ familyName f, show (distinctOrigins f), show (distinctDestinations f), show (expandedEdges f)
    , show (componentsTouched tileComp [t | t <- transports, transportType t == familyName f])
    ]
  localEdges =
    [ (a, b)
    | t <- concat (Map.elems (worldTransports world))
    , Just o <- [origin t]
    , Just d <- [destination t]
    , Just a <- [IntMap.lookup (unTile o) tileComp]
    , Just b <- [IntMap.lookup (unTile d) tileComp]
    , Set.member a allCompIds
    , Set.member b allCompIds
    ]
  sameComponent = length [() | (a, b) <- localEdges, a == b]
  crossComponent = length [() | (a, b) <- localEdges, a /= b]
  incoming = Map.fromListWith (+) [(b, 1 :: Int) | (a, b) <- localEdges, a /= b]
  outgoing = Map.fromListWith (+) [(a, 1 :: Int) | (a, b) <- localEdges, a /= b]
  allCompIds = Set.fromList (map compId comps)
  noIncoming = Set.size (allCompIds Set.\\ Map.keysSet incoming)
  noOutgoing = Set.size (allCompIds Set.\\ Map.keysSet outgoing)
  highDegree = take 20 (sortOn (\c -> negate (Map.findWithDefault 0 c incoming + Map.findWithDefault 0 c outgoing)) (Set.toList allCompIds))
  degreeRow c = [show c, show (Map.findWithDefault 0 c outgoing), show (Map.findWithDefault 0 c incoming)]
  largestWeak = largestWeakRegion localEdges
  outgoingDistinct = Map.fromListWith Set.union [(a, Set.singleton b) | (a, b) <- localEdges, a /= b]
  incomingDistinct = Map.fromListWith Set.union [(b, Set.singleton a) | (a, b) <- localEdges, a /= b]
  largestNeighbourRow c =
    [ show (compId c)
    , show (Set.size (Map.findWithDefault Set.empty (compId c) outgoingDistinct))
    , show (Set.size (Map.findWithDefault Set.empty (compId c) incomingDistinct))
    ]

jsonReport :: World -> IntSet.IntSet -> [Component] -> IntSet.IntSet -> IntMap.IntMap Int -> ComponentCounts -> [Family] -> Value
jsonReport world walkable comps reachableIds tileComp counts families =
  object
    [ "walkableTiles" .= IntSet.size walkable
    , "componentCount" .= length comps
    , "scope" .= ("reachable-from-lumbridge" :: String)
    , "lumbridgeTile" .= ("3221 3218 0" :: String)
    , "components" .= map (componentJson counts) comps
    , "transportFamilies" .= map familyJson families
    , "globalTeleportActions" .= length (worldGlobalTeleports world)
    , "globalTeleportDestinations" .= Set.size (Set.fromList (catMaybes (map destination (worldGlobalTeleports world))))
    , "componentGraph" .= componentGraphJson reachableIds tileComp (allTransports world)
    ]

componentJson :: ComponentCounts -> Component -> Value
componentJson counts c =
  object
    [ "id" .= compId c, "tiles" .= compSize c, "minX" .= minX c, "minY" .= minY c
    , "maxX" .= maxX c, "maxY" .= maxY c, "plane" .= compPlane c
    , "banks" .= count bankCounts
    , "origins" .= count originCounts
    , "destinations" .= count destinationCounts
    , "globalDestinations" .= count globalDestinationCounts
    , "interestingTiles" .= count interestingCounts
    ]
 where
  count getter = IntMap.findWithDefault 0 (compId c) (getter counts)

familyJson :: Family -> Value
familyJson f =
  object
    [ "family" .= familyName f, "category" .= category f, "rawRows" .= rawRows f, "logicalCount" .= logicalCount f
    , "origins" .= distinctOrigins f, "destinations" .= distinctDestinations f, "expandedEdges" .= expandedEdges f
    , "bidirectionalPairs" .= bidirectionalPairs f, "minCost" .= minCost f, "medianCost" .= medianCost f
    , "p95Cost" .= p95Cost f, "maxCost" .= maxCost f
    ]

componentGraphJson :: IntSet.IntSet -> IntMap.IntMap Int -> [Transport] -> [Value]
componentGraphJson allowed tileComp ts =
  [ object ["from" .= a, "to" .= b, "edges" .= n]
  | ((a, b), n) <- Map.toList edges
  ]
 where
  edges =
    Map.fromListWith (+)
      [ ((a, b), 1 :: Int)
      | t <- ts
      , Just o <- [origin t], Just d <- [destination t]
      , Just a <- [IntMap.lookup (unTile o) tileComp], Just b <- [IntMap.lookup (unTile d) tileComp]
      , IntSet.member a allowed, IntSet.member b allowed
      , a /= b
      ]

componentShapesJson :: IntSet.IntSet -> IntMap.IntMap Int -> ComponentCounts -> [Component] -> Value
componentShapesJson walkable tileComp counts comps =
  object
    [ "binSize" .= binSize
    , "componentLimit" .= componentLimit
    , "components" .= map (componentJson counts) selected
    , "bins" .= map binJson (Map.toList bins)
    ]
 where
  binSize = 16 :: Int
  componentLimit = 50 :: Int
  selected = take componentLimit (sortOn (negate . compSize) comps)
  selectedIds = IntSet.fromList (map compId selected)
  bins = IntSet.foldl' addTile Map.empty walkable
  addTile acc packed =
    case IntMap.lookup packed tileComp of
      Just cid | IntSet.member cid selectedIds ->
        let (x, y, p) = unpackTile (Tile packed)
            bx = (x `div` binSize) * binSize
            by = (y `div` binSize) * binSize
         in Map.insertWith (+) (cid, bx, by, p) (1 :: Int) acc
      _ -> acc
  binJson ((cid, x, y, p), n) =
    object ["component" .= cid, "x" .= x, "y" .= y, "plane" .= p, "tiles" .= n]

combineCategory :: Family -> Family -> Family
combineCategory a b =
  a
    { logicalCount = logicalCount a + logicalCount b
    , distinctOrigins = distinctOrigins a + distinctOrigins b
    , distinctDestinations = distinctDestinations a + distinctDestinations b
    , expandedEdges = expandedEdges a + expandedEdges b
    , bidirectionalPairs = bidirectionalPairs a + bidirectionalPairs b
    , medianCost = Nothing
    , p95Cost = Nothing
    , maxCost = maxMaybe (maxCost a) (maxCost b)
    }

componentsTouched :: IntMap.IntMap Int -> [Transport] -> Int
componentsTouched tileComp ts =
  Set.size (Set.fromList (catMaybes ([IntMap.lookup (unTile t) tileComp | t <- catMaybes (map origin ts)] <> [IntMap.lookup (unTile t) tileComp | t <- catMaybes (map destination ts)])))

largestWeakRegion :: [(Int, Int)] -> Int
largestWeakRegion edges = maximum (0 : map Set.size comps)
 where
  adj = Map.fromListWith Set.union (concat [[(a, Set.singleton b), (b, Set.singleton a)] | (a, b) <- edges, a /= b])
  comps = go (Map.keysSet adj) []
  go unseen acc =
    case Set.minView unseen of
      Nothing -> acc
      Just (x, rest) ->
        let seen = flood rest [x] (Set.singleton x)
         in go (unseen Set.\\ seen) (seen : acc)
  flood _ [] seen = seen
  flood unseen (x:xs) seen =
    let ns = Set.toList (Map.findWithDefault Set.empty x adj `Set.intersection` unseen)
     in flood (foldr Set.delete unseen ns) (xs <> ns) (foldr Set.insert seen ns)

percentile :: Double -> [Int] -> Maybe Int
percentile _ [] = Nothing
percentile q xs = Just (xs !! min (length xs - 1) (floor (q * fromIntegral (length xs - 1))))

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just (last xs)

maxMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing b = b
maxMaybe a Nothing = a
maxMaybe (Just a) (Just b) = Just (max a b)

bbox :: Component -> String
bbox c = printf "%d,%d..%d,%d p%d" (minX c) (minY c) (maxX c) (maxY c) (compPlane c)

csv :: [String] -> String
csv = intercalate "," . map escape
 where
  escape s
    | any (`elem` (",\"\n" :: String)) s = "\"" <> concatMap (\c -> if c == '"' then "\"\"" else [c]) s <> "\""
    | otherwise = s

tableRows :: [[String]] -> String
tableRows = unlines . map (\cols -> "| " <> intercalate " | " cols <> " |")

showMaybe :: Show a => Maybe a -> String
showMaybe = maybe "" show

degreeStats :: Map.Map Int Int -> String
degreeStats degrees =
  showMaybe (percentile 0.50 xs) <> "/" <> showMaybe (percentile 0.95 xs) <> "/" <> showMaybe (lastMaybe xs)
 where
  xs = sort (Map.elems degrees)

timed :: String -> IO a -> IO a
timed label action = do
  putFlush ("start: " <> label)
  start <- getCurrentTime
  result <- action
  end <- getCurrentTime
  putFlush (printf "done: %s (%.2fs)" label (realToFrac (diffUTCTime end start) :: Double))
  pure result

putFlush :: String -> IO ()
putFlush msg = putStrLn msg >> hFlush stdout
