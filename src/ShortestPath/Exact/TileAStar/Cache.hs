module ShortestPath.Exact.TileAStar.Cache
  ( loadOrBuildTileAStar
  ) where

import Control.Exception (evaluate)
import Control.Monad (filterM, replicateM, when)
import Data.Binary.Get (Get, getByteString, getWord32be, getWord64be, runGetOrFail)
import Data.Binary.Put (Put, putByteString, putInt32be, putWord32be, putWord64be, putWord8, runPut)
import Data.Bits ((.|.), shiftL, xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)
import Data.Int (Int32)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (sort)
import qualified Data.Set as Set
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Vector
import Data.Word (Word8, Word32, Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, renameFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeExtension, takeFileName)
import System.IO (hPutStrLn, stderr)

import ShortestPath.Exact.TileAStar
import ShortestPath.Exact.TileAStar.SparseWalking
import ShortestPath.Exact.TileAStar.Types (TileAStar(..), TileStatic(..))
import ShortestPath.Tile (Tile(..))
import ShortestPath.Topology
import ShortestPath.Transport (defaultSourcePaths, resourcesDir, separatorFile)
import ShortestPath.World (World(..))

-- The old cache stored intermediate NaturalComponents and TileStatic through
-- list-based Binary instances. This format stores the final routing topology,
-- shares its walkable tile index, and uses flat primitive arrays.
cacheMagic :: BS.ByteString
cacheMagic = BSC.pack "SPM-ROUTING-TOPOLOGY"

cacheVersion :: Word32
cacheVersion = 1

cachePath :: FilePath
cachePath = "out/tile-astar-topology-v2.bin"

data DecodedCache = DecodedCache
  { decodedFingerprint :: !Word64
  , decodedOwnerTiles :: !(Vector.Vector Int)
  , decodedNaturalIds :: !(Vector.Vector Int)
  , decodedNaturalComponentIds :: !(Vector.Vector Int)
  , decodedNaturalMaxComponentId :: !Int
  , decodedRoutingIds :: !(Vector.Vector Int)
  , decodedRoutingComponentIds :: !(Vector.Vector Int)
  , decodedRoutingMaxComponentId :: !Int
  , decodedCrossings :: ![RoutingCrossing]
  , decodedReachableIds :: !(Vector.Vector Int)
  , decodedStatic :: !TileStatic
  }

loadOrBuildTileAStar :: World -> IO TileAStar
loadOrBuildTileAStar world = do
  fingerprint <- timed "cache fingerprint" (sourceFingerprint world)
  cached <- timed "cache load" (loadCache world fingerprint)
  case cached of
    Just astar -> timed "cache force" (forceTileAStar astar)
    Nothing -> do
      astar <- timed "cold topology build" (buildTileAStar world >>= forceTileAStar)
      timed "cache write" (writeCache fingerprint astar)
      pure astar

loadCache :: World -> Word64 -> IO (Maybe TileAStar)
loadCache world expectedFingerprint = do
  exists <- doesFileExist cachePath
  if not exists
    then pure Nothing
    else do
      bytes <- timed "cache read" (BL.readFile cachePath)
      decodedResult <- timed "cache decode" (evaluate (runGetOrFail getCache bytes))
      case decodedResult of
        Right (rest, _, decoded)
          | not (BL.null rest) -> hPutStrLn stderr "routing topology cache has trailing bytes" >> pure Nothing
          | decodedFingerprint decoded /= expectedFingerprint -> hPutStrLn stderr "routing topology cache fingerprint mismatch" >> pure Nothing
          | otherwise -> do
              assembled <- timedAssemble world decoded
              case assembled of
                Right astar -> pure (Just astar)
                Left message -> hPutStrLn stderr ("routing topology cache rejected: " <> message) >> pure Nothing
        Left (_, _, message) -> hPutStrLn stderr ("routing topology cache decode failed: " <> message) >> pure Nothing

timedAssemble :: World -> DecodedCache -> IO (Either String TileAStar)
timedAssemble world decoded = timed "cache assemble" (evaluate (assembleCached world decoded))

writeCache :: Word64 -> TileAStar -> IO ()
writeCache fingerprint astar = do
  either fail pure (validateCacheSource astar)
  createDirectoryIfMissing True "out"
  let temporary = cachePath <> ".tmp"
  BL.writeFile temporary (runPut (putCache fingerprint astar))
  renameFile temporary cachePath

putCache :: Word64 -> TileAStar -> Put
putCache fingerprint (TileAStar topology static) = do
  putByteString cacheMagic
  putWord32be cacheVersion
  putWord64be fingerprint
  putComponents topology
  putCrossings (topologySeparatorCrossings topology)
  putIntVectorWord32 (Vector.fromList (map fromIntegral (IntSet.toAscList (structurallyReachableIds (topologyStructuralReachability topology)))))
  putStatic static

putComponents :: WorldTopology -> Put
putComponents topology = do
  let natural = topologyNaturalComponents topology
      routing = topologyRoutingComponents topology
  putIntVectorWord32 (componentOwnerTiles natural)
  putIntVectorWord32 (componentOwnerIds natural)
  putIntVectorWord32 (componentIds natural)
  putIntWord32 (maxComponentId natural)
  putIntVectorWord32 (componentOwnerIds routing)
  putIntVectorWord32 (componentIds routing)
  putIntWord32 (maxComponentId routing)

putCrossings :: [RoutingCrossing] -> Put
putCrossings crossings = do
  putCount (length crossings)
  mapM_ putCrossing crossings
 where
  putCrossing crossing = do
    putWord32be (fromIntegral (unTile (crossingFromTile crossing)))
    putWord32be (fromIntegral (unTile (crossingToTile crossing)))
    putWord32be (fromIntegral (crossingFromComponent crossing))
    putWord32be (fromIntegral (crossingToComponent crossing))
    putWord32be (fromIntegral (crossingCost crossing))

putStatic :: TileStatic -> Put
putStatic static = do
  putIntVectorWord32 (staticSearchTiles static)
  putIntVectorWord32 (staticSearchComponents static)
  putWord8Vector (staticWalkingMasks static)
  putInt32Vector (staticNorthNodes static)
  putInt32Vector (staticSouthNodes static)
  putIntVectorWord32 (staticTiles static)
  putCSR (staticComponents static)
  putCSR (staticSiteComponentIds static)
  putWord32Vector (Vector.fromList (map (fromIntegral . unTile) (Set.toAscList (staticReachableBanks static))))
  putSparseWalkingNetwork (staticWalkingNetwork static)

putSparseWalkingNetwork :: SparseWalkingNetwork -> Put
putSparseWalkingNetwork network = do
  mapM_ (putWord32be . fromIntegral)
    [ sparseOriginalCount network
    , sparseVertexCount network
    , sparseSteinerCount network
    , sparseWalkingEdgeCount network
    ]
  putIntVectorWord32 (sparseOffsets network)
  putIntVectorWord32 (sparseDestinations network)
  putIntVectorWord32 (sparseWeights network)
  putIntVectorInt32 (sparseComponentRoots network)
  putWord8Vector (sparseAttachmentNodeKinds network)
  putIntVectorInt32 (sparseAttachmentSplitCoords network)
  putIntVectorInt32 (sparseAttachmentChainOffsets network)
  putIntVectorWord32 (sparseAttachmentChainLengths network)
  putIntVectorInt32 (sparseAttachmentLeftChildren network)
  putIntVectorInt32 (sparseAttachmentRightChildren network)
  putIntVectorInt32 (sparseAttachmentLeafOriginals network)
  putIntVectorInt32 (sparseAttachmentChainCoords network)
  putIntVectorWord32 (sparseAttachmentChainVertices network)

putCSR :: Boxed.Vector (Vector.Vector Int) -> Put
putCSR groups = do
  let lengths = Vector.generate (Boxed.length groups) (Vector.length . (groups Boxed.!))
      offsets = Vector.scanl' (+) 0 lengths
      values = Vector.concat (Boxed.toList groups)
  putIntVectorWord32 offsets
  putIntVectorWord32 values

getCache :: Get DecodedCache
getCache = do
  magic <- getByteString (BS.length cacheMagic)
  when (magic /= cacheMagic) (fail "routing topology cache magic mismatch")
  version <- getWord32be
  when (version /= cacheVersion) (fail "routing topology cache version mismatch")
  fingerprint <- getWord64be
  ownerTiles <- getIntWord32Vector
  naturalIds <- getIntWord32Vector
  naturalComponentIds <- getIntWord32Vector
  naturalMax <- getIntWord32
  routingIds <- getIntWord32Vector
  routingComponentIds <- getIntWord32Vector
  routingMax <- getIntWord32
  crossings <- getCrossings
  reachableIds <- getIntWord32Vector
  static <- getStatic
  pure (DecodedCache fingerprint ownerTiles naturalIds naturalComponentIds naturalMax routingIds routingComponentIds routingMax crossings reachableIds static)

getCrossings :: Get [RoutingCrossing]
getCrossings = do
  count <- getCount
  replicateM count $ do
    fromTile <- Tile . fromIntegral <$> getWord32be
    toTile <- Tile . fromIntegral <$> getWord32be
    fromComponent <- getIntWord32
    toComponent <- getIntWord32
    cost <- getIntWord32
    pure (RoutingCrossing fromTile toTile fromComponent toComponent cost)

getStatic :: Get TileStatic
getStatic = do
  searchTiles <- getIntWord32Vector
  searchComponents <- getIntWord32Vector
  walkingMasks <- getWord8Vector
  northNodes <- getInt32Vector
  southNodes <- getInt32Vector
  tiles <- getIntWord32Vector
  components <- getCSR
  componentSites <- getCSR
  reachableBanks <- getIntWord32Vector
  network <- getSparseWalkingNetwork
  pure (TileStatic
    searchTiles
    searchComponents
    walkingMasks
    northNodes
    southNodes
    tiles
    components
    (IntMap.fromList [(tile, ix) | (ix, tile) <- Vector.toList (Vector.indexed tiles)])
    componentSites
    (Set.fromList (map Tile (Vector.toList reachableBanks)))
    network)

getSparseWalkingNetwork :: Get SparseWalkingNetwork
getSparseWalkingNetwork = do
  originalCount <- fromIntegral <$> getWord32be
  vertexCount <- fromIntegral <$> getWord32be
  steinerCount <- fromIntegral <$> getWord32be
  edgeCount <- fromIntegral <$> getWord32be
  offsets <- getIntWord32Vector
  destinations <- getIntWord32Vector
  weights <- getIntWord32Vector
  roots <- getInt32AsIntVector
  kinds <- getWord8Vector
  splits <- getInt32AsIntVector
  chainOffsets <- getInt32AsIntVector
  chainLengths <- getIntWord32Vector
  leftChildren <- getInt32AsIntVector
  rightChildren <- getInt32AsIntVector
  leafOriginals <- getInt32AsIntVector
  chainCoords <- getInt32AsIntVector
  chainVertices <- getIntWord32Vector
  pure (SparseWalkingNetwork
    originalCount vertexCount steinerCount edgeCount
    offsets destinations weights roots
    kinds
    splits chainOffsets chainLengths leftChildren rightChildren leafOriginals chainCoords chainVertices)

getCSR :: Get (Boxed.Vector (Vector.Vector Int))
getCSR = do
  offsets <- getIntWord32Vector
  values <- getIntWord32Vector
  validateOffsets offsets values
  pure (Boxed.generate (max 0 (Vector.length offsets - 1)) $ \ix ->
    let start = fromIntegral (offsets Vector.! ix)
        end = fromIntegral (offsets Vector.! (ix + 1))
     in Vector.slice start (end - start) values)

validateOffsets :: Vector.Vector Int -> Vector.Vector Int -> Get ()
validateOffsets offsets values = do
  when (Vector.null offsets) (fail "empty CSR offsets")
  when (offsets Vector.! 0 /= 0) (fail "CSR offsets do not start at zero")
  when (Vector.any (> Vector.length values) offsets) (fail "CSR offset exceeds value count")
  when (Vector.any id (Vector.zipWith (<) (Vector.tail offsets) offsets)) (fail "CSR offsets are not monotonic")

getIntWord32Vector :: Get (Vector.Vector Int)
getIntWord32Vector = do
  count <- getCount
  bytes <- getByteString (byteLength count 4)
  pure (Vector.generate count (fromIntegral . word32At bytes))

getIntWord32 :: Get Int
getIntWord32 = do
  value <- getWord32be
  let result = fromIntegral value
  when (fromIntegral result /= value) (fail "cache Word32 value does not fit in Int")
  pure result

getWord8Vector :: Get (Vector.Vector Word8)
getWord8Vector = do
  count <- getCount
  bytes <- getByteString (byteLength count 1)
  pure (Vector.generate count (BS.index bytes))

getInt32Vector :: Get (Vector.Vector Int32)
getInt32Vector = do
  count <- getCount
  bytes <- getByteString (byteLength count 4)
  pure (Vector.generate count (int32At bytes))

getInt32AsIntVector :: Get (Vector.Vector Int)
getInt32AsIntVector = do
  count <- getCount
  bytes <- getByteString (byteLength count 4)
  pure (Vector.generate count (fromIntegral . int32At bytes))

byteLength :: Int -> Int -> Int
byteLength count width
  | count > maxBound `div` width = error "cache vector byte length overflow"
  | otherwise = count * width

getCount :: Get Int
getCount = do
  count <- getWord32be
  let result = fromIntegral count
  when (fromIntegral result /= count) (fail "cache count does not fit in Int")
  when (result > 20000000) (fail "cache count is unreasonably large")
  pure result

putIntVectorWord32 :: Vector.Vector Int -> Put
putIntVectorWord32 values = do
  putCount (Vector.length values)
  Vector.forM_ values (putWord32be . fromIntegral)

putIntVectorInt32 :: Vector.Vector Int -> Put
putIntVectorInt32 values = do
  putCount (Vector.length values)
  Vector.forM_ values (putInt32be . fromIntegral)

putInt32Vector :: Vector.Vector Int32 -> Put
putInt32Vector values = do
  putCount (Vector.length values)
  Vector.forM_ values putInt32be

putWord8Vector :: Vector.Vector Word8 -> Put
putWord8Vector values = do
  putCount (Vector.length values)
  Vector.forM_ values putWord8

putWord32Vector :: Vector.Vector Word32 -> Put
putWord32Vector values = do
  putCount (Vector.length values)
  Vector.forM_ values putWord32be

putCount :: Int -> Put
putCount count = putWord32be (fromIntegral count)

putIntWord32 :: Int -> Put
putIntWord32 = putWord32be . fromIntegral

word32At :: BS.ByteString -> Int -> Word32
word32At bytes ix =
  let offset = ix * 4
      byte n = fromIntegral (BS.index bytes (offset + n))
   in (byte 0 `shiftL` 24) .|. (byte 1 `shiftL` 16) .|. (byte 2 `shiftL` 8) .|. byte 3

int32At :: BS.ByteString -> Int -> Int32
int32At bytes ix = fromIntegral (word32At bytes ix)

assembleCached :: World -> DecodedCache -> Either String TileAStar
assembleCached world decoded = do
  let ownerTiles = decodedOwnerTiles decoded
      natural = NaturalComponents ownerTiles
        (decodedNaturalIds decoded)
        (decodedNaturalComponentIds decoded)
        (decodedNaturalMaxComponentId decoded)
      routing = NaturalComponents ownerTiles
        (decodedRoutingIds decoded)
        (decodedRoutingComponentIds decoded)
        (decodedRoutingMaxComponentId decoded)
      topology = WorldTopology world natural routing (decodedCrossings decoded)
        (StructuralReachability (IntSet.fromList (Vector.toList (decodedReachableIds decoded))))
  validateCachedTopology topology decoded >>= \() -> pure (TileAStar topology (decodedStatic decoded))

validateCachedTopology :: WorldTopology -> DecodedCache -> Either String ()
validateCachedTopology topology decoded = do
  let natural = topologyNaturalComponents topology
      routing = topologyRoutingComponents topology
      ownerTiles = componentOwnerTiles natural
      naturalIds = componentOwnerIds natural
      routingIds = componentOwnerIds routing
      static = decodedStatic decoded
  require (Vector.length ownerTiles == Vector.length naturalIds) "natural component arrays differ in length"
  require (Vector.length ownerTiles == Vector.length routingIds) "routing component array differs in length"
  require (Vector.length ownerTiles == Vector.length (decodedOwnerTiles decoded)) "owner tile array changed during decode"
  require (Vector.length (staticSearchTiles static) == Vector.length (staticSearchComponents static)) "search arrays differ in length"
  require (Vector.length (staticSearchTiles static) == Vector.length (staticWalkingMasks static)) "search masks differ in length"
  require (Vector.length (staticSearchTiles static) == Vector.length (staticNorthNodes static)) "north array differs in length"
  require (Vector.length (staticSearchTiles static) == Vector.length (staticSouthNodes static)) "south array differs in length"
  let searchCount = Vector.length (staticSearchTiles static)
      searchCount32 = fromIntegral searchCount :: Int32
      routingCount = maxComponentId routing + 1
      staticComponentValues = Vector.concat (Boxed.toList (staticComponents static))
      siteComponentValues = Vector.concat (Boxed.toList (staticSiteComponentIds static))
  require (Vector.all (\cid -> cid >= 0 && cid < routingCount) (staticSearchComponents static)) "search component ID out of range"
  require (Vector.all (\cid -> cid >= 0 && cid < routingCount) staticComponentValues) "static component ID out of range"
  require (Vector.all (\cid -> cid >= 0 && cid < routingCount) siteComponentValues) "site component ID out of range"
  require (Vector.all (\node -> node == -1 || node >= 0 && node < searchCount32) (staticNorthNodes static)) "north node out of range"
  require (Vector.all (\node -> node == -1 || node >= 0 && node < searchCount32) (staticSouthNodes static)) "south node out of range"
  requireSorted "owner tiles" ownerTiles
  requireSorted "natural component IDs" (componentIds natural)
  requireSorted "routing component IDs" (componentIds routing)
  require (Vector.all (\cid -> cid >= 0 && cid <= maxComponentId natural) naturalIds) "natural component ID out of range"
  require (Vector.all (\cid -> cid >= 0 && cid <= maxComponentId routing) routingIds) "routing component ID out of range"
  require (Vector.all (\cid -> cid >= 0 && cid <= maxComponentId routing) (decodedReachableIds decoded)) "reachable component ID out of range"
  validateSparseNetwork (staticWalkingNetwork static)
  mapM_ (validateCrossing natural routing) (topologySeparatorCrossings topology)
 where
  require condition message = if condition then Right () else Left message
  requireSorted label values =
    require (Vector.and (Vector.zipWith (<) values (Vector.tail values))) (label <> " are not strictly sorted")
  validateCrossing natural routing crossing = do
    let maxId = maxComponentId routing
    require (crossingFromComponent crossing >= 0 && crossingFromComponent crossing <= maxId) "crossing source component out of range"
    require (crossingToComponent crossing >= 0 && crossingToComponent crossing <= maxId) "crossing target component out of range"
    require (componentOfTile routing (crossingFromTile crossing) == Just (crossingFromComponent crossing)) "crossing source tile mismatch"
    require (componentOfTile routing (crossingToTile crossing) == Just (crossingToComponent crossing)) "crossing target tile mismatch"
    require (componentOfTile natural (crossingFromTile crossing) /= Nothing) "crossing source tile is not walkable"
    require (componentOfTile natural (crossingToTile crossing) /= Nothing) "crossing target tile is not walkable"
  validateSparseNetwork network = do
    let offsets = sparseOffsets network
        destinations = sparseDestinations network
        vertices = sparseVertexCount network
    require (Vector.length offsets == vertices + 1) "sparse offsets length mismatch"
    require (not (Vector.null offsets) && Vector.head offsets == 0) "sparse offsets do not start at zero"
    require (not (Vector.null offsets) && Vector.last offsets == Vector.length destinations) "sparse offsets do not end at edge count"
    require (Vector.and (Vector.zipWith (<=) offsets (Vector.tail offsets))) "sparse offsets are not monotonic"
    require (Vector.all (\target -> target >= 0 && target < vertices) destinations) "sparse target out of range"
    let attachmentCount = Vector.length (sparseAttachmentNodeKinds network)
        validNode node = node == -1 || node >= 0 && node < attachmentCount
        validOriginal node = node == -1 || node >= 0 && node < sparseOriginalCount network
    require (Vector.length (sparseAttachmentChainOffsets network) == attachmentCount) "sparse chain offset count mismatch"
    require (Vector.length (sparseAttachmentChainLengths network) == attachmentCount) "sparse chain length count mismatch"
    require (Vector.length (sparseAttachmentLeftChildren network) == attachmentCount) "sparse left child count mismatch"
    require (Vector.length (sparseAttachmentRightChildren network) == attachmentCount) "sparse right child count mismatch"
    require (Vector.length (sparseAttachmentLeafOriginals network) == attachmentCount) "sparse leaf count mismatch"
    require (Vector.all validNode (sparseAttachmentLeftChildren network)) "sparse left child out of range"
    require (Vector.all validNode (sparseAttachmentRightChildren network)) "sparse right child out of range"
    require (Vector.all validOriginal (sparseAttachmentLeafOriginals network)) "sparse leaf original out of range"
    require (Vector.all (\vertex -> vertex >= 0 && vertex < vertices) (sparseAttachmentChainVertices network)) "sparse chain vertex out of range"

validateCacheSource :: TileAStar -> Either String ()
validateCacheSource (TileAStar topology static) = do
  validateWord32Vector "natural owner tiles" (componentOwnerTiles natural)
  validateWord32Vector "natural owner IDs" (componentOwnerIds natural)
  validateWord32Vector "natural component IDs" (componentIds natural)
  validateIntWord32 "natural max component ID" (maxComponentId natural)
  validateWord32Vector "routing owner IDs" (componentOwnerIds routing)
  validateWord32Vector "routing component IDs" (componentIds routing)
  validateIntWord32 "routing max component ID" (maxComponentId routing)
  validateWord32Vector "search tiles" (staticSearchTiles static)
  validateWord32Vector "search components" (staticSearchComponents static)
  validateWord32Vector "static tiles" (staticTiles static)
  validateWord32Vector "reachable banks" (Vector.fromList (map unTile (Set.toAscList (staticReachableBanks static))))
  validateWord32Vector "sparse offsets" (sparseOffsets network)
  validateWord32Vector "sparse destinations" (sparseDestinations network)
  validateWord32Vector "sparse weights" (sparseWeights network)
  validateInt32Vector "sparse chain offsets" (sparseAttachmentChainOffsets network)
  validateWord32Vector "sparse chain lengths" (sparseAttachmentChainLengths network)
  validateInt32Vector "sparse component roots" (sparseComponentRoots network)
  validateInt32Vector "sparse left children" (sparseAttachmentLeftChildren network)
  validateInt32Vector "sparse right children" (sparseAttachmentRightChildren network)
  validateInt32Vector "sparse leaf originals" (sparseAttachmentLeafOriginals network)
  validateWord32Vector "sparse chain vertices" (sparseAttachmentChainVertices network)
  validateInt32Vector "sparse split coordinates" (sparseAttachmentSplitCoords network)
  validateInt32Vector "sparse chain coordinates" (sparseAttachmentChainCoords network)
  mapM_ (validateCSR "static components") [staticComponents static, staticSiteComponentIds static]
  mapM_ validateCrossing (topologySeparatorCrossings topology)
 where
  natural = topologyNaturalComponents topology
  routing = topologyRoutingComponents topology
  network = staticWalkingNetwork static
  validateCSR label groups = validateWord32Vector label (Vector.concat (Boxed.toList groups))
  validateCrossing crossing = do
    validateIntWord32 "crossing source tile" (unTile (crossingFromTile crossing))
    validateIntWord32 "crossing target tile" (unTile (crossingToTile crossing))
    validateIntWord32 "crossing source component" (crossingFromComponent crossing)
    validateIntWord32 "crossing target component" (crossingToComponent crossing)
    validateIntWord32 "crossing cost" (crossingCost crossing)

validateIntWord32 :: String -> Int -> Either String ()
validateIntWord32 label value
  | value < 0 || toInteger value > toInteger (maxBound :: Word32) = Left (label <> " is outside Word32: " <> show value)
  | otherwise = Right ()

validateWord32Vector :: String -> Vector.Vector Int -> Either String ()
validateWord32Vector label values =
  case Vector.find (\value -> value < 0 || toInteger value > toInteger (maxBound :: Word32)) values of
    Nothing -> Right ()
    Just value -> Left (label <> " contains value outside Word32: " <> show value)

validateInt32Vector :: String -> Vector.Vector Int -> Either String ()
validateInt32Vector label values =
  case Vector.find (\value -> toInteger value < toInteger (minBound :: Int32) || toInteger value > toInteger (maxBound :: Int32)) values of
    Nothing -> Right ()
    Just value -> Left (label <> " contains value outside Int32: " <> show value)

sourceFingerprint :: World -> IO Word64
sourceFingerprint world = do
  files <- filter isFingerprintFile <$> (concat <$> mapM filesBelow sourceInputs)
  foldl' hashFile (pure fnvOffset) files
 where
  hashFile action path = do
    hash <- action
    bytes <- BL.readFile path
    pure (hashBytes (hashString hash path) bytes)

  sourceInputs =
    [ resourcesDir defaultSourcePaths
    , "src/ShortestPath/Transport.hs"
    , "src/ShortestPath/World.hs"
    , "src/ShortestPath/Tile.hs"
    , "src/ShortestPath/Separator.hs"
    , "src/ShortestPath/Topology.hs"
    , "src/ShortestPath/Exact/TileAStar"
    , "src/ShortestPath/Pathfinder.hs"
    , maybe (separatorFile defaultSourcePaths) id (worldSeparatorArtifactPath world)
    ]

  hashString hash = foldl' (\h character -> hashByte h (fromIntegral (ord character) :: Word64)) hash
  hashBytes = BL.foldlChunks (BS.foldl' hashByte)
  hashByte hash byte = (hash `xor` fromIntegral byte) * fnvPrime
  isFingerprintFile path = takeFileName path /= "Cache.hs" && takeExtension path `notElem` [".o", ".hi", ".dyn_o", ".dyn_hi", ".a"]

filesBelow :: FilePath -> IO [FilePath]
filesBelow path = do
  directory <- doesDirectoryExist path
  if not directory then pure [path] else do
    entries <- map (path </>) . sort <$> listDirectory path
    files <- filterM doesFileExist entries
    directories <- filterM doesDirectoryExist entries
    nested <- concat <$> mapM filesBelow directories
    pure (files <> nested)

fnvOffset, fnvPrime :: Word64
fnvOffset = 14695981039346656037
fnvPrime = 1099511628211

timed :: String -> IO a -> IO a
timed label action = do
  enabled <- (== Just "1") <$> lookupEnv "SPM_CACHE_TIMINGS"
  if not enabled
    then action
    else do
      started <- getMonotonicTimeNSec
      result <- action
      finished <- getMonotonicTimeNSec
      hPutStrLn stderr ("cache " <> label <> ": " <> show (fromIntegral (finished - started) / 1000000 :: Double) <> " ms")
      pure result
