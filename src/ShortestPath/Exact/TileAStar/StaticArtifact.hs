{-# LANGUAGE OverloadedStrings #-}

module ShortestPath.Exact.TileAStar.StaticArtifact
  ( RoutingStaticV1(..)
  , routingStaticV1
  , encodeRoutingStaticV1
  , decodeRoutingStaticV1
  , validateRoutingStaticV1
  , writeRoutingStaticV1
  , routingStaticPointAttachments
  ) where

import Control.Monad (when)
import Data.Binary.Get
  ( Get
  , getByteString
  , getInt32le
  , getRemainingLazyByteString
  , getWord32le
  , getWord8
  , lookAhead
  , runGetOrFail
  )
import Data.Binary.Put
  ( Put
  , putByteString
  , putInt32le
  , putWord32le
  , putWord8
  , runPut
  )
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Int (Int32)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Vector
import Data.Word (Word32, Word8)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import ShortestPath.Exact.TileAStar.SparseWalking
import ShortestPath.Exact.TileAStar.Types
import ShortestPath.Tile
import ShortestPath.Topology
import ShortestPath.Transport
import ShortestPath.World

data RoutingStaticV1 = RoutingStaticV1
  { artifactSearchTiles :: !(Vector.Vector Word32)
  , artifactSearchComponents :: !(Vector.Vector Int32)
  , artifactWalkingMasks :: !(Vector.Vector Word8)
  , artifactNorthNodes :: !(Vector.Vector Int32)
  , artifactSouthNodes :: !(Vector.Vector Int32)
  , artifactSiteTiles :: !(Vector.Vector Word32)
  , artifactSiteComponentOffsets :: !(Vector.Vector Word32)
  , artifactSiteComponentIds :: !(Vector.Vector Int32)
  , artifactRoutingComponentCount :: !Word32
  , artifactComponentSiteOffsets :: !(Vector.Vector Word32)
  , artifactComponentSiteIds :: !(Vector.Vector Int32)
  , artifactReachableBankTiles :: !(Vector.Vector Word32)
  , artifactCrossingFromSite :: !(Vector.Vector Int32)
  , artifactCrossingToSite :: !(Vector.Vector Int32)
  , artifactCrossingCosts :: !(Vector.Vector Int32)
  , artifactSparseOriginalCount :: !Word32
  , artifactSparseVertexCount :: !Word32
  , artifactSparseSteinerCount :: !Word32
  , artifactSparseUndirectedEdgeCount :: !Word32
  , artifactSparseAdjacencyCount :: !Word32
  , artifactSparseOffsets :: !(Vector.Vector Word32)
  , artifactSparseDestinations :: !(Vector.Vector Int32)
  , artifactSparseWeights :: !(Vector.Vector Int32)
  }
  deriving stock (Eq, Show)

magic :: BS.ByteString
magic = "RSPWORLD"

routingStaticV1 :: TileAStar -> Either String RoutingStaticV1
routingStaticV1 (TileAStar topology static) = do
  searchTiles <- checkedPackedVector "search tiles" (staticSearchTiles static)
  searchComponents <- checkedInt32Vector "search components" (staticSearchComponents static)
  siteTiles <- checkedPackedVector "site tiles" (staticTiles static)
  (siteComponentOffsets, siteComponentIds) <- toCSR (staticComponents static)
  (componentSiteOffsets, componentSiteIds) <- toCSR (staticSiteComponentIds static)
  reachableBankTiles <- checkedPackedList "reachable bank tiles" (Set.toAscList (staticReachableBanks static))
  (crossingFromSite, crossingToSite, crossingCosts) <- crossingArrays (staticTiles static) (topologySeparatorCrossings topology)
  let network = staticWalkingNetwork static
  sparseOffsets <- checkedOffsetVector "sparse offsets" (sparseOffsets network)
  sparseDestinationValues <- checkedInt32Vector "sparse destinations" (sparseDestinations network)
  sparseWeights <- checkedNonNegativeInt32Vector "sparse weights" (sparseWeights network)
  sparseOriginalCount <- checkedCountInt "sparse original count" (sparseOriginalCount network)
  sparseVertexCount <- checkedCountInt "sparse vertex count" (sparseVertexCount network)
  sparseSteinerCount <- checkedCountInt "sparse Steiner count" (sparseSteinerCount network)
  sparseEdgeCount <- checkedCountInt "sparse edge count" (sparseWalkingEdgeCount network)
  sparseAdjacencyCount <- checkedCountInt "sparse adjacency count" (Vector.length sparseDestinationValues)
  componentCount <- checkedCountInt "routing component count" (Boxed.length (staticSiteComponentIds static))
  let result = RoutingStaticV1
        searchTiles
        searchComponents
        (staticWalkingMasks static)
        (staticNorthNodes static)
        (staticSouthNodes static)
        siteTiles
        siteComponentOffsets
        siteComponentIds
        componentCount
        componentSiteOffsets
        componentSiteIds
        reachableBankTiles
        crossingFromSite
        crossingToSite
        crossingCosts
        sparseOriginalCount
        sparseVertexCount
        sparseSteinerCount
        sparseEdgeCount
        sparseAdjacencyCount
        sparseOffsets
        sparseDestinationValues
        sparseWeights
  validateSource topology static result
  validateRoutingStaticV1 result
  pure result

checkedPackedVector :: String -> Vector.Vector Int -> Either String (Vector.Vector Word32)
checkedPackedVector label = Vector.mapM (checkedPackedTile label)

checkedPackedList :: String -> [Tile] -> Either String (Vector.Vector Word32)
checkedPackedList label = fmap Vector.fromList . mapM (checkedPackedTile label . unTile)

checkedPackedTile :: String -> Int -> Either String Word32
checkedPackedTile label value
  | value < 0 || toInteger value > toInteger (maxBound :: Word32) = Left (label <> " is not a packed u32 tile: " <> show value)
  | otherwise = Right (fromIntegral value)

checkedInt32Vector :: String -> Vector.Vector Int -> Either String (Vector.Vector Int32)
checkedInt32Vector label = Vector.mapM (checkedInt32 label)

checkedNonNegativeInt32Vector :: String -> Vector.Vector Int -> Either String (Vector.Vector Int32)
checkedNonNegativeInt32Vector label = Vector.mapM (checkedNonNegativeInt32 label)

checkedInt32 :: String -> Int -> Either String Int32
checkedInt32 label value
  | integer < toInteger (minBound :: Int32) || integer > toInteger (maxBound :: Int32) = Left (label <> " does not fit i32: " <> show value)
  | otherwise = Right (fromIntegral value)
 where
  integer = toInteger value

checkedNonNegativeInt32 :: String -> Int -> Either String Int32
checkedNonNegativeInt32 label value
  | value < 0 = Left (label <> " contains a negative value: " <> show value)
  | otherwise = checkedInt32 label value

checkedOffsetVector :: String -> Vector.Vector Int -> Either String (Vector.Vector Word32)
checkedOffsetVector label = Vector.mapM (checkedOffset label)

checkedOffset :: String -> Int -> Either String Word32
checkedOffset label value
  | value < 0 || toInteger value > toInteger (maxBound :: Int32) = Left (label <> " is not a Java-sized offset: " <> show value)
  | otherwise = Right (fromIntegral value)

checkedCountInt :: String -> Int -> Either String Word32
checkedCountInt label value
  | value < 0 || toInteger value > toInteger (maxBound :: Int32) = Left (label <> " exceeds Integer.MAX_VALUE: " <> show value)
  | otherwise = Right (fromIntegral value)

toCSR :: Boxed.Vector (Vector.Vector Int) -> Either String (Vector.Vector Word32, Vector.Vector Int32)
toCSR groups = do
  offsets <- Vector.fromList <$> go 0 (Boxed.toList groups)
  values <- Vector.concat <$> pure (Boxed.toList groups)
  values' <- checkedNonNegativeInt32Vector "CSR values" values
  pure (offsets, values')
 where
  go :: Integer -> [Vector.Vector Int] -> Either String [Word32]
  go total [] = pure [fromInteger total]
  go total (group:rest) = do
    current <- checkedIntegerOffset total
    _ <- checkedIntegerOffset (total + toInteger (Vector.length group))
    (current :) <$> go (total + toInteger (Vector.length group)) rest
  checkedIntegerOffset :: Integer -> Either String Word32
  checkedIntegerOffset value
    | value < 0 || value > toInteger (maxBound :: Int32) = Left "CSR offset exceeds Integer.MAX_VALUE"
    | otherwise = Right (fromInteger value)

crossingArrays :: Vector.Vector Int -> [RoutingCrossing] -> Either String (Vector.Vector Int32, Vector.Vector Int32, Vector.Vector Int32)
crossingArrays sites crossings = do
  let index = IntMap.fromList [(tile, ix) | (ix, tile) <- Vector.toList (Vector.indexed sites)]
  rows <- mapM (crossingRow index) crossings
  pure (Vector.fromList [from | (from, _, _) <- rows], Vector.fromList [to | (_, to, _) <- rows], Vector.fromList [cost | (_, _, cost) <- rows])
 where
  crossingRow index crossing = do
    from <- siteFor "separator crossing source" index (unTile (crossingFromTile crossing))
    to <- siteFor "separator crossing destination" index (unTile (crossingToTile crossing))
    cost <- checkedInt32 "separator crossing cost" (crossingCost crossing)
    pure (from, to, cost)
  siteFor label index tile =
    case IntMap.lookup tile index of
      Nothing -> Left (label <> " is not a static site: " <> show tile)
      Just value -> checkedInt32 label value

validateSource :: WorldTopology -> TileStatic -> RoutingStaticV1 -> Either String ()
validateSource topology static artifact = do
  when (Vector.length (staticSearchTiles static) /= Vector.length (staticSearchComponents static)) (Left "source search arrays are not aligned")
  when (Vector.length (staticSearchTiles static) /= Vector.length (staticWalkingMasks static)) (Left "source walking masks are not aligned")
  when (Vector.length (staticSearchTiles static) /= Vector.length (staticNorthNodes static)) (Left "source north nodes are not aligned")
  when (Vector.length (staticSearchTiles static) /= Vector.length (staticSouthNodes static)) (Left "source south nodes are not aligned")
  when (IntMap.size (staticSiteTileIndex static) /= Vector.length (staticTiles static)) (Left "source site index is not aligned")
  when (Vector.length (staticTiles static) /= sparseOriginalCount (staticWalkingNetwork static)) (Left "source sparse original count differs from site count")
  validateRoutingStaticV1 artifact
  let world = topologyWorld topology
      sites = Vector.toList (staticTiles static)
      endpoints = concat (map (\transport -> maybeToList (origin transport) <> maybeToList (destination transport)) (concat (Map.elems (worldTransports world)) <> worldGlobalTeleports world))
      crossingEndpoints = concat [[crossingFromTile edge, crossingToTile edge] | edge <- topologySeparatorCrossings topology]
      siteSet = IntSet.fromList sites
  mapM_ (requireSite siteSet "reachable bank") (Set.toList (staticReachableBanks static))
  mapM_ (requireSite siteSet "transport endpoint") endpoints
  mapM_ (requireSite siteSet "separator endpoint") crossingEndpoints
  pure ()
 where
  requireSite sites label tile = when (IntSet.notMember (unTile tile) sites) (Left (label <> " missing from static sites: " <> show tile))
  maybeToList Nothing = []
  maybeToList (Just value) = [value]

validateRoutingStaticV1 :: RoutingStaticV1 -> Either String ()
validateRoutingStaticV1 artifact = do
  let searchCount = Vector.length (artifactSearchTiles artifact)
      siteCount = Vector.length (artifactSiteTiles artifact)
      componentCount = fromIntegral (artifactRoutingComponentCount artifact) :: Integer
      sparseVertexCount' = fromIntegral (artifactSparseVertexCount artifact) :: Integer
  checkCount "search count" searchCount
  checkCount "site count" siteCount
  checkCount "routing component count" (fromIntegral (artifactRoutingComponentCount artifact))
  checkAligned "search components" searchCount (Vector.length (artifactSearchComponents artifact))
  checkAligned "walking masks" searchCount (Vector.length (artifactWalkingMasks artifact))
  checkAligned "north nodes" searchCount (Vector.length (artifactNorthNodes artifact))
  checkAligned "south nodes" searchCount (Vector.length (artifactSouthNodes artifact))
  checkStrictlyAscending "search tiles" (artifactSearchTiles artifact)
  checkStrictlyAscending "site tiles" (artifactSiteTiles artifact)
  validateComponentRefs componentCount (artifactSearchComponents artifact)
  validateNodeRefs searchCount (artifactNorthNodes artifact)
  validateNodeRefs searchCount (artifactSouthNodes artifact)
  validateCSR "site components" siteCount componentCount (artifactSiteComponentOffsets artifact) (artifactSiteComponentIds artifact)
  validateCSR "component sites" (fromIntegral (artifactRoutingComponentCount artifact)) (toInteger siteCount) (artifactComponentSiteOffsets artifact) (artifactComponentSiteIds artifact)
  checkStrictlyAscending "reachable bank tiles" (artifactReachableBankTiles artifact)
  validatePackedSubset "reachable bank tiles" (artifactSiteTiles artifact) (artifactReachableBankTiles artifact)
  validateCrossings siteCount artifact
  checkAligned "crossing destinations" (Vector.length (artifactCrossingFromSite artifact)) (Vector.length (artifactCrossingToSite artifact))
  checkAligned "crossing costs" (Vector.length (artifactCrossingFromSite artifact)) (Vector.length (artifactCrossingCosts artifact))
  validateRelation artifact
  validateSparse siteCount sparseVertexCount' artifact
  pure ()

checkCount :: String -> Int -> Either String ()
checkCount label value
  | value < 0 || toInteger value > toInteger (maxBound :: Int32) = Left (label <> " exceeds Integer.MAX_VALUE")
  | otherwise = pure ()

checkAligned :: String -> Int -> Int -> Either String ()
checkAligned label expected actual = when (expected /= actual) (Left (label <> " length mismatch: expected " <> show expected <> ", got " <> show actual))

checkStrictlyAscending :: (Vector.Unbox a, Ord a, Show a) => String -> Vector.Vector a -> Either String ()
checkStrictlyAscending label values = Vector.ifoldM'_ step () values
 where
  step _ 0 _ = pure ()
  step _ ix value
    | value > values Vector.! (ix - 1) = pure ()
    | otherwise = Left (label <> " is not strictly ascending at index " <> show ix)

validateNodeRefs :: Int -> Vector.Vector Int32 -> Either String ()
validateNodeRefs searchCount = Vector.ifoldM'_ check ()
 where
  check _ ix value
    | value == -1 = pure ()
    | value >= 0 && toInteger value < toInteger searchCount = pure ()
    | otherwise = Left ("node reference out of range at index " <> show ix)

validateComponentRefs :: Integer -> Vector.Vector Int32 -> Either String ()
validateComponentRefs componentCount = Vector.ifoldM'_ check ()
 where
  check _ ix value = when (value < 0 || toInteger value >= componentCount) (Left ("search component out of range at index " <> show ix))

validatePackedSubset :: String -> Vector.Vector Word32 -> Vector.Vector Word32 -> Either String ()
validatePackedSubset label universe values = Vector.ifoldM'_ check () values
 where
  check _ ix value = when (binarySearchWord32 value universe == Nothing) (Left (label <> " tile is not a static site at index " <> show ix))

validateCSR :: String -> Int -> Integer -> Vector.Vector Word32 -> Vector.Vector Int32 -> Either String ()
validateCSR label groupCount valueLimit offsets values = do
  checkCount (label <> " value count") (Vector.length values)
  checkAligned (label <> " offsets") (groupCount + 1) (Vector.length offsets)
  when (Vector.null offsets || Vector.head offsets /= 0) (Left (label <> " offsets must start at zero"))
  Vector.ifoldM'_ checkOffsetValue () offsets
  when (not (Vector.null offsets) && fromIntegral (Vector.last offsets) /= toInteger (Vector.length values)) (Left (label <> " final offset does not equal value count"))
  Vector.ifoldM'_ checkValue () values
 where
  checkOffsetValue _ 0 _ = pure ()
  checkOffsetValue _ ix value
    | value >= offsets Vector.! (ix - 1) = pure ()
    | otherwise = Left (label <> " offsets are not monotonic at index " <> show ix)
  checkValue _ ix value
    | value >= 0 && toInteger value < valueLimit = pure ()
    | otherwise = Left (label <> " value out of range at index " <> show ix)

validateCrossings :: Int -> RoutingStaticV1 -> Either String ()
validateCrossings siteCount artifact = Vector.ifoldM'_ check () (artifactCrossingFromSite artifact)
 where
  check _ ix from = do
    let to = artifactCrossingToSite artifact Vector.! ix
        cost = artifactCrossingCosts artifact Vector.! ix
    validateSite "crossing source" ix from
    validateSite "crossing destination" ix to
    when (from == to) (Left ("self separator crossing at index " <> show ix))
    when (cost < 0) (Left ("negative separator crossing cost at index " <> show ix))
  validateSite label ix value = when (value < 0 || toInteger value >= toInteger siteCount) (Left (label <> " out of range at index " <> show ix))

validateRelation :: RoutingStaticV1 -> Either String ()
validateRelation artifact = do
  let siteCount = Vector.length (artifactSiteTiles artifact)
      componentCount = fromIntegral (artifactRoutingComponentCount artifact) :: Int
      siteComponents = csrSets (artifactSiteComponentOffsets artifact) (artifactSiteComponentIds artifact)
      componentSites = csrSets (artifactComponentSiteOffsets artifact) (artifactComponentSiteIds artifact)
  mapM_ (checkSite componentSites) [0 .. siteCount - 1]
  mapM_ (checkComponent siteComponents) [0 .. componentCount - 1]
 where
  checkSite componentSites site = mapM_ (checkMembership ("site/component relation at site " <> show site) site) (IntSet.toList (siteComponents Boxed.! site))
   where
    siteComponents = csrSets (artifactSiteComponentOffsets artifact) (artifactSiteComponentIds artifact)
    checkMembership label siteId component = when (not (IntSet.member siteId (componentSites Boxed.! component))) (Left (label <> " missing reverse membership"))
  checkComponent siteComponents component = mapM_ (checkMembership ("component/site relation at component " <> show component) component) (IntSet.toList (componentSites Boxed.! component))
   where
    componentSites = csrSets (artifactComponentSiteOffsets artifact) (artifactComponentSiteIds artifact)
    checkMembership label componentId site = when (not (IntSet.member componentId (siteComponents Boxed.! site))) (Left (label <> " missing reverse membership"))

csrSets :: Vector.Vector Word32 -> Vector.Vector Int32 -> Boxed.Vector IntSet.IntSet
csrSets offsets values = Boxed.generate (max 0 (Vector.length offsets - 1)) $ \ix ->
  IntSet.fromList [fromIntegral (values Vector.! pos) | pos <- [fromIntegral (offsets Vector.! ix) .. fromIntegral (offsets Vector.! (ix + 1)) - 1]]

validateSparse :: Int -> Integer -> RoutingStaticV1 -> Either String ()
validateSparse siteCount vertexCount artifact = do
  let original = fromIntegral (artifactSparseOriginalCount artifact) :: Integer
      steiner = fromIntegral (artifactSparseSteinerCount artifact) :: Integer
      edges = fromIntegral (artifactSparseUndirectedEdgeCount artifact) :: Integer
      adjacency = fromIntegral (artifactSparseAdjacencyCount artifact) :: Integer
  checkCount "sparse original count" (fromIntegral (artifactSparseOriginalCount artifact))
  checkCount "sparse vertex count" (fromIntegral (artifactSparseVertexCount artifact))
  checkCount "sparse Steiner count" (fromIntegral (artifactSparseSteinerCount artifact))
  checkCount "sparse edge count" (fromIntegral (artifactSparseUndirectedEdgeCount artifact))
  checkCount "sparse adjacency count" (fromIntegral (artifactSparseAdjacencyCount artifact))
  when (original /= toInteger siteCount) (Left "sparse original count differs from site count")
  when (steiner /= vertexCount - original) (Left "sparse Steiner count is inconsistent")
  checkAligned "sparse offsets" (fromIntegral vertexCount + 1) (Vector.length offsets)
  when (Vector.null offsets || Vector.head offsets /= 0) (Left "sparse offsets must start at zero")
  Vector.ifoldM'_ checkOffsetValue () offsets
  when (not (Vector.null offsets) && fromIntegral (Vector.last offsets) /= adjacency) (Left "sparse final offset does not equal adjacency count")
  when (adjacency /= 2 * edges) (Left "sparse adjacency count is not twice the undirected edge count")
  checkAligned "sparse destinations" (fromIntegral adjacency) (Vector.length (artifactSparseDestinations artifact))
  checkAligned "sparse weights" (fromIntegral adjacency) (Vector.length (artifactSparseWeights artifact))
  Vector.ifoldM'_ checkDestination () (artifactSparseDestinations artifact)
  Vector.ifoldM'_ checkWeight () (artifactSparseWeights artifact)
 where
  offsets = artifactSparseOffsets artifact
  checkOffsetValue _ 0 _ = pure ()
  checkOffsetValue _ ix value
    | value >= offsets Vector.! (ix - 1) = pure ()
    | otherwise = Left ("sparse offsets are not monotonic at index " <> show ix)
  checkDestination _ ix value = when (value < 0 || toInteger value >= vertexCount) (Left ("sparse destination out of range at index " <> show ix))
  checkWeight _ ix value = when (value < 0) (Left ("negative sparse weight at index " <> show ix))

encodeRoutingStaticV1 :: RoutingStaticV1 -> Either String BL.ByteString
encodeRoutingStaticV1 artifact = do
  validateRoutingStaticV1 artifact
  pure (runPut (putRoutingStaticV1 artifact))

putRoutingStaticV1 :: RoutingStaticV1 -> Put
putRoutingStaticV1 artifact = do
  putByteString magic
  putWord32le 1
  putWord32le 0
  putCount (artifactSearchTiles artifact)
  putWord32Vector (artifactSearchTiles artifact)
  putInt32Vector (artifactSearchComponents artifact)
  putWord8Vector (artifactWalkingMasks artifact)
  putInt32Vector (artifactNorthNodes artifact)
  putInt32Vector (artifactSouthNodes artifact)
  putCount (artifactSiteTiles artifact)
  putWord32Vector (artifactSiteTiles artifact)
  putCount (artifactSiteComponentIds artifact)
  putWord32Vector (artifactSiteComponentOffsets artifact)
  putInt32Vector (artifactSiteComponentIds artifact)
  putWord32le (artifactRoutingComponentCount artifact)
  putCount (artifactComponentSiteIds artifact)
  putWord32Vector (artifactComponentSiteOffsets artifact)
  putInt32Vector (artifactComponentSiteIds artifact)
  putCount (artifactReachableBankTiles artifact)
  putWord32Vector (artifactReachableBankTiles artifact)
  putCount (artifactCrossingFromSite artifact)
  putInt32Vector (artifactCrossingFromSite artifact)
  putInt32Vector (artifactCrossingToSite artifact)
  putInt32Vector (artifactCrossingCosts artifact)
  putWord32le (artifactSparseOriginalCount artifact)
  putWord32le (artifactSparseVertexCount artifact)
  putWord32le (artifactSparseSteinerCount artifact)
  putWord32le (artifactSparseUndirectedEdgeCount artifact)
  putWord32le (artifactSparseAdjacencyCount artifact)
  putWord32Vector (artifactSparseOffsets artifact)
  putInt32Vector (artifactSparseDestinations artifact)
  putInt32Vector (artifactSparseWeights artifact)
 where
  putCount values = putWord32le (fromIntegral (Vector.length values))

putWord32Vector :: Vector.Vector Word32 -> Put
putWord32Vector = Vector.mapM_ putWord32le

putInt32Vector :: Vector.Vector Int32 -> Put
putInt32Vector = Vector.mapM_ putInt32le

putWord8Vector :: Vector.Vector Word8 -> Put
putWord8Vector = Vector.mapM_ putWord8

decodeRoutingStaticV1 :: BL.ByteString -> Either String RoutingStaticV1
decodeRoutingStaticV1 bytes =
  case runGetOrFail getRoutingStaticV1 bytes of
    Left (_, offset, message) -> Left ("routing-static-v1 decode at byte " <> show offset <> ": " <> message)
    Right (rest, _, artifact)
      | not (BL.null rest) -> Left "routing-static-v1 has trailing bytes"
      | otherwise -> validateRoutingStaticV1 artifact >> pure artifact

getRoutingStaticV1 :: Get RoutingStaticV1
getRoutingStaticV1 = do
  actualMagic <- getByteString 8
  when (actualMagic /= magic) (fail "wrong magic")
  version <- getWord32le
  when (version /= 1) (fail ("unsupported version " <> show version))
  flags <- getWord32le
  when (flags /= 0) (fail "non-zero flags")
  searchCount <- getCount "search count"
  searchTiles <- getWord32VectorN "search tiles" searchCount
  searchComponents <- getInt32VectorN "search components" searchCount
  walkingMasks <- getWord8VectorN "walking masks" searchCount
  northNodes <- getInt32VectorN "north nodes" searchCount
  southNodes <- getInt32VectorN "south nodes" searchCount
  siteCount <- getCount "site count"
  siteTiles <- getWord32VectorN "site tiles" siteCount
  siteComponentValueCount <- getCount "site component value count"
  siteComponentOffsets <- getWord32VectorN "site component offsets" =<< plusOne "site count" siteCount
  siteComponentIds <- getInt32VectorN "site component ids" siteComponentValueCount
  routingComponentCount <- getWord32le
  checkRawCount "routing component count" routingComponentCount
  componentSiteValueCount <- getCount "component site value count"
  componentSiteOffsets <- getWord32VectorN "component site offsets" =<< plusOneWord "routing component count" routingComponentCount
  componentSiteIds <- getInt32VectorN "component site ids" componentSiteValueCount
  bankCount <- getCount "reachable bank count"
  reachableBankTiles <- getWord32VectorN "reachable bank tiles" bankCount
  crossingCount <- getCount "crossing count"
  crossingFromSite <- getInt32VectorN "crossing source sites" crossingCount
  crossingToSite <- getInt32VectorN "crossing destination sites" crossingCount
  crossingCosts <- getInt32VectorN "crossing costs" crossingCount
  sparseOriginalCount <- getWord32le
  sparseVertexCount <- getWord32le
  sparseSteinerCount <- getWord32le
  sparseUndirectedEdgeCount <- getWord32le
  sparseAdjacencyCount <- getWord32le
  mapM_ (uncurry checkRawCount)
    [ ("sparse original count", sparseOriginalCount)
    , ("sparse vertex count", sparseVertexCount)
    , ("sparse Steiner count", sparseSteinerCount)
    , ("sparse edge count", sparseUndirectedEdgeCount)
    , ("sparse adjacency count", sparseAdjacencyCount)
    ]
  sparseOffsets <- getWord32VectorN "sparse offsets" =<< plusOneWord "sparse vertex count" sparseVertexCount
  sparseAdjacencyCount' <- rawCountToInt "sparse adjacency count" sparseAdjacencyCount
  sparseDestinations <- getInt32VectorN "sparse destinations" sparseAdjacencyCount'
  sparseWeights <- getInt32VectorN "sparse weights" sparseAdjacencyCount'
  pure (RoutingStaticV1 searchTiles searchComponents walkingMasks northNodes southNodes siteTiles siteComponentOffsets siteComponentIds routingComponentCount componentSiteOffsets componentSiteIds reachableBankTiles crossingFromSite crossingToSite crossingCosts sparseOriginalCount sparseVertexCount sparseSteinerCount sparseUndirectedEdgeCount sparseAdjacencyCount sparseOffsets sparseDestinations sparseWeights)

getCount :: String -> Get Int
getCount label = rawCountToInt label =<< getWord32le

rawCountToInt :: String -> Word32 -> Get Int
rawCountToInt label value
  | value > fromIntegral (maxBound :: Int32) = fail (label <> " exceeds Integer.MAX_VALUE")
  | otherwise = do
      remaining <- lookAhead (BL.length <$> getRemainingLazyByteString)
      when (remaining < 0) (fail "negative remaining input")
      pure (fromIntegral value)

checkRawCount :: String -> Word32 -> Get ()
checkRawCount label value = when (value > fromIntegral (maxBound :: Int32)) (fail (label <> " exceeds Integer.MAX_VALUE"))

plusOne :: String -> Int -> Get Int
plusOne label value
  | value == maxBound = fail (label <> " cannot be represented with a CSR terminator")
  | otherwise = pure (value + 1)

plusOneWord :: String -> Word32 -> Get Int
plusOneWord label value
  | value >= fromIntegral (maxBound :: Int32) = fail (label <> " cannot be represented with a CSR terminator")
  | otherwise = pure (fromIntegral value + 1)

getWord32VectorN :: String -> Int -> Get (Vector.Vector Word32)
getWord32VectorN label count = getVectorN label 4 getWord32le count

getInt32VectorN :: String -> Int -> Get (Vector.Vector Int32)
getInt32VectorN label count = getVectorN label 4 getInt32le count

getWord8VectorN :: String -> Int -> Get (Vector.Vector Word8)
getWord8VectorN label count = getVectorN label 1 getWord8 count

getVectorN :: Vector.Unbox a => String -> Integer -> Get a -> Int -> Get (Vector.Vector a)
getVectorN label width getter count = do
  remaining <- lookAhead (BL.length <$> getRemainingLazyByteString)
  when (toInteger count * width > toInteger remaining) (fail (label <> " is truncated"))
  Vector.generateM count (const getter)

routingStaticPointAttachments :: RoutingStaticV1 -> (Tile -> [Tile]) -> Tile -> [Int]
routingStaticPointAttachments artifact neighborFunction point =
  IntSet.toAscList (IntSet.fromList components)
 where
  packed = fromIntegral (unTile point) :: Word32
  components = case binarySearchWord32 packed (artifactSearchTiles artifact) of
    Just ix -> [fromIntegral (artifactSearchComponents artifact Vector.! ix)]
    Nothing ->
      [ fromIntegral (artifactSearchComponents artifact Vector.! ix)
      | neighbor <- neighborFunction point
      , Just ix <- [binarySearchWord32 (fromIntegral (unTile neighbor)) (artifactSearchTiles artifact)]
      ]

binarySearchWord32 :: Word32 -> Vector.Vector Word32 -> Maybe Int
binarySearchWord32 target values = go 0 (Vector.length values)
 where
  go low high
    | low >= high = Nothing
    | otherwise =
        let middle = low + (high - low) `div` 2
            value = values Vector.! middle
         in case compare target value of
              EQ -> Just middle
              LT -> go low middle
              GT -> go (middle + 1) high

writeRoutingStaticV1 :: FilePath -> TileAStar -> IO (Integer, RoutingStaticV1)
writeRoutingStaticV1 path astar = do
  artifact <- either fail pure (routingStaticV1 astar)
  bytes <- either fail pure (encodeRoutingStaticV1 artifact)
  createDirectoryIfMissing True (takeDirectory path)
  BL.writeFile path bytes
  decoded <- either fail pure . decodeRoutingStaticV1 =<< BL.readFile path
  when (decoded /= artifact) (fail "routing-static-v1 round-trip differs from source")
  pure (toInteger (BL.length bytes), decoded)
