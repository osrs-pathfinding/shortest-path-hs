module ShortestPath.Topology
  ( NaturalComponents(..)
  , RoutingCrossing(..)
  , StructuralReachabilityPolicy(..)
  , ReachabilityError(..)
  , StructuralReachability(..)
  , WorldTopology(..)
  , productionStructuralReachabilityPolicy
  , buildWorldTopology
  , buildWorldTopologyWithPolicy
  , renderReachabilityError
  , worldTopologyFromComponents
  , naturalComponents
  , walkingTopologyIdentity
  , withEmptySeparatorArtifact
  , componentOfTile
  , routingComponentOfTile
  , pointAttachments
  , pointAttachmentDetails
  , structurallyReachablePointAttachments
  , routingPointAttachments
  , componentIsStructurallyReachable
  , reachableComponentTiles
  , routingComponentFacts
  , routingTileFacts
  , separatorCrossingFacts
  , componentFacts
  , tileFacts
  ) where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Binary (Binary(..))
import Data.Bits (xor, shiftR)
import Data.List (sort)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as Mutable
import Data.Word (Word64)
import Numeric (showHex)

import ShortestPath.Tile
import ShortestPath.Separator
import ShortestPath.Transport
import ShortestPath.World

data NaturalComponents = NaturalComponents
  { componentOwnerTiles :: Vector.Vector Int
  , componentOwnerIds :: Vector.Vector Int
  , componentIds :: Vector.Vector Int
  , maxComponentId :: !Int
  }

instance Binary NaturalComponents where
  put components = do
    put (Vector.toList (componentOwnerTiles components))
    put (Vector.toList (componentOwnerIds components))
    put (Vector.toList (componentIds components))
    put (maxComponentId components)
  get = NaturalComponents
    <$> (Vector.fromList <$> get)
    <*> (Vector.fromList <$> get)
    <*> (Vector.fromList <$> get)
    <*> get

data StructuralReachabilityPolicy = StructuralReachabilityPolicy
  { structuralReachabilitySeeds :: [Tile]
  , structuralIgnoredTransportTypes :: Set.Set String
  }
  deriving stock (Eq, Show)

data ReachabilityError
  = NoStructuralReachabilitySeeds
  | MissingStructuralReachabilitySeed Tile
  | MissingSeparatorArtifact
  | SeparatorFormatMismatch String
  | SeparatorTopologyMismatch String String
  | DuplicateSeparatorCuts
  | InvalidSeparatorCut SeparatorCut String
  deriving stock (Eq, Show)

newtype StructuralReachability = StructuralReachability
  { structurallyReachableIds :: IntSet.IntSet
  }
  deriving stock (Eq, Show)

data WorldTopology = WorldTopology
  { topologyWorld :: World
  , topologyNaturalComponents :: NaturalComponents
  , topologyRoutingComponents :: NaturalComponents
  , topologySeparatorCrossings :: [RoutingCrossing]
  , topologyStructuralReachability :: StructuralReachability
  }

data RoutingCrossing = RoutingCrossing
  { crossingFromTile :: !Tile
  , crossingToTile :: !Tile
  , crossingFromComponent :: !Int
  , crossingToComponent :: !Int
  , crossingCost :: !Int
  }
  deriving stock (Eq, Ord, Show)

productionStructuralReachabilityPolicy :: StructuralReachabilityPolicy
productionStructuralReachabilityPolicy = StructuralReachabilityPolicy
  [packTile 3221 3218 0]
  (Set.singleton "SEASONAL_TRANSPORTS")

buildWorldTopology :: World -> IO WorldTopology
buildWorldTopology world = do
  result <- buildWorldTopologyWithPolicy productionStructuralReachabilityPolicy world
  either (fail . renderReachabilityError world) pure result

buildWorldTopologyWithPolicy :: StructuralReachabilityPolicy -> World -> IO (Either ReachabilityError WorldTopology)
buildWorldTopologyWithPolicy policy world = do
  components <- naturalComponents world
  pure (worldTopologyFromComponents policy world components)

worldTopologyFromComponents :: StructuralReachabilityPolicy -> World -> NaturalComponents -> Either ReachabilityError WorldTopology
worldTopologyFromComponents policy _ _ | null (structuralReachabilitySeeds policy) = Left NoStructuralReachabilitySeeds
worldTopologyFromComponents policy world components = do
  artifact <- maybe (Left MissingSeparatorArtifact) Right (worldSeparatorArtifact world)
  cuts <- validateSeparatorArtifact world components artifact
  let routing = routingComponents world cuts
  crossings <- traverse (makeCrossing routing) (Set.toAscList cuts)
  seeds <- traverse (seedComponents routing crossings) (structuralReachabilitySeeds policy)
  let topology = WorldTopology world components routing crossings (StructuralReachability IntSet.empty)
      edges = structuralEdges topology
      globals = globalDestinations topology
      reachable = close edges globals (IntSet.unions seeds) (IntSet.toList (IntSet.unions seeds))
  pure topology {topologyStructuralReachability = StructuralReachability reachable}
 where
  seedComponents routing crossings seed =
    case pointAttachments (WorldTopology world components routing crossings (StructuralReachability IntSet.empty)) seed of
      [] -> Left (MissingStructuralReachabilitySeed seed)
      attached -> Right (IntSet.fromList attached)
  close _ _ seen [] = seen
  close edges globals seen (component:rest) =
    let next = Map.findWithDefault [] component edges <> globals
        fresh = filter (`IntSet.notMember` seen) next
     in close edges globals (foldr IntSet.insert seen fresh) (fresh <> rest)
  allowed transport = Set.notMember (transportType transport) (structuralIgnoredTransportTypes policy)
  structuralEdges topology = Map.fromListWith (<>) (transportEdges <> attachmentEdges)
   where
    transports = concat (Map.elems (worldTransports world)) <> worldGlobalTeleports world
    transportEdges =
      [ (a, [b])
      | transport <- concat (Map.elems (worldTransports world))
      , allowed transport
      , Just originTile <- [origin transport]
      , Just destinationTile <- [destination transport]
      , a <- pointAttachments topology originTile
      , b <- pointAttachments topology destinationTile
      ]
    attachmentEdges =
      [ (a, [b])
      | point <- Set.toList (Set.fromList [tile | transport <- transports, tile <- maybeToList (origin transport) <> maybeToList (destination transport)])
      , a <- pointAttachments topology point
      , b <- pointAttachments topology point
      , a /= b
      ]
  globalDestinations topology = IntSet.toList (IntSet.fromList
    [ component
    | transport <- worldGlobalTeleports world
    , allowed transport
    , Just destinationTile <- [destination transport]
    , component <- pointAttachments topology destinationTile
    ])
  maybeToList Nothing = []
  maybeToList (Just value) = [value]

  makeCrossing routingComponents' cut@(SeparatorCut a b) =
    case (componentOfTile routingComponents' a, componentOfTile routingComponents' b) of
      (Just ca, Just cb) | ca /= cb -> Right (RoutingCrossing a b ca cb 1)
      _ -> Left (InvalidSeparatorCut cut "cut does not separate two routing components")

validateSeparatorArtifact :: World -> NaturalComponents -> SeparatorArtifact -> Either ReachabilityError (Set.Set SeparatorCut)
validateSeparatorArtifact world natural artifact
  | separatorFormat artifact /= separatorArtifactVersion = Left (SeparatorFormatMismatch (separatorFormat artifact))
  | separatorTopologyIdentity artifact /= actual = Left (SeparatorTopologyMismatch (separatorTopologyIdentity artifact) actual)
  | Set.size cuts /= length (separatorCuts artifact) = Left DuplicateSeparatorCuts
  | otherwise = traverse_ validate cuts >> Right cuts
 where
  actual = walkingTopologyIdentity world
  cuts = Set.fromList (separatorCuts artifact)
  validate cut@(SeparatorCut a b)
    | a == b = Left (InvalidSeparatorCut cut "self edge")
    | b `notElem` walkingNeighbors world a = Left (InvalidSeparatorCut cut "not an authoritative walking edge")
    | componentOfTile natural a /= componentOfTile natural b = Left (InvalidSeparatorCut cut "crosses natural components")
    | componentOfTile natural a == Nothing = Left (InvalidSeparatorCut cut "endpoint is not a natural walking tile")
    | otherwise = Right ()
  traverse_ f = foldr ((>>) . f) (Right ()) . Set.toList

renderReachabilityError :: World -> ReachabilityError -> String
renderReachabilityError world (SeparatorTopologyMismatch stored actual) = unlines
  [ "Routing separator artifact does not match the current walking topology."
  , ""
  , "Artifact:"
  , "  " <> maybe "<unknown>" id (worldSeparatorArtifactPath world)
  , ""
  , "Artifact topology identity:"
  , "  " <> stored
  , ""
  , "Current topology identity:"
  , "  " <> actual
  , ""
  , "The collision data or walking topology has changed."
  , "Refusing to construct routing topology with stale separators."
  , ""
  , "Regenerate with:"
  , ""
  , "  cabal run separator-artifact -- generate data/routing-separators-v1.json 20000 500 32 40 42"
  ]
renderReachabilityError _ err = show err

walkingTopologyIdentity :: World -> String
walkingTopologyIdentity world = "fnv1a64:" <> pad (showHex digest "")
 where
  walkable = IntSet.fromList (map unTile (collisionTiles (worldCollision world)))
  rows = [(tile, sort [unTile next | next <- walkingNeighbors world (Tile tile), IntSet.member (unTile next) walkable]) | tile <- IntSet.toAscList walkable]
  digest = foldl' hashInt offset (concatMap (\(tile, neighbours) -> tile : (-1) : neighbours <> [-2]) rows)
  offset = 14695981039346656037 :: Word64
  prime = 1099511628211 :: Word64
  hashInt hash value = foldl' (\h byte -> (h `xor` byte) * prime) hash
    [fromIntegral ((fromIntegral value :: Word64) `shiftR` shift) | shift <- [0, 8 .. 56]]
  pad value = replicate (16 - length value) '0' <> value

withEmptySeparatorArtifact :: World -> World
withEmptySeparatorArtifact world = world {worldSeparatorArtifact = Just (SeparatorArtifact
  separatorArtifactVersion (walkingTopologyIdentity world) (SeparatorConfig 0 0 0 0 "none" 0) [])}

routingComponents :: World -> Set.Set SeparatorCut -> NaturalComponents
routingComponents world cuts = componentsAvoiding world blocked
 where
  blocked a b = Set.member (canonicalCut a b) cuts

componentsAvoiding :: World -> (Tile -> Tile -> Bool) -> NaturalComponents
componentsAvoiding world blocked = runST $ do
  let walkable = IntSet.fromList (map unTile (collisionTiles (worldCollision world)))
  queue <- Mutable.new (max 1 (IntSet.size walkable))
  owner <- go queue 1 walkable IntMap.empty
  let pairs = IntMap.toAscList owner
      ids = IntSet.toAscList (IntSet.fromList (map snd pairs))
  pure NaturalComponents
    { componentOwnerTiles = Vector.fromList (map fst pairs)
    , componentOwnerIds = Vector.fromList (map snd pairs)
    , componentIds = Vector.fromList ids
    , maxComponentId = maximum (0 : ids)
    }
 where
  go :: Mutable.MVector s Int -> Int -> IntSet.IntSet -> IntMap.IntMap Int -> ST s (IntMap.IntMap Int)
  go queue cid remaining owner = case IntSet.minView remaining of
    Nothing -> pure owner
    Just (start, rest) -> do
      Mutable.write queue 0 start
      (owner', rest') <- flood queue cid rest (IntMap.insert start cid owner) 0 1
      go queue (cid + 1) rest' owner'
  flood :: Mutable.MVector s Int -> Int -> IntSet.IntSet -> IntMap.IntMap Int -> Int -> Int -> ST s (IntMap.IntMap Int, IntSet.IntSet)
  flood queue cid remaining owner readIx writeIx
    | readIx == writeIx = pure (owner, remaining)
    | otherwise = do
        packed <- Mutable.read queue readIx
        let from = Tile packed
            next = [unTile tile | tile <- walkingNeighbors world from, IntSet.member (unTile tile) remaining, not (blocked from tile)]
            remaining' = foldr IntSet.delete remaining next
            owner' = foldr (`IntMap.insert` cid) owner next
        forM_ (zip [writeIx ..] next) (uncurry (Mutable.write queue))
        flood queue cid remaining' owner' (readIx + 1) (writeIx + length next)

componentOfTile :: NaturalComponents -> Tile -> Maybe Int
componentOfTile components tile =
  (componentOwnerIds components Vector.!?) =<< binarySearch (unTile tile) (componentOwnerTiles components)

pointAttachments :: WorldTopology -> Tile -> [Int]
pointAttachments topology = map snd . pointAttachmentDetails topology

pointAttachmentDetails :: WorldTopology -> Tile -> [(Tile, Int)]
pointAttachmentDetails topology point =
  case componentOfTile components point of
    Just cid -> [(point, cid)]
    Nothing -> IntMap.elems (IntMap.fromList
      [ (cid, (tile, cid))
      | tile <- walkingNeighbors world point
      , Just cid <- [componentOfTile components tile]
      ])
 where
  world = topologyWorld topology
  components = topologyNaturalComponents topology

structurallyReachablePointAttachments :: WorldTopology -> Tile -> [Int]
{-# INLINE structurallyReachablePointAttachments #-}
structurallyReachablePointAttachments topology =
  filter (componentIsStructurallyReachable topology) . pointAttachments topology

routingPointAttachments :: WorldTopology -> Tile -> [Int]
routingPointAttachments topology point = IntMap.keys (IntMap.fromList
  [ (routingId, ())
  | (resolved, naturalId) <- pointAttachmentDetails topology point
  , componentIsStructurallyReachable topology naturalId
  , Just routingId <- [routingComponentOfTile topology resolved]
  ])

componentIsStructurallyReachable :: WorldTopology -> Int -> Bool
{-# INLINE componentIsStructurallyReachable #-}
componentIsStructurallyReachable topology cid =
  IntSet.member cid (structurallyReachableIds (topologyStructuralReachability topology))

reachableComponentTiles :: WorldTopology -> [(Int, Int)]
reachableComponentTiles topology =
  [ (packed, routingId)
  | (packed, routingId) <- zip (Vector.toList (componentOwnerTiles routing)) (Vector.toList (componentOwnerIds routing))
  , Just naturalId <- [componentOfTile natural (Tile packed)]
  , componentIsStructurallyReachable topology naturalId
  ]
 where
  natural = topologyNaturalComponents topology
  routing = topologyRoutingComponents topology

componentFacts :: WorldTopology -> [(Int, Int, Bool, Int, Int, Int, Int, Int, Int)]
componentFacts topology =
  [ (cid, count, componentIsStructurallyReachable topology cid, loX, hiX, loY, hiY, loP, hiP)
  | (cid, (count, loX, hiX, loY, hiY, loP, hiP)) <- IntMap.toAscList stats
  ]
 where
  components = topologyNaturalComponents topology
  stats = foldl add IntMap.empty (zip (Vector.toList (componentOwnerTiles components)) (Vector.toList (componentOwnerIds components)))
  add acc (packed, cid) = IntMap.insertWith combine cid (1, x, x, y, y, p, p) acc
   where (x, y, p) = unpackTile (Tile packed)
  combine (count, loX, hiX, loY, hiY, loP, hiP) (count', loX', hiX', loY', hiY', loP', hiP') =
    (count + count', min loX loX', max hiX hiX', min loY loY', max hiY hiY', min loP loP', max hiP hiP')

tileFacts :: WorldTopology -> [(Tile, Int)]
tileFacts topology =
  [(Tile tile, cid) | (tile, cid) <- zip (Vector.toList (componentOwnerTiles components)) (Vector.toList (componentOwnerIds components))]
 where components = topologyNaturalComponents topology

routingComponentFacts :: WorldTopology -> [(Int, Int, Int, Int, Int)]
routingComponentFacts topology =
  [ (routingId, naturalId, tileCount, crossingCount, IntSet.size neighbours)
  | (routingId, (naturalId, tileCount)) <- IntMap.toAscList sizes
  , let incident = filter (\edge -> crossingFromComponent edge == routingId || crossingToComponent edge == routingId) crossings
        crossingCount = length incident
        neighbours = IntSet.fromList [if crossingFromComponent edge == routingId then crossingToComponent edge else crossingFromComponent edge | edge <- incident]
  ]
 where
  natural = topologyNaturalComponents topology
  routing = topologyRoutingComponents topology
  crossings = topologySeparatorCrossings topology
  sizes = foldl' add IntMap.empty (zip (Vector.toList (componentOwnerTiles routing)) (Vector.toList (componentOwnerIds routing)))
  add acc (packed, routingId) = case componentOfTile natural (Tile packed) of
    Just naturalId -> IntMap.insertWith (\(_, n) (original, total) -> (original, n + total)) routingId (naturalId, 1) acc
    Nothing -> acc

routingTileFacts :: WorldTopology -> [(Tile, Int, Int)]
routingTileFacts topology =
  [ (Tile packed, naturalId, routingId)
  | (packed, routingId) <- zip (Vector.toList (componentOwnerTiles routing)) (Vector.toList (componentOwnerIds routing))
  , Just naturalId <- [componentOfTile natural (Tile packed)]
  ]
 where
  natural = topologyNaturalComponents topology
  routing = topologyRoutingComponents topology

separatorCrossingFacts :: WorldTopology -> [RoutingCrossing]
separatorCrossingFacts = topologySeparatorCrossings

naturalComponents :: World -> IO NaturalComponents
naturalComponents world = pure (componentsAvoiding world (\_ _ -> False))

binarySearch :: Int -> Vector.Vector Int -> Maybe Int
binarySearch needle values = go 0 (Vector.length values - 1)
 where
  go lo hi
    | lo > hi = Nothing
    | value == needle = Just mid
    | value < needle = go (mid + 1) hi
    | otherwise = go lo (mid - 1)
   where
    mid = (lo + hi) `div` 2
    value = values Vector.! mid
routingComponentOfTile :: WorldTopology -> Tile -> Maybe Int
routingComponentOfTile topology = componentOfTile (topologyRoutingComponents topology)
