module ShortestPath.Topology
  ( NaturalComponents(..)
  , StructuralReachabilityPolicy(..)
  , ReachabilityError(..)
  , StructuralReachability(..)
  , WorldTopology(..)
  , productionStructuralReachabilityPolicy
  , buildWorldTopology
  , buildWorldTopologyWithPolicy
  , worldTopologyFromComponents
  , componentOfTile
  , pointAttachments
  , pointAttachmentDetails
  , structurallyReachablePointAttachments
  , componentIsStructurallyReachable
  , reachableComponentTiles
  , componentFacts
  , tileFacts
  ) where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Binary (Binary(..))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as Mutable

import ShortestPath.Tile
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
  deriving stock (Eq, Show)

newtype StructuralReachability = StructuralReachability
  { structurallyReachableIds :: IntSet.IntSet
  }
  deriving stock (Eq, Show)

data WorldTopology = WorldTopology
  { topologyWorld :: World
  , topologyNaturalComponents :: NaturalComponents
  , topologyStructuralReachability :: StructuralReachability
  }

productionStructuralReachabilityPolicy :: StructuralReachabilityPolicy
productionStructuralReachabilityPolicy = StructuralReachabilityPolicy
  [packTile 3221 3218 0]
  (Set.fromList ["VIRTUAL_WALL", "SEASONAL_TRANSPORTS"])

buildWorldTopology :: World -> IO WorldTopology
buildWorldTopology world = do
  result <- buildWorldTopologyWithPolicy productionStructuralReachabilityPolicy world
  either (fail . show) pure result

buildWorldTopologyWithPolicy :: StructuralReachabilityPolicy -> World -> IO (Either ReachabilityError WorldTopology)
buildWorldTopologyWithPolicy policy world = do
  components <- naturalComponents world
  pure (worldTopologyFromComponents policy world components)

worldTopologyFromComponents :: StructuralReachabilityPolicy -> World -> NaturalComponents -> Either ReachabilityError WorldTopology
worldTopologyFromComponents policy _ _ | null (structuralReachabilitySeeds policy) = Left NoStructuralReachabilitySeeds
worldTopologyFromComponents policy world components = do
  seeds <- traverse seedComponents (structuralReachabilitySeeds policy)
  let topology = WorldTopology world components (StructuralReachability IntSet.empty)
      edges = structuralEdges topology
      globals = globalDestinations topology
      reachable = close edges globals (IntSet.unions seeds) (IntSet.toList (IntSet.unions seeds))
  pure topology {topologyStructuralReachability = StructuralReachability reachable}
 where
  seedComponents seed =
    case pointAttachments (WorldTopology world components (StructuralReachability IntSet.empty)) seed of
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
      | tile <- walkingNeighborsRaw world point
      , Just cid <- [componentOfTile components tile]
      ])
 where
  world = topologyWorld topology
  components = topologyNaturalComponents topology

structurallyReachablePointAttachments :: WorldTopology -> Tile -> [Int]
structurallyReachablePointAttachments topology =
  filter (componentIsStructurallyReachable topology) . pointAttachments topology

componentIsStructurallyReachable :: WorldTopology -> Int -> Bool
componentIsStructurallyReachable topology cid =
  IntSet.member cid (structurallyReachableIds (topologyStructuralReachability topology))

reachableComponentTiles :: WorldTopology -> [(Int, Int)]
reachableComponentTiles topology =
  [ pair
  | pair@(_, cid) <- zip (Vector.toList (componentOwnerTiles components)) (Vector.toList (componentOwnerIds components))
  , componentIsStructurallyReachable topology cid
  ]
 where
  components = topologyNaturalComponents topology

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

naturalComponents :: World -> IO NaturalComponents
naturalComponents world = do
  let walkable = IntSet.fromList (map unTile (collisionTiles (worldCollision world)))
  owner <- pure $ runST $ do
    queue <- Mutable.new (max 1 (IntSet.size walkable))
    go queue 1 walkable IntMap.empty
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
  go queue cid remaining owner =
    case IntSet.minView remaining of
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
        let next =
              [ unTile tile
              | tile <- walkingNeighborsRaw world (Tile packed)
              , IntSet.member (unTile tile) remaining
              ]
            remaining' = foldr IntSet.delete remaining next
            owner' = foldr (`IntMap.insert` cid) owner next
        forM_ (zip [writeIx ..] next) (uncurry (Mutable.write queue))
        flood queue cid remaining' owner' (readIx + 1) (writeIx + length next)

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
