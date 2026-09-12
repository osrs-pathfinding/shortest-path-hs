module ShortestPath.Exact.TileAStar.Types
  ( TileAStar(..)
  , TileStatic(..)
  , SiteGraph(..)
  , TileAStarCounters(..)
  , TileBankGlobalObservation(..)
  , TileReverseCounters(..)
  , TileAStarTimings(..)
  , ReversePathDebug(..)
  , ReversePathState(..)
  , ReversePathEdge(..)
  , HeuristicRender(..)
  , HeuristicLayer(..)
  , HeuristicTile(..)
  , tileStaticStats
  , tileTopology
  , tileWorld
  ) where

import Data.Binary (Binary(..))
import Data.Int (Int32, Int64)
import Data.Word (Word8)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Vector

import ShortestPath.Exact.TileAStar.SparseWalking
import ShortestPath.Tile
import ShortestPath.Topology
import ShortestPath.World

data TileAStar = TileAStar WorldTopology TileStatic

tileWorld :: TileAStar -> World
tileWorld = topologyWorld . tileTopology

tileTopology :: TileAStar -> WorldTopology
tileTopology (TileAStar topology _) = topology

data TileStatic = TileStatic
  { staticSearchTiles :: Vector.Vector Int
  , staticSearchComponents :: Vector.Vector Int
  , staticWalkingMasks :: Vector.Vector Word8
  , staticNorthNodes :: Vector.Vector Int32
  , staticSouthNodes :: Vector.Vector Int32
  , staticTiles :: Vector.Vector Int
  , staticComponents :: Boxed.Vector (Vector.Vector Int)
  , staticSiteTileIndex :: IntMap.IntMap Int
  , staticSiteComponentIds :: Boxed.Vector (Vector.Vector Int)
  , staticReachableBanks :: Set.Set Tile
  , staticWalkingNetwork :: SparseWalkingNetwork
  }

data SiteGraph = SiteGraph
  { siteTiles :: Vector.Vector Int
  , siteTileIndex :: IntMap.IntMap Int
  , siteComponents :: Boxed.Vector (Vector.Vector Int)
  , siteStaticCount :: !Int
  , siteSparseNetwork :: SparseWalkingNetwork
  , siteComponentSiteIds :: Boxed.Vector (Vector.Vector Int)
  , siteReverseEdges :: Boxed.Vector (Vector.Vector (Int, Int))
  }

instance Binary TileStatic where
  put value = do
    put (Vector.toList (staticSearchTiles value))
    put (Vector.toList (staticSearchComponents value))
    put (Vector.toList (staticWalkingMasks value))
    put (Vector.toList (staticNorthNodes value))
    put (Vector.toList (staticSouthNodes value))
    put (Vector.toList (staticTiles value))
    put (map Vector.toList (Boxed.toList (staticComponents value)))
    put (IntMap.toList (staticSiteTileIndex value))
    put (map Vector.toList (Boxed.toList (staticSiteComponentIds value)))
    put (Set.toList (staticReachableBanks value))
    put (staticWalkingNetwork value)
  get =
    TileStatic
      <$> (Vector.fromList <$> get)
      <*> (Vector.fromList <$> get)
      <*> (Vector.fromList <$> get)
      <*> (Vector.fromList <$> get)
      <*> (Vector.fromList <$> get)
      <*> (Vector.fromList <$> get)
      <*> (Boxed.fromList . map Vector.fromList <$> get)
      <*> (IntMap.fromList <$> get)
      <*> (Boxed.fromList . map Vector.fromList <$> get)
      <*> (Set.fromList <$> get)
      <*> get

tileStaticStats :: TileAStar -> (Int, Int, Int, Int)
tileStaticStats (TileAStar _ static) =
  ( Vector.length (staticTiles static)
  , sparseSteinerCount network
  , sparseVertexCount network
  , sparseWalkingEdgeCount network
  )
 where
  network = staticWalkingNetwork static

data TileAStarCounters = TileAStarCounters
  { tileStatesPopped :: !Int
  , tileStalePqEntries :: !Int
  , tilePqPushes :: !Int
  , tileUniqueStatesReached :: !Int
  , tileWalkingRelaxations :: !Int
  , tileTransportRelaxations :: !Int
  , tileHeuristicEvaluations :: !Int
  , tileHeuristicCalls :: !Int
  , tileHeuristicCandidatesScanned :: !Int
  , tileHeuristicMaxCandidatesPerCall :: !Int
  , tileHeuristicUnreachable :: !Int
  , tileUnknownComponentPrunes :: !Int
  , tileNoReverseSeedPrunes :: !Int
  , tileBestBankCostUpdates :: !Int
  , tileFinalBestBankCost :: !Int
  , tileBankDominatedHeuristicEvaluations :: !Int
  , tileBankGlobalTransitionsSuppressed :: !Int
  , tileBankBoundPQRekeys :: !Int
  , tileBankGlobalTrace :: [TileBankGlobalObservation]
  }
  deriving stock (Eq, Show)

data TileBankGlobalObservation = TileBankGlobalObservation
  { bankGlobalTile :: !Tile
  , bankGlobalStateBanked :: !Bool
  , bankGlobalCost :: !Int
  , bankGlobalBestBankCost :: !Int
  , bankGlobalSuppressed :: !Bool
  , bankGlobalEdges :: [(Tile, Int, String)]
  }
  deriving stock (Eq, Show)

data TileReverseCounters = TileReverseCounters
  { reverseStatesPopped :: !Int
  , reverseStalePqEntries :: !Int
  , reversePqPushes :: !Int
  , reversePqPops :: !Int
  , reversePqMaxSize :: !Int
  , reverseEdgesRelaxed :: !Int
  , reverseSameComponentSiteScans :: !Int
  , reverseTotalSitesScanned :: !Int
  , reverseChebyshevComparisons :: !Int
  , reverseMaxSitesScannedPerPop :: !Int
  , reverseTransportRelaxations :: !Int
  }
  deriving stock (Eq, Show)

data TileAStarTimings = TileAStarTimings
  { tileAccountPrepareMilliseconds :: !Double
  , tileTargetPrepareMilliseconds :: !Double
  , tileHeuristicSetupMilliseconds :: !Double
  , tileReverseDijkstraMilliseconds :: !Double
  , tileSeedTableMilliseconds :: !Double
  , tileHeuristicSeedCount :: !Int
  , tileHeuristicComponentCount :: !Int
  , tileHeuristicMaxSeedsPerComponent :: !Int
  , tileHeuristicSeedsPerComponentP50 :: !Int
  , tileHeuristicSeedsPerComponentP90 :: !Int
  , tileHeuristicSeedsPerComponentP95 :: !Int
  , tileHeuristicSeedsPerComponentP99 :: !Int
  , tileHeuristicGeneratorCount :: !Int
  , tileHeuristicMaxGeneratorsPerComponent :: !Int
  , tileHeuristicGeneratorsPerComponentP50 :: !Int
  , tileHeuristicGeneratorsPerComponentP90 :: !Int
  , tileHeuristicGeneratorsPerComponentP95 :: !Int
  , tileHeuristicGeneratorsPerComponentP99 :: !Int
  , tileHeuristicGeneratorSeedRatioP50 :: !Double
  , tileHeuristicGeneratorSeedRatioP90 :: !Double
  , tileHeuristicGeneratorSeedRatioP95 :: !Double
  , tileHeuristicGeneratorSeedRatioP99 :: !Double
  , tileHeuristicGeneratorSeedRatioMax :: !Double
  , tileHeuristicGeneratorSeedRatio :: !Double
  , tileSearchMilliseconds :: !Double
  , tileForwardAllocatedBytes :: !Int64
  , tileTotalMilliseconds :: !Double
  , tileSearchCounters :: !TileAStarCounters
  , tileReverseCounters :: !TileReverseCounters
  }
  deriving stock (Eq, Show)

data ReversePathDebug = ReversePathDebug
  { reverseDebugSeed :: !Tile
  , reverseDebugTarget :: !Tile
  , reverseDebugStates :: [ReversePathState]
  }
  deriving stock (Eq, Show)

data ReversePathState = ReversePathState
  { reverseStateBanked :: !Bool
  , reverseStateTile :: !(Maybe Tile)
  , reverseStateDistance :: !Int
  , reverseStateHeuristic :: !Int
  , reverseStateUnreachable :: !Bool
  , reverseStatePath :: [ReversePathEdge]
  }
  deriving stock (Eq, Show)

data ReversePathEdge = ReversePathEdge
  { reverseEdgeFrom :: !Tile
  , reverseEdgeTo :: !Tile
  , reverseEdgeFromBanked :: !Bool
  , reverseEdgeToBanked :: !Bool
  , reverseEdgeType :: String
  , reverseEdgeLabel :: String
  , reverseEdgeCost :: !Int
  , reverseEdgeCumulativeCost :: !Int
  }
  deriving stock (Eq, Show)

data HeuristicRender = HeuristicRender
  { renderTileSize :: !Int
  , renderLayers :: [HeuristicLayer]
  }
  deriving stock (Eq, Show)

data HeuristicLayer = HeuristicLayer
  { layerKey :: String
  , layerLabel :: String
  , layerBankPathEnabled :: !Bool
  , layerMinimum :: !Int
  , layerMaximum :: !Int
  , layerHeuristicMilliseconds :: !Double
  , layerTransformMilliseconds :: !Double
  , layerWriteMilliseconds :: !Double
  , layerSeeds :: [(Tile, Int)]
  , layerTiles :: [HeuristicTile]
  }
  deriving stock (Eq, Show)

data HeuristicTile = HeuristicTile
  { tileUrl :: String
  , tilePlane :: !Int
  , tileX :: !Int
  , tileY :: !Int
  , tileMinimum :: !Int
  , tileMaximum :: !Int
  }
  deriving stock (Eq, Show)
