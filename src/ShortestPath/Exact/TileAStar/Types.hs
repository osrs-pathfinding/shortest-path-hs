module ShortestPath.Exact.TileAStar.Types
  ( TileAStar(..)
  , TileStatic(..)
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
  , staticTiles :: Vector.Vector Int
  , staticComponents :: Boxed.Vector (Vector.Vector Int)
  , staticWalkingNetwork :: SparseWalkingNetwork
  }

instance Binary TileStatic where
  put value = do
    put (Vector.toList (staticSearchTiles value))
    put (Vector.toList (staticTiles value))
    put (map Vector.toList (Boxed.toList (staticComponents value)))
    put (staticWalkingNetwork value)
  get =
    TileStatic
      <$> (Vector.fromList <$> get)
      <*> (Vector.fromList <$> get)
      <*> (Boxed.fromList . map Vector.fromList <$> get)
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
  , reverseSameComponentSiteScans :: !Int
  , reverseTotalSitesScanned :: !Int
  , reverseChebyshevComparisons :: !Int
  , reverseMaxSitesScannedPerPop :: !Int
  , reverseTransportRelaxations :: !Int
  }
  deriving stock (Eq, Show)

data TileAStarTimings = TileAStarTimings
  { tileHeuristicSetupMilliseconds :: !Double
  , tileReverseDijkstraMilliseconds :: !Double
  , tileSeedTableMilliseconds :: !Double
  , tileSearchMilliseconds :: !Double
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
