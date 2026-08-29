module Main (main) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import ShortestPath.Requirements
import ShortestPath.Hierarchy.Partition
import ShortestPath.Hierarchy.Preprocess
import ShortestPath.Hierarchy.Types
import ShortestPath.Tile
import ShortestPath.Transport
import ShortestPath.World

main :: IO ()
main = do
  assert (unpackTile (packTile 3200 3201 2) == (3200, 3201, 2))
  assert (parseSkills "75 Construction;83 Farming" == [SkillReq 75 "Construction", SkillReq 83 "Farming"])
  assert (parseVars Varbit "4070=0;4560&2" == [VarReq Varbit 4070 0 VarEq, VarReq Varbit 4560 2 VarMask])
  assert (parseTileField "3221 3218 0" == Just (packTile 3221 3218 0))
  assert (length virtualWalls == 3)
  assert (isVirtualWallTile (packTile 2836 3451 0))
  assert (syntheticPartitionCheck == Right ())
  syntheticPreprocessCheck

syntheticPreprocessCheck :: IO ()
syntheticPreprocessCheck = do
  let a = packTile 0 0 0
      b = packTile 1 0 0
      separator = packTile 2 0 0
      d = packTile 3 0 0
      leafA = LeafId 1 "a"
      leafB = LeafId 1 "b"
      classes = IntMap.fromList
        [ (unTile a, LeafTile leafA)
        , (unTile b, LeafTile leafA)
        , (unTile separator, SeparatorTile "s" 0)
        , (unTile d, LeafTile leafB)
        ]
      partition = Partition
        classes
        (Map.fromList [(leafA, IntSet.fromList (map unTile [a, b])), (leafB, IntSet.singleton (unTile d))])
        (IntSet.singleton (unTile separator))
      neighbours tile = Map.findWithDefault [] tile (Map.fromList [(a, [b]), (b, [a, separator]), (separator, [b, d]), (d, [separator])])
      roles = TerminalRoles (Set.singleton a) (Set.singleton b) (Set.singleton d) Set.empty
  hierarchy <- preprocessHierarchyWith partition neighbours roles
  let overlayA = Map.lookup leafA (leafOverlays hierarchy)
      bKinds = overlayA >>= Map.lookup b . leafTerminals
  assert ((overlayA >>= \overlay -> leafDistance overlay a b) == Just 1)
  assert (bKinds == Just (Set.fromList [LocalTransportOrigin, RegionGateway]))
  assert (Map.member separator (hierarchySeparatorNodes hierarchy))

assert :: Bool -> IO ()
assert True = pure ()
assert False = fail "assertion failed"
