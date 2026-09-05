module Main (main) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import ShortestPath.Requirements
import ShortestPath.Account
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
  requirementChecks
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

requirementChecks :: IO ()
requirementChecks = do
  let account = emptyAccountBuild
        { accountLevels = Map.singleton "Agility" 70
        , accountCompletedQuests = Set.singleton "Quest"
        , accountVarbits = Map.singleton 1 6
        , accountVarPlayers = Map.singleton 2 100
        , accountInventory = Map.singleton "1" 2
        , accountEquipment = Map.singleton "2" 1
        , accountRunePouch = Map.singleton "3" 1
        , accountBank = Map.singleton "4" 1
        }
      carried = RequirementContext account CarriedOnly 130
      banked = RequirementContext account CarriedAndBank 130
      transport = Transport "TEST" Nothing Nothing 0 "" "" False Nothing [SkillReq 70 "Agility"] (Just (ItemAnd [ItemOne (ItemTerm "1" 2), ItemOr [ItemOne (ItemTerm "2" 1), ItemOne (ItemTerm "4" 1)]])) ["Quest"] [VarReq Varbit 1 6 VarEq, VarReq Varbit 1 2 VarMask] [VarReq VarPlayer 2 20 VarCooldownMinutes] "test"
  assert (requirementsSatisfied carried transport)
  assert (not (requirementsSatisfied carried (transport { skills = [SkillReq 71 "Agility"] })))
  assert (not (requirementsSatisfied carried (transport { quests = ["Missing"] })))
  assert (not (requirementsSatisfied carried (transport { varbits = [VarReq Varbit 9 0 VarEq] })))
  assert (not (requirementsSatisfied carried (transport { items = Just (ItemOne (ItemTerm "4" 1)) })))
  assert (requirementsSatisfied banked (transport { items = Just (ItemOne (ItemTerm "4" 1)) }))
