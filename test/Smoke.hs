module Main (main) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (find)

import ShortestPath.Requirements
import ShortestPath.Account
import qualified ShortestPath.GameVars.Varbits as VB
import qualified ShortestPath.GameVars.VarPlayers as VP
import ShortestPath.BenchmarkProfiles
import ShortestPath.Hierarchy.Partition
import ShortestPath.Hierarchy.Preprocess
import ShortestPath.Hierarchy.Types
import ShortestPath.Tile
import ShortestPath.Transport
import ShortestPath.World
import ShortestPath.Pathfinder
import ShortestPath.Exact.RawDijkstra

main :: IO ()
main = do
  assert (unpackTile (packTile 3200 3201 2) == (3200, 3201, 2))
  assert (parseSkills "75 Construction;83 Farming" == [SkillReq 75 "Construction", SkillReq 83 "Farming"])
  assert (parseVars Varbit "4070=0;4560&2" == [VarReq (GameVarbit (VarbitId 4070)) 0 VarEq, VarReq (GameVarbit (VarbitId 4560)) 2 VarMask])
  requirementChecks
  profileChecks
  semanticProfileChecks
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
        , accountVarbits = Map.singleton (VarbitId 1) 6
        , accountVarPlayers = Map.singleton (VarPlayerId 2) 100
        , accountInventory = Map.singleton "1" 2
        , accountEquipment = Map.singleton "2" 1
        , accountRunePouch = Map.singleton "3" 1
        , accountBank = Map.singleton "4" 1
        }
      carried = RequirementContext account CarriedOnly 130
      banked = RequirementContext account CarriedAndBank 130
      transport = Transport "TEST" Nothing Nothing 0 "" "" False Nothing [SkillReq 70 "Agility"] (Just (ItemAnd [ItemOne (ItemTerm "1" 2), ItemOr [ItemOne (ItemTerm "2" 1), ItemOne (ItemTerm "4" 1)]])) ["Quest"] [VarReq (GameVarbit (VarbitId 1)) 6 VarEq, VarReq (GameVarbit (VarbitId 1)) 2 VarMask] [VarReq (GameVarPlayer (VarPlayerId 2)) 20 VarCooldownMinutes] "test"
  assert (requirementsSatisfied carried transport)
  assert (not (requirementsSatisfied (carried { requirementAccount = account { accountVarPlayers = Map.singleton (VarPlayerId 2) 110 } }) transport))
  assert (requirementsSatisfied (carried { requirementAccount = account { accountVarPlayers = Map.singleton (VarPlayerId 2) 109 } }) transport)
  assert (not (requirementsSatisfied carried (transport { skills = [SkillReq 71 "Agility"] })))
  assert (not (requirementsSatisfied carried (transport { quests = ["Missing"] })))
  assert (not (requirementsSatisfied carried (transport { varbits = [VarReq (GameVarbit (VarbitId 9)) 0 VarEq] })))
  assert (not (requirementsSatisfied carried (transport { items = Just (ItemOne (ItemTerm "4" 1)) })))
  assert (requirementsSatisfied banked (transport { items = Just (ItemOne (ItemTerm "4" 1)) }))

profileChecks :: IO ()
profileChecks = do
  let fairyRing = Transport "FAIRY_RING" Nothing Nothing 0 "" "" False Nothing [] Nothing [] [] [] "test"
      basicBox = Transport "TELEPORTATION_BOX" Nothing Nothing 0 "Basic Jewellery Box" "" False Nothing [] Nothing [] [] [] "test"
      varrockPortal = Transport "TELEPORTATION_PORTAL_POH" Nothing Nothing 0 "Varrock Portal" "" False Nothing [] Nothing [] [] [] "test"
      profile name = mustProfile name (benchmarkAccount name [])
      early = profile "early"
      mid = profile "mid"
      end = profile "end"
      maxed = profile "maxed"
      dragonDoor = Transport "TRANSPORT" Nothing Nothing 0 "Open Wall" "" False Nothing [] Nothing ["Dragon Slayer I"] [VarReq (GameVarbit VB.dragonslayerCrandorFoundSecretDoor) 1 VarEq] [] "test"
      maxedWithDragon = mustProfile "maxed" (benchmarkAccount "maxed" [dragonDoor])
      unknownDoor = dragonDoor { quests = [], varbits = [VarReq (GameVarbit VB.darkmShortcutInner) 1 VarEq] }
      context account = RequirementContext account CarriedOnly benchmarkNowMinutes
      withoutStaff account = account { accountInventory = Map.delete "772" (accountInventory account) }
  assert (requirementsSatisfied (context early) fairyRing)
  assert (not (requirementsSatisfied (context (withoutStaff early)) fairyRing))
  assert (requirementsSatisfied (context mid) fairyRing)
  assert (not (requirementsSatisfied (context (withoutStaff mid)) fairyRing))
  assert (requirementsSatisfied (context end) fairyRing)
  assert (requirementsSatisfied (context maxed) fairyRing)
  assert (not (requirementsSatisfied (context early) basicBox))
  assert (requirementsSatisfied (context mid) basicBox)
  assert (not (requirementsSatisfied (context early) varrockPortal))
  assert (requirementsSatisfied (context mid) varrockPortal)
  assert (requirementsSatisfied (context end) varrockPortal)
  assert (requirementsSatisfied (context maxedWithDragon) dragonDoor)
  assert (requirementsSatisfied (context maxed) dragonDoor)
  assert (all (== Just 1) [Map.lookup VB.dragonslayerCrandorFoundSecretDoor (accountVarbits account) | account <- [early, mid, end, maxed]])
  assert (all (== Just 0) [Map.lookup VB.spellbook (accountVarbits account) | account <- [early, mid, end, maxed]])
  assert (all (== Just 0) [Map.lookup VB.pohTeleToggle (accountVarbits account) | account <- [early, mid, end, maxed]])
  assert (all (== Just 0) [Map.lookup VB.fremennikBasicTeleport (accountVarbits account) | account <- [early, mid, end, maxed]])
  assert (all (== Just 1) [Map.lookup VB.pohHouseLocation (accountVarbits account) | account <- [early, mid, end, maxed]])
  assert (case transportAvailability (context maxed) unknownDoor of Unavailable failures -> any isUnknown failures; _ -> False)
  assert (Map.member "13393" (accountBank mid))
  assert (Map.member "28327" (accountBank end))
  assert (not (Map.member "28327" (accountBank mid)))
  assert (not (Map.member "13249" (accountBank end)))
  assert (not (Map.member "CAPESLOT" (accountBank mid)))
  assert (Map.keysSet (accountBank end) `Set.isSubsetOf` Map.keysSet (accountBank maxed))
 where
  isUnknown (UnknownVarRequirements _) = True
  isUnknown _ = False

mustProfile :: String -> Maybe AccountBuild -> AccountBuild
mustProfile _ (Just account) = account
mustProfile name Nothing = error ("missing benchmark profile: " <> name)

semanticProfileChecks :: IO ()
semanticProfileChecks = do
  assert (quetzalPlatformBit CamTorum == 32)
  assert (quetzalPlatformBit ColossalWyrmRemains == 64)
  assert (quetzalPlatformBit OuterFortis == 128)
  assert (quetzalPlatformBit FortisColosseum == 256)
  assert (quetzalPlatformBit SalvagerOverlook == 2048)
  assert (quetzalPlatformBit Kastori == 16384)
  assert (quetzalPlatformMask Set.empty == 0)
  assert (quetzalPlatformMask (Set.fromList [minBound .. maxBound]) == 18912)
  let early = mustProfile "early" (benchmarkAccount "early" [])
      cam = early
        { accountVarPlayers = Map.singleton VP.quetzalsUnlocked (quetzalPlatformMask (Set.singleton CamTorum)) }
      whistleIds = map VarbitId [29271, 29273, 29275, 33120]
  assert (Map.lookup VP.quetzalsUnlocked (accountVarPlayers early) == Just 0)
  assert (Map.notMember (VarbitId 4182) (accountVarbits early))
  assert (all (`Map.notMember` accountVarbits early) whistleIds)
  let mid = mustProfile "mid" (benchmarkAccount "mid" [])
      end = mustProfile "end" (benchmarkAccount "end" [])
      maxed = mustProfile "maxed" (benchmarkAccount "maxed" [])
  assert (Map.lookup (VarbitId 4498) (accountVarbits end) == Just 1)
  assert (Map.lookup (VarbitId 4566) (accountVarbits end) == Just 0)
  assert (Map.lookup (VarbitId 4566) (accountVarbits maxed) == Just 1)
  assert (Map.lookup VP.slug2Regionuid (accountVarPlayers early) == Just (benchmarkNowMinutes - 21))
  assert (Set.member "Land of the Goblins" (accountCompletedQuests maxed))
  assert (Set.member "Sins of the Father" (accountCompletedQuests maxed))
  assert (all (\account -> Set.member "Cabin Fever" (accountCompletedQuests account)) [mid, end, maxed])
  assert (not (Set.member "Cabin Fever" (accountCompletedQuests early)))
  assert (all (\account -> Set.member "The Path of Glouphrie" (accountCompletedQuests account)) [mid, end, maxed])
  assert (not (Set.member "The Path of Glouphrie" (accountCompletedQuests early)))
  assert (all (\account -> Set.member "Troubled Tortugans" (accountCompletedQuests account)) [mid, end, maxed])
  assert (not (Set.member "Troubled Tortugans" (accountCompletedQuests early)))
  assert (Map.lookup VB.lotg (accountVarbits early) == Just 0)
  assert (Map.lookup VB.myq5 (accountVarbits early) == Just 0)
  assert (Map.lookup VB.my2armStatus (accountVarbits early) == Just 0)
  assert (Map.lookup VB.hosidiusquest (accountVarbits early) == Just 0)
  assert (Map.lookup VB.thzfeBlockingBarricade (accountVarbits early) == Just 0)
  assert (Map.lookup VB.lotg (accountVarbits maxed) == Just 50)
  assert (Map.lookup VB.myq5 (accountVarbits maxed) == Just 88)
  assert (Map.lookup VB.my2armStatus (accountVarbits maxed) == Just 207)
  assert (all (== Just 7) [Map.lookup VB.hosidiusquest (accountVarbits account) | account <- [mid, end, maxed]])
  assert (all (== Just 1) [Map.lookup VB.thzfeBlockingBarricade (accountVarbits account) | account <- [mid, end, maxed]])
  transports <- loadTransports defaultSourcePaths
  let context account = RequirementContext account CarriedOnly benchmarkNowMinutes
      available account transport = case transportAvailability (context account) transport of
        Available -> True
        _ -> False
      findTransport kind label = find (\transport -> transportType transport == kind && displayInfo transport == label) transports
      base = findTransport "QUETZAL" "Auburnvale"
      camTorum = findTransport "QUETZAL" "Cam Torum"
      outerFortis = findTransport "QUETZAL" "Outer Fortis"
      primio = find (\transport -> origin transport == Just (packTile 3280 3412 0) && destination transport == Just (packTile 1700 3141 0)) transports
  assert (maybe False (available early) base)
  assert (maybe False (not . available early) camTorum)
  assert (maybe False (available cam) camTorum)
  assert (maybe False (not . available cam) outerFortis)
  assert (maybe False (available early) primio)
  world <- loadWorld defaultSourcePaths
  let route = findRoute (RawDijkstra world)
        (defaultQuery (packTile 3280 3412 0) (packTile 1700 3141 0))
          { requirementMode = ConfiguredRequirements early }
  assert (routeCost route < maxBound)
