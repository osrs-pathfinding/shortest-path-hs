module Main (main) where

import Data.Char (isDigit)
import Data.Either (isLeft, isRight)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (find, isInfixOf)

import ShortestPath.Requirements
import ShortestPath.Items
import ShortestPath.Account
import ShortestPath.AccountSemantics
import qualified ShortestPath.GameVars.Varbits as VB
import qualified ShortestPath.GameVars.VarPlayers as VP
import ShortestPath.BenchmarkProfiles
import ShortestPath.Tile
import ShortestPath.Topology
import ShortestPath.Transport
import ShortestPath.World
import ShortestPath.Pathfinder
import ShortestPath.Exact.ReferenceDijkstra
import ShortestPath.Exact.TileAStar

main :: IO ()
main = do
  assert (unpackTile (packTile 3200 3201 2) == (3200, 3201, 2))
  assert (parseSkills "75 Construction;83 Farming" == [SkillReq 75 "Construction", SkillReq 83 "Farming"])
  assert (parseVars Varbit "4070=0;4560&2" == [VarReq (GameVarbit (VarbitId 4070)) 0 VarEq, VarReq (GameVarbit (VarbitId 4560)) 2 VarMask])
  requirementChecks
  itemNormalizationChecks
  profileChecks
  semanticProfileChecks
  assert (parseTileField "3221 3218 0" == Just (packTile 3221 3218 0))

assert :: Bool -> IO ()
assert True = pure ()
assert False = fail "assertion failed"

requirementChecks :: IO ()
requirementChecks = do
  let account = emptyAccountState
        { accountLevels = Map.singleton "Agility" 70
        , accountCompletedQuests = Set.singleton "Quest"
        , accountVarbits = Map.singleton (VarbitId 1) 6
        , accountVarPlayers = Map.singleton (VarPlayerId 2) 100
        , accountInventory = Map.singleton 1 2
        , accountEquipment = Map.singleton 2 1
        , accountRunePouch = Map.singleton 3 1
        , accountBank = Map.singleton 4 1
        }
      carried = RequirementContext account CarriedOnly 130
      banked = RequirementContext account CarriedAndBank 130
      transport = Transport "TEST" Nothing Nothing 0 "" "" False Nothing [SkillReq 70 "Agility"] (Just (ItemAnd [ItemOne (ItemTerm "1" [1] 2), ItemOr [ItemOne (ItemTerm "2" [2] 1), ItemOne (ItemTerm "4" [4] 1)]])) ["Quest"] [VarReq (GameVarbit (VarbitId 1)) 6 VarEq, VarReq (GameVarbit (VarbitId 1)) 2 VarMask] [VarReq (GameVarPlayer (VarPlayerId 2)) 20 VarCooldownMinutes] "test"
  assert (requirementsSatisfied carried transport)
  assert (not (requirementsSatisfied (carried { requirementAccount = account { accountVarPlayers = Map.singleton (VarPlayerId 2) 110 } }) transport))
  assert (requirementsSatisfied (carried { requirementAccount = account { accountVarPlayers = Map.singleton (VarPlayerId 2) 109 } }) transport)
  assert (not (requirementsSatisfied carried (transport { skills = [SkillReq 71 "Agility"] })))
  assert (not (requirementsSatisfied carried (transport { quests = ["Missing"] })))
  assert (not (requirementsSatisfied carried (transport { varbits = [VarReq (GameVarbit (VarbitId 9)) 0 VarEq] })))
  assert (not (requirementsSatisfied carried (transport { items = Just (ItemOne (ItemTerm "4" [4] 1)) })))
  assert (requirementsSatisfied banked (transport { items = Just (ItemOne (ItemTerm "4" [4] 1)) }))

itemNormalizationChecks :: IO ()
itemNormalizationChecks = do
  assert (resolveItemName "995" == Right (ItemVariation [995]))
  assert (resolveItemName "COINS" == Right (ItemVariation [995]))
  assert (resolveItemName "AIR_RUNE" == Right (ItemVariation [556, 4695, 4696, 4697]))
  assert (resolveItemName "LAW_RUNE" == Right (ItemVariation [563]))
  assert (resolveItemName "SHANTAY_PASS" == Right (ItemVariation [1854]))
  let coinsRequirement = parsed "COINS=100"
      numericAccount = emptyAccountState {accountInventory = Map.singleton 995 100000}
      symbolicSpec = emptySpec
      symbolicAccountSpec = symbolicSpec
        { accountSpecCarried = ItemLoadout (Map.singleton "COINS" 1000) Map.empty Map.empty }
      unknownAccountSpec = symbolicSpec
        { accountSpecCarried = ItemLoadout (Map.singleton "UNKNOWN_ROUTING_ITEM" 1) Map.empty Map.empty }
  assert (requirementsSatisfied (RequirementContext numericAccount CarriedOnly 0) (itemTransport coinsRequirement))
  assert (compiledItem 995 symbolicAccountSpec == Just 1000)
  assert (case compileAccount 0 unknownAccountSpec of
    Left (UnknownItemReference "UNKNOWN_ROUTING_ITEM" _) -> True
    _ -> False)
  assert (compileItemReferences "test" (Map.fromList [("995", 1000), ("COINS", 500)]) == Right (Map.singleton 995 1500))
  assert (requiresItem "AIR_RUNE=3" (Map.singleton 556 10))
  assert (requiresItem "AIR_RUNE=3" (Map.singleton 4695 10))
  assert (not (requiresItem "AIR_RUNE=3" (Map.fromList [(556, 2), (4695, 1)])))
  assert (requiresItem "AXE=1" (Map.singleton 1349 1))
  assert (requiresItem "SHANTAY_PASS=1" (Map.singleton 1854 1))
  assert (requiresItem "SHANTAY_PASS=1|COINS=5" (Map.singleton 1854 1))
  assert (not (requiresItem "SHANTAY_PASS=1|COINS=5" (Map.singleton 995 4)))
  assert (requiresItem "SHANTAY_PASS=1|COINS=5" (Map.singleton 995 5))
  assert (isLeft (parseItems "UNKNOWN_ROUTING_ITEM=1"))
  let alternatives = parsed "1=50|2=100"
  assert (requiresItem "1=50|2=100" (Map.singleton 1 50))
  assert (requiresItem "1=50|2=100" (Map.singleton 2 100))
  assert (not (requirementsSatisfied (RequirementContext (emptyAccountState {accountInventory = Map.fromList [(1, 49), (2, 99)]}) CarriedOnly 0) (itemTransport alternatives)))
  transports <- loadTransports defaultSourcePaths
  let routingNames = symbolicNames transports
  putStrLn ("symbolic item names: " <> show (Set.toAscList routingNames))
  assert (all (isRight . resolveItemName) (Set.toList routingNames))
  assert (any (\transport ->
    transportType transport == "GNOME_GLIDER"
      && origin transport == Just (packTile 2465 3501 3)
      && destination transport == Just (packTile 2850 3498 0)) transports)
 where
  parsed raw = case parseItems raw of
    Right (Just expression) -> expression
    result -> error ("item parse failed: " <> show result)
  itemTransport expression = Transport "ITEM_TEST" Nothing Nothing 0 "" "" False Nothing [] (Just expression) [] [] [] "test"
  requiresItem raw counts = requirementsSatisfied (RequirementContext (emptyAccountState {accountInventory = counts}) CarriedOnly 0) (itemTransport (parsed raw))
  compiledItem identifier spec = Map.lookup identifier =<< either (const Nothing) (Just . accountInventory) (compileAccount 0 spec)
  symbolic name = not (null name) && not (all isDigit name)
  symbolicNames transports = Set.fromList
    [ itemName term
    | transport <- transports
    , Just expression <- [items transport]
    , term <- itemTerms expression
    , symbolic (itemName term)
    ]
  itemTerms expression = case expression of
    ItemOne term -> [term]
    ItemAnd expressions -> concatMap itemTerms expressions
    ItemOr expressions -> concatMap itemTerms expressions

profileChecks :: IO ()
profileChecks = do
  let fairyRing = Transport "FAIRY_RING" Nothing Nothing 0 "" "" False Nothing [] Nothing [] [] [] "test"
      basicBox = Transport "TELEPORTATION_BOX" Nothing Nothing 0 "" "Basic Jewellery Box" False Nothing [] Nothing [] [] [] "test"
      varrockPortal = Transport "TELEPORTATION_PORTAL_POH" Nothing Nothing 0 "Varrock Portal" "" False Nothing [] Nothing [] [] [] "test"
      profile name = mustProfile name (benchmarkAccount name [])
      early = profile "early"
      mid = profile "mid"
      end = profile "end"
      maxed = profile "maxed"
      dragonDoor = Transport "TRANSPORT" Nothing Nothing 0 "Open Wall" "" False Nothing [] Nothing ["Dragon Slayer I"] [VarReq (GameVarbit VB.dragonslayerCrandorFoundSecretDoor) 1 VarEq] [] "test"
      maxedWithDragon = mustProfile "maxed" (benchmarkAccount "maxed" [dragonDoor])
      unknownDoor = dragonDoor { quests = [], varbits = [VarReq (GameVarbit VB.tapoyauikRuinsFailedWallslide) 1 VarEq] }
      context account = RequirementContext account CarriedOnly benchmarkNowMinutes
      withoutStaff account = account { accountInventory = Map.delete 772 (accountInventory account) }
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
  assert (Map.member 13393 (accountBank mid))
  assert (Map.member 28327 (accountBank end))
  assert (not (Map.member 28327 (accountBank mid)))
  assert (not (Map.member 13249 (accountBank end)))
  assert (not (Map.member 4513 (accountBank mid)))
  assert (Map.keysSet (accountBank end) `Set.isSubsetOf` Map.keysSet (accountBank maxed))
 where
  isUnknown (UnknownVarRequirements _) = True
  isUnknown _ = False

mustProfile :: String -> Maybe AccountState -> AccountState
mustProfile _ (Just account) = account
mustProfile name Nothing = error ("missing benchmark profile: " <> name)

semanticProfileChecks :: IO ()
semanticProfileChecks = do
  let specs = [emptySpec]
      compiled = map (compileAccount benchmarkNowMinutes) specs
  assert (all isRight compiled)
  assert (compiled == map (compileAccount benchmarkNowMinutes) specs)
  compilerChecks emptySpec
  assert (classifyUnmodelledVar (GameVarbit VB.karamDungeonEntryfee) == Just RuntimeVar)
  assert (classifyUnmodelledVar (GameVarPlayer VP.leagueCombatMasteryPaths) == Just SpecialModeVar)
  assert (classifyUnmodelledVar (GameVarPlayer VP.haunted) == Just NeedsInvestigation)
  assert (all (== 0) (compilePermanentUnlockVars Set.empty))
  assert (compileCatacombsEntranceVars (Set.singleton CatacombsForthosDungeon) == Map.fromList
    [ (VB.cataHole1, 1)
    , (VB.cataHole2, 0)
    , (VB.cataHoleGiantsDen, 0)
    ])
  assert (compileHotAirBalloonVars (Set.singleton BalloonEntrana) == Map.fromList
    [ (VB.zepMultiBasket, 2)
    , (VB.zepMultiPiccard, 0)
    , (VB.zepMultiCast, 0)
    , (VB.zepMultiGno, 0)
    , (VB.zepMultiCraft, 0)
    , (VB.zepMultiVarr, 0)
    ])
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
  assert (all (\account -> Set.member "Song of the Elves" (accountCompletedQuests account)) [mid, end, maxed])
  assert (not (Set.member "Song of the Elves" (accountCompletedQuests early)))
  assert (all (\account -> Set.member "Enlightened Journey" (accountCompletedQuests account)) [mid, end, maxed])
  assert (not (Set.member "Enlightened Journey" (accountCompletedQuests early)))
  assert (all (\account -> Set.member "Legends' Quest" (accountCompletedQuests account)) [mid, end, maxed])
  assert (not (Set.member "Legends' Quest" (accountCompletedQuests early)))
  assert (all (\account -> Set.member "Shilo Village" (accountCompletedQuests account)) [mid, end, maxed])
  assert (not (Set.member "Shilo Village" (accountCompletedQuests early)))
  assert (all (\account -> Set.member "Waterfall Quest" (accountCompletedQuests account)) [mid, end, maxed])
  assert (not (Set.member "Waterfall Quest" (accountCompletedQuests early)))
  assert (not (Set.member "Darkness of Hallowvale" (accountCompletedQuests early)))
  assert (Map.lookup VP.fishingcompo (accountVarPlayers early) == Just 0)
  assert (all (== Just 5) [Map.lookup VP.fishingcompo (accountVarPlayers account) | account <- [mid, end, maxed]])
  assert (all (== Just 10) [Map.lookup VB.golemA (accountVarbits account) | account <- [mid, end, maxed]])
  assert (all (== Just 26) [Map.lookup VB.icsLittleVar (accountVarbits account) | account <- [mid, end, maxed]])
  assert (all (== Just 200) [Map.lookup VB.swansong (accountVarbits account) | account <- [mid, end, maxed]])
  assert (all (== Just 108) [Map.lookup VB.bcs (accountVarbits account) | account <- [mid, end, maxed]])
  assert (Map.lookup VB.lostTribeQuest (accountVarbits early) == Just 12)
  assert (Map.lookup VP.grandtree (accountVarPlayers early) == Just 160)
  assert (Map.lookup VP.regicideQuest (accountVarPlayers early) == Just 15)
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
  assert (Map.lookup VB.lovaquest (accountVarbits early) == Just 0)
  assert (all (== Just 11) [Map.lookup VB.lovaquest (accountVarbits account) | account <- [mid, end, maxed]])
  assert (Map.lookup VP.legendsquest (accountVarPlayers early) == Just 0)
  assert (all (== Just 75) [Map.lookup VP.legendsquest (accountVarPlayers account) | account <- [mid, end, maxed]])
  assert (Map.lookup VP.zombiequeen (accountVarPlayers early) == Just 0)
  assert (all (== Just 15) [Map.lookup VP.zombiequeen (accountVarPlayers account) | account <- [mid, end, maxed]])
  assert (Map.lookup VP.waterfallQuest (accountVarPlayers early) == Just 0)
  assert (all (== Just 10) [Map.lookup VP.waterfallQuest (accountVarPlayers account) | account <- [mid, end, maxed]])
  assert (Map.lookup VB.myq3MainQuest (accountVarbits early) == Just 0)
  assert (all (== Just 320) [Map.lookup VB.myq3MainQuest (accountVarbits account) | account <- [mid, end, maxed]])
  assert (all (== Just 0)
    [ Map.lookup varbit (accountVarbits early)
    | varbit <- [VB.zepMultiBasket, VB.zepMultiPiccard, VB.zepMultiCast, VB.zepMultiGno, VB.zepMultiCraft, VB.zepMultiVarr]
    ])
  assert (all (== Just True)
    [ (== value) <$> Map.lookup varbit (accountVarbits account)
    | account <- [mid, end, maxed]
    , (varbit, value) <- [(VB.zepMultiBasket, 2), (VB.zepMultiPiccard, 2), (VB.zepMultiCast, 1), (VB.zepMultiGno, 1), (VB.zepMultiCraft, 1), (VB.zepMultiVarr, 1)]
    ])
  assert (all (== Just 0)
    [ Map.lookup varbit (accountVarbits early)
    | varbit <- [VB.cataHole1, VB.cataHole2, VB.cataHoleGiantsDen]
    ])
  assert (all (== Just 1)
    [ Map.lookup varbit (accountVarbits account)
    | account <- [mid, end, maxed]
    , varbit <- [VB.cataHole1, VB.cataHole2, VB.cataHoleGiantsDen]
    ])
  assert (Map.lookup VB.raidsGuideTravelUnlock (accountVarbits early) == Just 0)
  assert (all (== Just 1) [Map.lookup VB.raidsGuideTravelUnlock (accountVarbits account) | account <- [mid, end, maxed]])
  assert (all (== Just 1) [Map.lookup VB.corsairCoveResourceEntry (accountVarbits account) | account <- [early, mid, end, maxed]])
  assert (all (== Just 153) [Map.lookup VB.vmKudos (accountVarbits account) | account <- [mid, end, maxed]])
  assert (all (== Just 1) [Map.lookup VB.atjunMedReward (accountVarbits account) | account <- [early, mid, end, maxed]])
  assert (all (== Just 1) [Map.lookup VB.amenityRowboatVatrachos (accountVarbits account) | account <- [mid, end, maxed]])
  assert (Map.lookup VB.amenityRowboatVatrachos (accountVarbits early) == Just 0)
  assert (all (== Just 0) [Map.lookup VB.yanilleTeleportLocation (accountVarbits account) | account <- [early, mid, end, maxed]])
  assert (Map.lookup VB.faladorSpawn (accountVarbits early) == Just 0)
  assert (all (== Just 1) [Map.lookup VB.faladorSpawn (accountVarbits account) | account <- [mid, end, maxed]])

  transports <- loadTransports defaultSourcePaths
  let context account = RequirementContext account CarriedOnly benchmarkNowMinutes
      available account transport = case transportAvailability (context account) transport of
        Available -> True
        _ -> False
      findTransport kind label = find (\transport -> transportType transport == kind && displayInfo transport == label) transports
      base = findTransport "QUETZAL" "Auburnvale"
      camTorum = findTransport "QUETZAL" "Cam Torum"
      outerFortis = findTransport "QUETZAL" "Outer Fortis"
      freeMinecart = find (\transport -> transportType transport == "MINECART" && varbits transport == [VarReq (GameVarbit VB.lovaquest) 11 VarEq]) transports
      legendsCaveShortcut = find (\transport -> varPlayers transport == [VarReq (GameVarPlayer VP.legendsquest) 6 VarGt]) transports
      kharaziShortcut = find (\transport -> varPlayers transport == [VarReq (GameVarPlayer VP.legendsquest) 49 VarGt] && items transport == Nothing) transports
      shiloCart = find (\transport -> varPlayers transport == [VarReq (GameVarPlayer VP.zombiequeen) 14 VarGt] && items transport == Nothing) transports
      waterfallAccess =
        [ find (\transport -> varPlayers transport == [VarReq (GameVarPlayer VP.waterfallQuest) value operator]) transports
        | (value, operator) <- [(2, VarGt), (10, VarEq)]
        ]
      hallowvaleAccess = find (\transport -> varbits transport == [VarReq (GameVarbit VB.myq3MainQuest) 320 VarEq]) transports
      fishingContestAccess = find (\transport -> varPlayers transport == [VarReq (GameVarPlayer VP.fishingcompo) 5 VarEq]) transports
      catacombsEntrances =
        [ find (\transport -> varbits transport == [VarReq (GameVarbit varbit) 1 VarEq]) transports
        | varbit <- [VB.cataHole1, VB.cataHole2, VB.cataHoleGiantsDen]
        ]
      mountainGuideTravel = find (\transport -> varbits transport == [VarReq (GameVarbit VB.raidsGuideTravelUnlock) 1 VarEq]) transports
      corsairResourceArea = find (\transport -> varbits transport == [VarReq (GameVarbit VB.corsairCoveResourceEntry) 1 VarEq]) transports
      balloons = map (findTransport "HOT_AIR_BALLOON") ["Entrana", "Taverley", "Castle Wars", "Grand Tree", "Crafting Guild", "Varrock"]
      primio = find (\transport -> origin transport == Just (packTile 3280 3412 0) && destination transport == Just (packTile 1700 3141 0)) transports
  assert (maybe False (available early) base)
  assert (maybe False (not . available early) camTorum)
  assert (maybe False (available cam) camTorum)
  assert (maybe False (not . available cam) outerFortis)
  assert (maybe False (not . available early) freeMinecart)
  assert (maybe False (available mid) freeMinecart)
  assert (maybe False (not . available early) legendsCaveShortcut)
  assert (maybe False (available mid) legendsCaveShortcut)
  assert (maybe False (not . available early) kharaziShortcut)
  assert (maybe False (available mid) kharaziShortcut)
  assert (maybe False (not . available early) shiloCart)
  assert (maybe False (available mid) shiloCart)
  assert (all (maybe False (not . available early)) waterfallAccess)
  assert (all (maybe False (available mid)) waterfallAccess)
  assert (maybe False (not . available early) hallowvaleAccess)
  assert (maybe False (available mid) hallowvaleAccess)
  assert (maybe False (not . available early) fishingContestAccess)
  assert (maybe False (available mid) fishingContestAccess)
  assert (all (maybe False (not . available early)) catacombsEntrances)
  assert (all (maybe False (available mid)) catacombsEntrances)
  assert (maybe False (not . available early) mountainGuideTravel)
  assert (maybe False (available mid) mountainGuideTravel)
  assert (maybe False (available early) corsairResourceArea)
  assert (all (maybe False (not . available early)) balloons)
  assert (all (maybe False (available mid)) balloons)
  assert (maybe False (available early) primio)
  world <- loadWorld defaultSourcePaths
  let transportSites =
        [ site
        | transport <- concat (Map.elems (worldTransports world)) <> worldGlobalTeleports world
        , Just site <- [origin transport, destination transport]
        ]
      walkingSamples = take 4096 (collisionTiles (worldCollision world)) <> transportSites
      maskNeighbors tile = ordinaryWalkingNeighborsFromMask tile (ordinaryWalkingMask (worldCollision world) tile)
      ordinaryNeighbors tile = filter (isWalkable (worldCollision world)) (walkingNeighbors world tile)
  assert (all (\tile -> not (isWalkable (worldCollision world) tile) || maskNeighbors tile == ordinaryNeighbors tile) walkingSamples)
  topology <- buildWorldTopology world
  astar <- buildTileAStarFromTopology topology
  let transportData = concat (Map.elems (worldTransports world)) <> worldGlobalTeleports world
  pohRealChecks astar transportData
  let route = findRouteReferenceDijkstra (ReferenceDijkstra topology)
        (defaultQuery (packTile 3280 3412 0) (packTile 1700 3141 0))
          { requirementMode = ConfiguredRequirements early }
  assert (routeCost route < maxBound)
pohRealChecks :: TileAStar -> [Transport] -> IO ()
pohRealChecks astar transports = do
  let maxed = mustProfile "maxed" (benchmarkAccount "maxed" transports)
      early = mustProfile "early" (benchmarkAccount "early" transports)
  let inbound kind file = find
        (\transport -> transportType transport == kind
          && file `isInfixOf` source transport
          && destination transport == Just pohLanding
          && maybe False (not . isInsidePoh) (origin transport)
          && transportAvailability (RequirementContext maxed CarriedOnly benchmarkNowMinutes) transport == Available)
        transports
      fairyIngress = must "real fairy-ring POH ingress" (inbound "FAIRY_RING" "fairy_rings.tsv:")
      spiritIngress = must "real spirit-tree POH ingress" (inbound "SPIRIT_TREE" "spirit_trees.tsv:")
      portals = filter (available maxed) [transport | transport <- transports, transportType transport == "TELEPORTATION_PORTAL_POH", isPohOrigin transport]
      portalA = must "real POH portal exit" (find (const True) portals)
      portalB = must "second real POH portal exit" (find ((/= destination portalA) . destination) portals)
      options = Set.fromList ["FAIRY_RING", "SPIRIT_TREE", "TELEPORTATION_PORTAL_POH"]
      queryFor ingress exit = (defaultQuery (must "ingress origin" (origin ingress)) (must "exit destination" (destination exit)))
        { enabledTransportTypes = options
        , bankPathEnabled = False
        , requirementMode = ConfiguredRequirements maxed
        }
      route1 = findRouteTileAStar astar (queryFor fairyIngress portalA)
      route2 = findRouteTileAStar astar (queryFor spiritIngress portalB)
      expected ingress exit = [UseTransport (transportLabel ingress) pohLanding, UseTransport (transportLabel exit) (must "exit destination" (destination exit))]
      atLanding = preparedLocalTransportsAt
        (compiledTransportAvailability (compileRoutingAccount astar (routingOptionsFromQuery (queryFor fairyIngress portalA)))) False pohLanding
      sameEdge left right = origin right == Just pohLanding
        && destination right == destination left
        && duration right == duration left
        && transportType right == transportType left
        && displayInfo right == displayInfo left
      available account transport = transportAvailability (RequirementContext account CarriedOnly benchmarkNowMinutes) transport == Available
      isPohOrigin transport = case origin transport of
        Just tile -> isInsidePoh tile && tile /= pohLanding
        Nothing -> False
  assertPoh "fairy ingress destination" (destination fairyIngress == Just pohLanding)
  assertPoh "spirit ingress destination" (destination spiritIngress == Just pohLanding)
  assertPoh "maxed landing exposes both exits" (all (\exit -> any (sameEdge exit) atLanding) [portalA, portalB])
  assertPoh "early landing does not expose portal A" (not (any (sameEdge portalA) (preparedLocalTransportsAt
    (compiledTransportAvailability (compileRoutingAccount astar (routingOptionsFromQuery (queryFor fairyIngress portalA) {requirementMode = ConfiguredRequirements early}))) False pohLanding)))
  assertPoh "fairy to POH portal route" (routeCost route1 == duration fairyIngress + duration portalA && routeSteps route1 == expected fairyIngress portalA)
  assertPoh "spirit tree to POH portal route" (routeCost route2 == duration spiritIngress + duration portalB && routeSteps route2 == expected spiritIngress portalB)
 where
  assertPoh _ True = pure ()
  assertPoh message False = fail ("POH assertion failed: " <> message)
  must _ (Just value) = value
  must message Nothing = error message

compilerChecks :: AccountSpec -> IO ()
compilerChecks base = do
  let raw = accountSpecRawGameState base
      withRaw bits players = base {accountSpecRawGameState = raw {rawVarbitOverrides = bits, rawVarPlayerOverrides = players}}
      sameSpellbook = withRaw (Map.singleton VB.spellbook 0) Map.empty
      rawOnly = withRaw (Map.singleton (VarbitId 999999) 7) Map.empty
      conflictingSpellbook = withRaw (Map.singleton VB.spellbook 1) Map.empty
      conflictingQuetzals = withRaw Map.empty (Map.singleton VP.quetzalsUnlocked 1)
      ancient = base {accountSpecRuntime = (accountSpecRuntime base) {runtimeSpellbook = Ancient}}
      taverley = base {accountSpecPoh = (accountSpecPoh base) {pohLocation = Taverley}}
  assert (isRight (compileAccount benchmarkNowMinutes sameSpellbook))
  assert (compiledVarbit (VarbitId 999999) rawOnly == Just 7)
  assert (compileAccount benchmarkNowMinutes conflictingSpellbook == Left (ConflictingVarbit VB.spellbook 0 1))
  assert (compileAccount benchmarkNowMinutes conflictingQuetzals == Left (ConflictingVarPlayer VP.quetzalsUnlocked 0 1))
  assert (compiledVarbit VB.spellbook ancient == Just 1)
  assert (compiledVarbit VB.pohHouseLocation taverley == Just 2)
 where
  compiledVarbit varbit spec = either (const Nothing) (Map.lookup varbit . accountVarbits) (compileAccount benchmarkNowMinutes spec)

emptySpec :: AccountSpec
emptySpec = AccountSpec
  (Progression Map.empty Set.empty Set.empty Map.empty False Set.empty Set.empty Set.empty Set.empty Set.empty)
  (RawGameState Map.empty Map.empty)
  (PohBuild Rimmington NoJewelleryBox (SelectedPohPortals Set.empty) False False False False False False False)
  (ItemLoadout (Map.singleton "COINS" 1000) Map.empty Map.empty)
  Map.empty
  (RuntimeState Standard CooldownReady True)
