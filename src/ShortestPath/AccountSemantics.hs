module ShortestPath.AccountSemantics
  ( AccountSpec(..)
  , AccountCompileError(..)
  , Progression(..)
  , Diary(..)
  , QuestMilestone(..)
  , QuetzalPlatform(..)
  , HotAirBalloonDestination(..)
  , CatacombsEntrance(..)
  , PermanentUnlock(..)
  , UnmodelledVarClass(..)
  , CompiledVars(..)
  , RawGameState(..)
  , ItemLoadout(..)
  , compileAccount
  , compileProgressionVars
  , compileHotAirBalloonVars
  , compileCatacombsEntranceVars
  , compilePermanentUnlockVars
  , classifyUnmodelledVar
  , effectiveQuestMilestones
  , diaryVarbitsFor
  , compileDiary
  , quetzalPlatformBit
  , quetzalPlatformMask
  ) where

import Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Bits ((.|.))

import ShortestPath.Account
import qualified ShortestPath.GameVars.Varbits as VB
import qualified ShortestPath.GameVars.VarPlayers as VP
import ShortestPath.Requirements

data QuetzalPlatform
  = CamTorum
  | ColossalWyrmRemains
  | OuterFortis
  | FortisColosseum
  | SalvagerOverlook
  | Kastori
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data HotAirBalloonDestination
  = BalloonEntrana
  | BalloonTaverley
  | BalloonCastleWars
  | BalloonGrandTree
  | BalloonCraftingGuild
  | BalloonVarrock
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data CatacombsEntrance
  = CatacombsForthosDungeon
  | CatacombsSurfaceEntrances
  | CatacombsGiantsDen
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data PermanentUnlock
  = RaidsMountainGuideTravel | CorsairCoveResourceArea
  | LostTribeCellarHole | BarbarianAssaultTutorial | MuseumKudos153
  | BarbarianFiremakingTraining | FenkenstrainBridgeNorth | FenkenstrainBridgeSouth
  | KaramjaDungeonBackdoor | ObservatoryShortcutRope
  | HosidiusDungeonWestDoor | HosidiusDungeonEastDoor
  | DarkmeyerInnerShortcut | DarkmeyerOuterShortcut | MetAuburnMountainGuide
  | BookOfScrollsNardah | BookOfScrollsDigsite | BookOfScrollsFeldip
  | BookOfScrollsLunarIsle | BookOfScrollsMortton | BookOfScrollsPestControl
  | BookOfScrollsPiscatoris | BookOfScrollsTaiBwo | BookOfScrollsElf
  | BookOfScrollsMosLeHarmless | BookOfScrollsLumberyard | BookOfScrollsZulAndra
  | BookOfScrollsCerberus | BookOfScrollsRevenants | BookOfScrollsWatson
  | PendantOfAtesDarkfrost | PendantOfAtesTwilight | PendantOfAtesRalos | PendantOfAtesAldarin
  | PharaohsSceptreNecropolis | ColosseumWaveNine
  | RowboatVatrachos | RowboatAnglers | RowboatSoulTear | RowboatYnysdail | RowboatBuccaneers
  | RespawnFalador | RespawnCamelot | RespawnEdgeville
  | RespawnFeroxEnclave | RespawnKourendCastle | RespawnCivitasIllaFortis
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data UnmodelledVarClass = RuntimeVar | SpecialModeVar | NeedsInvestigation
  deriving stock (Eq, Ord, Show)

classifyUnmodelledVar :: GameVar -> Maybe UnmodelledVarClass
classifyUnmodelledVar variable
  | variable `elem` map GameVarbit
      [ VB.karamDungeonEntryfee, VB.veosMemoirCharges
      , VB.tapoyauikRuinsFailedWallslide, VB.tapoyauikFailedSteppingStones
      ] = Just RuntimeVar
  | variable == GameVarPlayer VP.leagueCombatMasteryPaths = Just SpecialModeVar
  | variable == GameVarPlayer VP.haunted = Just NeedsInvestigation
  | otherwise = Nothing

data Progression = Progression
  { progressionLevels :: ItemCounts
  , progressionQuests :: Set.Set String
  , progressionMilestones :: Set.Set QuestMilestone
  , progressionDiaries :: Map.Map Diary DiaryTier
  , progressionFairyRings :: Bool
  , progressionQuetzalPlatforms :: Set.Set QuetzalPlatform
  , progressionHotAirBalloonDestinations :: Set.Set HotAirBalloonDestination
  , progressionCatacombsEntrances :: Set.Set CatacombsEntrance
  , progressionPermanentUnlocks :: Set.Set PermanentUnlock
  }
  deriving stock (Eq, Show)

data Diary = Ardougne | Desert | Falador | Fremennik | Kandarin | Karamja
  | KourendKebos | LumbridgeDraynor | Morytania | Varrock
  | WesternProvinces | Wilderness
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data QuestMilestone
  = LandOfTheGoblinsYuBiuskAccess
  | SinsOfTheFatherSlepeBoatAccess
  deriving stock (Eq, Ord, Show)

data ItemLoadout = ItemLoadout
  { loadoutInventory :: ItemCounts
  , loadoutEquipment :: ItemCounts
  , loadoutRunePouch :: ItemCounts
  }
  deriving stock (Eq, Show)

data RawGameState = RawGameState
  { rawVarbitOverrides :: Map.Map VarbitId Int
  , rawVarPlayerOverrides :: Map.Map VarPlayerId Int
  }
  deriving stock (Eq, Show)

data CompiledVars = CompiledVars
  { compiledVarbits :: Map.Map VarbitId Int
  , compiledVarPlayers :: Map.Map VarPlayerId Int
  }
  deriving stock (Eq, Show)

data AccountSpec = AccountSpec
  { accountSpecProgression :: Progression
  , accountSpecRawGameState :: RawGameState
  , accountSpecPoh :: PohBuild
  , accountSpecCarried :: ItemLoadout
  , accountSpecBank :: ItemCounts
  , accountSpecRuntime :: RuntimeState
  }
  deriving stock (Eq, Show)

diaryName :: Diary -> String
diaryName = \case
  Ardougne -> "Ardougne"; Desert -> "Desert"; Falador -> "Falador"
  Fremennik -> "Fremennik"; Kandarin -> "Kandarin"; Karamja -> "Karamja"
  KourendKebos -> "Kourend & Kebos"; LumbridgeDraynor -> "Lumbridge & Draynor"
  Morytania -> "Morytania"; Varrock -> "Varrock"
  WesternProvinces -> "Western Provinces"; Wilderness -> "Wilderness"

effectiveQuestMilestones :: Progression -> Set.Set QuestMilestone
effectiveQuestMilestones progress = progressionMilestones progress <> Set.fromList
  [ LandOfTheGoblinsYuBiuskAccess | Set.member "Land of the Goblins" quests ]
  <> Set.fromList
  [ SinsOfTheFatherSlepeBoatAccess | Set.member "Sins of the Father" quests ]
 where
  quests = progressionQuests progress

data AccountCompileError
  = ConflictingVarbit VarbitId Int Int
  | ConflictingVarPlayer VarPlayerId Int Int
  deriving stock (Eq, Show)

compileAccount :: Int -> AccountSpec -> Either AccountCompileError AccountState
compileAccount now spec = do
  progressionVars <- compileProgressionVars progress
  compiled <- mergeCompiledVars
    [ progressionVars
    , compileRuntimeVars now (accountSpecRuntime spec)
    , compilePohVars (accountSpecPoh spec)
    , CompiledVars (rawVarbitOverrides raw) (rawVarPlayerOverrides raw)
    ]
  pure emptyAccountState
    { accountLevels = progressionLevels progress
    , accountCompletedQuests = progressionQuests progress
    , accountVarbits = compiledVarbits compiled
    , accountVarPlayers = compiledVarPlayers compiled
    , accountInventory = loadoutInventory loadout
    , accountEquipment = loadoutEquipment loadout
    , accountRunePouch = loadoutRunePouch loadout
    , accountBank = accountSpecBank spec
    , accountDiaries = Map.fromList
        [(diaryName diary, tier) | (diary, tier) <- Map.toList (progressionDiaries progress)]
    , accountPoh = accountSpecPoh spec
    , accountFairyRingsUnlocked = progressionFairyRings progress
    , accountRuntime = accountSpecRuntime spec
    }
 where
  progress = accountSpecProgression spec
  raw = accountSpecRawGameState spec
  loadout = accountSpecCarried spec

compileProgressionVars :: Progression -> Either AccountCompileError CompiledVars
compileProgressionVars progress = mergeCompiledVars
  [ CompiledVars (diaryVarbits (progressionDiaries progress)) Map.empty
  , CompiledVars (compileDiaryDerivedVarbits (progressionDiaries progress)) Map.empty
  , CompiledVars (compileQuestDerivedVarbits progress) Map.empty
  , CompiledVars Map.empty (compileQuestDerivedVarPlayers progress)
  , CompiledVars (compileHotAirBalloonVars (progressionHotAirBalloonDestinations progress)) Map.empty
  , CompiledVars (compileCatacombsEntranceVars (progressionCatacombsEntrances progress)) Map.empty
  , CompiledVars (compilePermanentUnlockVars (progressionPermanentUnlocks progress)) Map.empty
  , CompiledVars Map.empty (compileQuetzalVars (progressionQuetzalPlatforms progress))
  , compileDefaultVars
  ]

compileQuestDerivedVarbits :: Progression -> Map.Map VarbitId Int
compileQuestDerivedVarbits progress = Map.fromList
  [ (VB.lovaquest, completed "The Forsaken Tower" forsakenTowerCompleteValue)
  , (VB.my2armStatus, completed "Making Friends with My Arm" makingFriendsCompleteValue)
  , (VB.thzfeBlockingBarricade, completed "Zogre Flesh Eaters" thzfeBlockingBarricadeValue)
  , (VB.hosidiusquest, completed "The Depths of Despair" depthsOfDespairCaveAccessValue)
  , (VB.myq5, if SinsOfTheFatherSlepeBoatAccess `Set.member` milestones then myq5BoatUnlockedValue else 0)
  , (VB.lotg, if LandOfTheGoblinsYuBiuskAccess `Set.member` milestones then lotgYuBiuskUnlockedValue else 0)
  , (VB.dragonslayerCrandorFoundSecretDoor, completed "Dragon Slayer I" 1)
  , (VB.myq3MainQuest, completed "Darkness of Hallowvale" darknessOfHallowvaleCompleteValue)
  , (VB.mdaughterQuestVar, completed "Mountain Daughter" 70)
  , (VB.dwarfrockQuest, completed "Between a Rock..." 10)
  , (VB.golemA, completed "The Golem" 10)
  , (VB.icsLittleVar, completed "Icthlarin's Little Helper" 26)
  , (VB.togJunaBowl, completed "Tears of Guthix" 2)
  , (VB.zogre, completed "Zogre Flesh Eaters" 14)
  , (VB.lostTribeQuest, completed "The Lost Tribe" 12)
  , (VB.swansong, completed "Swan Song" 200)
  , (VB.frisQuest, completed "The Fremennik Isles" 340)
  , (VB.veosProgress, completed "Client of Kourend" 1)
  , (VB.hosidiusquestReward, completed "The Depths of Despair" 1)
  , (VB.piscquestReward, completed "The Queen of Thieves" 1)
  , (VB.shayzienquestReward, completed "The Tale of the Righteous" 1)
  , (VB.lovaquestReward, completed "The Forsaken Tower" 1)
  , (VB.arcquestReward, completed "Architectural Alliance" 1)
  , (VB.bcs, completed "Beneath Cursed Sands" 108)
  ]
 where
  quests = progressionQuests progress
  milestones = effectiveQuestMilestones progress
  completed quest value
    | Set.member quest quests = value
    | otherwise = 0

-- Benchmark semantic fallback values for the quest states required by GPS.
lotgYuBiuskUnlockedValue, myq5BoatUnlockedValue, makingFriendsCompleteValue, depthsOfDespairCaveAccessValue, thzfeBlockingBarricadeValue, forsakenTowerCompleteValue, darknessOfHallowvaleCompleteValue :: Int
lotgYuBiuskUnlockedValue = 50
myq5BoatUnlockedValue = 88
makingFriendsCompleteValue = 207
depthsOfDespairCaveAccessValue = 7
thzfeBlockingBarricadeValue = 1
forsakenTowerCompleteValue = 11
darknessOfHallowvaleCompleteValue = 320

compileQuestDerivedVarPlayers :: Progression -> Map.Map VarPlayerId Int
compileQuestDerivedVarPlayers progress = Map.fromList
  [ (VP.legendsquest, completed "Legends' Quest" legendsQuestCompleteValue)
  , (VP.zombiequeen, completed "Shilo Village" shiloVillageCompleteValue)
  , (VP.waterfallQuest, completed "Waterfall Quest" waterfallQuestCompleteValue)
  , (VP.fishingcompo, completed "Fishing Contest" fishingContestCompleteValue)
  , (VP.treequest, completed "Tree Gnome Village" 9)
  , (VP.grandtree, completed "The Grand Tree" 160)
  , (VP.elenaquest, completed "Plague City" 30)
  , (VP.dragonquest, completed "Dragon Slayer I" 10)
  , (VP.itwatchtower, completed "Watchtower" 14)
  , (VP.regicideQuest, completed "Regicide" 15)
  , (VP.miscQuest, completed "Throne of Miscellania" 100)
  , (VP.mourningQuest, completed "Mourning's End Part I" 9)
  ]
 where
  completed quest value
    | Set.member quest (progressionQuests progress) = value
    | otherwise = 0

legendsQuestCompleteValue, shiloVillageCompleteValue, waterfallQuestCompleteValue, fishingContestCompleteValue :: Int
legendsQuestCompleteValue = 75
shiloVillageCompleteValue = 15
waterfallQuestCompleteValue = 10
fishingContestCompleteValue = 5

compileQuetzalVars :: Set.Set QuetzalPlatform -> Map.Map VarPlayerId Int
compileQuetzalVars platforms = Map.singleton quetzalsUnlockedVarPlayer (quetzalPlatformMask platforms)

compileHotAirBalloonVars :: Set.Set HotAirBalloonDestination -> Map.Map VarbitId Int
compileHotAirBalloonVars destinations = Map.fromList
  [ (VB.zepMultiBasket, unlocked BalloonEntrana 2)
  , (VB.zepMultiPiccard, unlocked BalloonTaverley 2)
  , (VB.zepMultiCast, unlocked BalloonCastleWars 1)
  , (VB.zepMultiGno, unlocked BalloonGrandTree 1)
  , (VB.zepMultiCraft, unlocked BalloonCraftingGuild 1)
  , (VB.zepMultiVarr, unlocked BalloonVarrock 1)
  ]
 where
  unlocked destination value
    | Set.member destination destinations = value
    | otherwise = 0

compileCatacombsEntranceVars :: Set.Set CatacombsEntrance -> Map.Map VarbitId Int
compileCatacombsEntranceVars entrances = Map.fromList
  [ (VB.cataHole1, unlocked CatacombsForthosDungeon)
  , (VB.cataHole2, unlocked CatacombsSurfaceEntrances)
  , (VB.cataHoleGiantsDen, unlocked CatacombsGiantsDen)
  ]
 where
  unlocked entrance
    | Set.member entrance entrances = 1
    | otherwise = 0

compilePermanentUnlockVars :: Set.Set PermanentUnlock -> Map.Map VarbitId Int
compilePermanentUnlockVars unlocks = Map.fromList
  [ (varbit, if Set.member unlock unlocks then value else 0)
  | (unlock, varbit, value) <- permanentUnlockVarbits
  ]
 where
  permanentUnlockVarbits =
    [ (RaidsMountainGuideTravel, VB.raidsGuideTravelUnlock, 1)
    , (CorsairCoveResourceArea, VB.corsairCoveResourceEntry, 1)
    , (LostTribeCellarHole, VB.lostTribeHole2Dug, 1)
    , (BarbarianAssaultTutorial, VB.barbassaultArenanewb, 11)
    , (MuseumKudos153, VB.vmKudos, 153)
    , (BarbarianFiremakingTraining, VB.brutFire, 2)
    , (FenkenstrainBridgeNorth, VB.fenkBuiltBridgeNorth, 2)
    , (FenkenstrainBridgeSouth, VB.fenkBuiltBridgeSouth, 2)
    , (KaramjaDungeonBackdoor, VB.karamDungeonBackdoor, 1)
    , (ObservatoryShortcutRope, VB.observatoryShortcutRope, 1)
    , (HosidiusDungeonWestDoor, VB.hosdunWestDoorStatus, 1)
    , (HosidiusDungeonEastDoor, VB.hosdunEastDoorStatus, 1)
    , (DarkmeyerInnerShortcut, VB.darkmShortcutInner, 1)
    , (DarkmeyerOuterShortcut, VB.darkmShortcutOuter, 1)
    , (MetAuburnMountainGuide, VB.metAuburnMountainGuide, 1)
    , (BookOfScrollsNardah, VB.bookofscrollsNardah, 1)
    , (BookOfScrollsDigsite, VB.bookofscrollsDigsite, 1)
    , (BookOfScrollsFeldip, VB.bookofscrollsFeldip, 1)
    , (BookOfScrollsLunarIsle, VB.bookofscrollsLunarisle, 1)
    , (BookOfScrollsMortton, VB.bookofscrollsMortton, 1)
    , (BookOfScrollsPestControl, VB.bookofscrollsPestcontrol, 1)
    , (BookOfScrollsPiscatoris, VB.bookofscrollsPiscatoris, 1)
    , (BookOfScrollsTaiBwo, VB.bookofscrollsTaibwo, 1)
    , (BookOfScrollsElf, VB.bookofscrollsElf, 1)
    , (BookOfScrollsMosLeHarmless, VB.bookofscrollsMosles, 1)
    , (BookOfScrollsLumberyard, VB.bookofscrollsLumberyard, 1)
    , (BookOfScrollsZulAndra, VB.bookofscrollsZulandra, 1)
    , (BookOfScrollsCerberus, VB.bookofscrollsCerberus, 1)
    , (BookOfScrollsRevenants, VB.bookofscrollsRevenants, 1)
    , (BookOfScrollsWatson, VB.bookofscrollsWatsonLowbits, 1)
    , (PendantOfAtesDarkfrost, VB.pendantOfAtesDarkfrostFound, 1)
    , (PendantOfAtesTwilight, VB.pendantOfAtesTwilightFound, 1)
    , (PendantOfAtesRalos, VB.pendantOfAtesRalosFound, 1)
    , (PendantOfAtesAldarin, VB.pendantOfAtesAldarinFound, 1)
    , (PharaohsSceptreNecropolis, VB.pharaohsSceptreNecropolis, 1)
    , (ColosseumWaveNine, VB.colosseumHighestWave, 9)
    , (RowboatVatrachos, VB.amenityRowboatVatrachos, 1)
    , (RowboatAnglers, VB.amenityRowboatAnglers, 1)
    , (RowboatSoulTear, VB.amenityRowboatSoulTear, 1)
    , (RowboatYnysdail, VB.amenityRowboatYnysdail, 1)
    , (RowboatBuccaneers, VB.amenityRowboatBuccaneers, 1)
    , (RespawnFalador, VB.faladorSpawn, 1)
    , (RespawnCamelot, VB.camelotSpawn, 1)
    , (RespawnEdgeville, VB.edgevilleSpawn, 1)
    , (RespawnFeroxEnclave, VB.wildernessSpawn, 1)
    , (RespawnKourendCastle, VB.kourendSpawn, 1)
    , (RespawnCivitasIllaFortis, VB.civitasSpawn, 1)
    ]

compileDiaryDerivedVarbits :: Map.Map Diary DiaryTier -> Map.Map VarbitId Int
compileDiaryDerivedVarbits diaries = Map.fromList
  [ (VB.atjunMedReward, unlocked Karamja Medium)
  , (VB.lumbridgeMedCount, unlocked LumbridgeDraynor Medium)
  ]
 where
  unlocked diary tier = if Map.findWithDefault NoDiary diary diaries >= tier then 1 else 0

compileDefaultVars :: CompiledVars
compileDefaultVars = CompiledVars (Map.fromList
  [ (VB.wildernessSwordLastTeleport, 0)
  , (VB.morytaniaLegsLastTeleport, 0)
  , (VB.yanilleTeleportLocation, 0)
  , (VB.lumbridgeCabbageTeleport, 0)
  , (VB.desertNardahTeleport, 0)
  , (VB.seersCamelotTeleport, 0)
  , (VB.seersSherlockTeleport, 0)
  , (VB.westernPiscTeleport, 0)
  , (VB.varrockGeTeleport, 0)
  , (VB.chinchompaTeleports, 0)
  , (VB.ardougneCloakLowbits, 0)
  , (VB.zeahBlessingWoodlandTeleport, 0)
  , (VB.zeahBlessingBrimstoneTeleport, 0)
  ]) (Map.fromList
  [ (VP.homeTeleportAnimToggles, 0)
  , (VP.aideTeleTimer, 0)
  ])

compileRuntimeVars :: Int -> RuntimeState -> CompiledVars
compileRuntimeVars now runtime = CompiledVars
  (Map.fromList
    [ (VB.spellbook, spellbookVarbit (runtimeSpellbook runtime))
    , (VB.pohTeleToggle, if runtimeArriveInsidePoh runtime then 0 else 1)
    , (VB.fremennikBasicTeleport, 0)
    ])
  (Map.singleton VP.slug2Regionuid (cooldownTimestamp now (runtimeMinigameTeleport runtime)))

cooldownTimestamp :: Int -> CooldownState -> Int
cooldownTimestamp now CooldownReady = now - 21
cooldownTimestamp _ (CooldownUsedAt timestamp) = timestamp

spellbookVarbit :: Spellbook -> Int
spellbookVarbit Standard = 0
spellbookVarbit Ancient = 1
spellbookVarbit Lunar = 2
spellbookVarbit Arceuus = 3

compilePohVars :: PohBuild -> CompiledVars
compilePohVars poh = CompiledVars
  (Map.singleton VB.pohHouseLocation (pohLocationVarbit (pohLocation poh)))
  Map.empty

pohLocationVarbit :: PohLocation -> Int
pohLocationVarbit = \case
  Rimmington -> 1
  Taverley -> 2
  Pollnivneach -> 3
  Rellekka -> 4
  Brimhaven -> 5
  Yanille -> 6
  Prifddinas -> 7
  Hosidius -> 8
  Aldarin -> 9

quetzalsUnlockedVarPlayer :: VarPlayerId
quetzalsUnlockedVarPlayer = VP.quetzalsUnlocked

mergeCompiledVars :: [CompiledVars] -> Either AccountCompileError CompiledVars
mergeCompiledVars = foldM merge (CompiledVars Map.empty Map.empty)
 where
  merge (CompiledVars bits players) (CompiledVars moreBits morePlayers) = do
    mergedBits <- Map.foldlWithKey' mergeBit (Right bits) moreBits
    mergedPlayers <- Map.foldlWithKey' mergePlayer (Right players) morePlayers
    pure (CompiledVars mergedBits mergedPlayers)
  mergeBit result key value = result >>= insertValue (ConflictingVarbit key) key value
  mergePlayer result key value = result >>= insertValue (ConflictingVarPlayer key) key value
  insertValue conflict key value values = case Map.lookup key values of
    Nothing -> Right (Map.insert key value values)
    Just old | old == value -> Right values
             | otherwise -> Left (conflict old value)

data DiaryVarbits = DiaryVarbits
  { diaryEasy :: VarbitId
  , diaryMedium :: VarbitId
  , diaryHard :: VarbitId
  , diaryElite :: VarbitId
  }

-- RuneLite Varbits.java: DIARY_*_EASY/MEDIUM/HARD/ELITE_COMPLETE.
diaryVarbitsFor :: Diary -> DiaryVarbits
diaryVarbitsFor = \case
  Ardougne -> DiaryVarbits (VarbitId 4458) (VarbitId 4459) (VarbitId 4460) (VarbitId 4461)
  Desert -> DiaryVarbits (VarbitId 4483) (VarbitId 4484) (VarbitId 4485) (VarbitId 4486)
  Falador -> DiaryVarbits (VarbitId 4462) (VarbitId 4463) (VarbitId 4464) (VarbitId 4465)
  Fremennik -> DiaryVarbits (VarbitId 4491) (VarbitId 4492) (VarbitId 4493) (VarbitId 4494)
  Kandarin -> DiaryVarbits (VarbitId 4475) (VarbitId 4476) (VarbitId 4477) (VarbitId 4478)
  Karamja -> DiaryVarbits (VarbitId 3578) (VarbitId 3599) (VarbitId 3611) (VarbitId 4566)
  KourendKebos -> DiaryVarbits (VarbitId 7925) (VarbitId 7926) (VarbitId 7927) (VarbitId 7928)
  LumbridgeDraynor -> DiaryVarbits (VarbitId 4495) (VarbitId 4496) (VarbitId 4497) (VarbitId 4498)
  Morytania -> DiaryVarbits (VarbitId 4487) (VarbitId 4488) (VarbitId 4489) (VarbitId 4490)
  Varrock -> DiaryVarbits (VarbitId 4479) (VarbitId 4480) (VarbitId 4481) (VarbitId 4482)
  WesternProvinces -> DiaryVarbits (VarbitId 4471) (VarbitId 4472) (VarbitId 4473) (VarbitId 4474)
  Wilderness -> DiaryVarbits (VarbitId 4466) (VarbitId 4467) (VarbitId 4468) (VarbitId 4469)

diaryVarbits :: Map.Map Diary DiaryTier -> Map.Map VarbitId Int
diaryVarbits = Map.foldlWithKey' addDiary Map.empty
 where
  addDiary values diary tier = values <> compileDiary tier (diaryVarbitsFor diary)

compileDiary :: DiaryTier -> DiaryVarbits -> Map.Map VarbitId Int
compileDiary tier vars = Map.fromList
  [ (diaryEasy vars, flag (tier >= Easy))
  , (diaryMedium vars, flag (tier >= Medium))
  , (diaryHard vars, flag (tier >= Hard))
  , (diaryElite vars, flag (tier >= Elite))
  ]
 where
  flag True = 1
  flag False = 0

quetzalPlatformMask :: Set.Set QuetzalPlatform -> Int
quetzalPlatformMask = Set.foldr ((.|.) . quetzalPlatformBit) 0

quetzalPlatformBit :: QuetzalPlatform -> Int
quetzalPlatformBit = \case
  CamTorum -> 32
  ColossalWyrmRemains -> 64
  OuterFortis -> 128
  FortisColosseum -> 256
  SalvagerOverlook -> 2048
  Kastori -> 16384
