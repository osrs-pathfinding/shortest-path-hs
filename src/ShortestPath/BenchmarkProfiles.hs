module ShortestPath.BenchmarkProfiles
  ( BenchmarkProfile(..)
  , Progression(..)
  , Diary(..)
  , QuestMilestone(..)
  , QuetzalPlatform(..)
  , CompiledVars(..)
  , GameStateSpec(..)
  , ItemLoadout(..)
  , benchmarkProfileNames
  , benchmarkAccount
  , benchmarkProfileVariableGaps
  , benchmarkNowMinutes
  , compileProgressionVars
  , effectiveQuestMilestones
  , diaryVarbitsFor
  , compileDiary
  , quetzalPlatformBit
  , quetzalPlatformMask
  ) where

import qualified Data.Map.Strict as Map
import Data.List (nub)
import qualified Data.Set as Set
import Data.Bits ((.|.))

import ShortestPath.Account
import qualified ShortestPath.GameVars.Varbits as VB
import qualified ShortestPath.GameVars.VarPlayers as VP
import ShortestPath.Requirements
import ShortestPath.Transport

data QuetzalPlatform
  = CamTorum
  | ColossalWyrmRemains
  | OuterFortis
  | FortisColosseum
  | SalvagerOverlook
  | Kastori
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Progression = Progression
  { progressionLevels :: ItemCounts
  , progressionQuests :: Set.Set String
  , progressionMilestones :: Set.Set QuestMilestone
  , progressionDiaries :: Map.Map Diary DiaryTier
  , progressionFairyRings :: Bool
  , progressionQuetzalPlatforms :: Set.Set QuetzalPlatform
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

data GameStateSpec = GameStateSpec
  { gameStateVarbits :: Map.Map VarbitId Int
  , gameStateVarPlayers :: Map.Map VarPlayerId Int
  }
  deriving stock (Eq, Show)

data CompiledVars = CompiledVars
  { compiledVarbits :: Map.Map VarbitId Int
  , compiledVarPlayers :: Map.Map VarPlayerId Int
  }
  deriving stock (Eq, Show)

data BenchmarkProfile = BenchmarkProfile
  { profileName :: String
  , profileProgression :: Progression
  , profileGameState :: GameStateSpec
  , profilePoh :: PohBuild
  , profileCarried :: ItemLoadout
  , profileBank :: ItemCounts
  , profileRuntime :: RuntimeState
  }
  deriving stock (Eq, Show)

benchmarkProfileNames :: [String]
benchmarkProfileNames = ["early", "mid", "end", "maxed"]

benchmarkAccount :: String -> [Transport] -> Maybe AccountBuild
benchmarkAccount name transports = compile <$> profile
 where
  profile = case name of
    "early" -> Just earlyProfile
    "mid" -> Just (midProfile allQuests)
    "end" -> Just (endProfile allQuests)
    "maxed" -> Just (maxedProfile allQuests allItems)
    _ -> Nothing
  allQuests = canonicalQuestUniverse
  allItems = Map.fromList [(item, 1000) | transport <- transports, item <- itemNames =<< maybeToList (items transport)]
  compile value =
    emptyAccountBuild
      { accountLevels = progressionLevels progress
      , accountCompletedQuests = progressionQuests progress
      , accountVarbits = compiledVarbits compiled
      , accountVarPlayers = compiledVarPlayers compiled
      , accountInventory = loadoutInventory loadout
      , accountEquipment = loadoutEquipment loadout
      , accountRunePouch = loadoutRunePouch loadout
      , accountBank = profileBank value
      , accountDiaries = Map.fromList
          [(diaryName diary, tier) | (diary, tier) <- Map.toList (progressionDiaries progress)]
      , accountPoh = profilePoh value
      , accountFairyRingsUnlocked = progressionFairyRings progress
      , accountRuntime = profileRuntime value
      }
   where
    progress = profileProgression value
    compiled = mergeCompiledVars
      [ CompiledVars (gameStateVarbits (profileGameState value)) (gameStateVarPlayers (profileGameState value))
      , compileProgressionVars progress
      , compileRuntimeVars benchmarkNowMinutes (profileRuntime value)
      , compilePohVars (profilePoh value)
      ]
    loadout = profileCarried value

benchmarkProfileVariableGaps :: [Transport] -> [(String, [VarReq])]
benchmarkProfileVariableGaps transports =
  [ (name, nub unknown)
  | name <- benchmarkProfileNames
  , Just account <- [benchmarkAccount name transports]
  , let context = RequirementContext account CarriedAndBank benchmarkNowMinutes
        unknown = concat
          [ requirements
          | transport <- transports
          , Unavailable failures <- [transportAvailability context transport]
          , UnknownVarRequirements requirements <- failures
          ]
  , not (null unknown)
  ]

earlyProfile :: BenchmarkProfile
earlyProfile = BenchmarkProfile "early" (progression earlyLevels (withCoreQuests earlyQuests) (allDiaries Medium) True Set.empty) emptyGameState basicPoh earlyLoadout earlyBank standardRuntime

midProfile :: Set.Set String -> BenchmarkProfile
midProfile allQuests = BenchmarkProfile "mid" (progression midLevels (withCoreQuests allQuests) (allDiaries Hard) True allPlatforms) emptyGameState midPoh midLoadout midBank standardRuntime

endProfile :: Set.Set String -> BenchmarkProfile
endProfile allQuests = BenchmarkProfile "end" (progression (Map.insert "Quest" 327 endLevels) (withCoreQuests allQuests) endDiaries True allPlatforms) emptyGameState maxedPoh endLoadout endBank standardRuntime

maxedProfile :: Set.Set String -> ItemCounts -> BenchmarkProfile
maxedProfile allQuests allItems = BenchmarkProfile "maxed" (progression (Map.fromList [(skill, 99) | skill <- allSkills] <> Map.fromList [("Quest", 327), ("Total", 2376)]) (withCoreQuests allQuests) (allDiaries Elite) True allPlatforms) emptyGameState maxedPoh maxedLoadout (allItems <> endBank) standardRuntime

withCoreQuests :: Set.Set String -> Set.Set String
withCoreQuests = Set.insert "Dragon Slayer I"

progression :: ItemCounts -> Set.Set String -> Map.Map Diary DiaryTier -> Bool -> Set.Set QuetzalPlatform -> Progression
progression levels quests diaries fairy platforms =
  Progression levels quests Set.empty diaries fairy platforms

allDiaries :: DiaryTier -> Map.Map Diary DiaryTier
allDiaries tier = Map.fromList [(diary, tier) | diary <- [minBound .. maxBound]]

endDiaries :: Map.Map Diary DiaryTier
endDiaries = Map.insert LumbridgeDraynor Elite (allDiaries Hard)

diaryName :: Diary -> String
diaryName = \case
  Ardougne -> "Ardougne"; Desert -> "Desert"; Falador -> "Falador"
  Fremennik -> "Fremennik"; Kandarin -> "Kandarin"; Karamja -> "Karamja"
  KourendKebos -> "Kourend & Kebos"; LumbridgeDraynor -> "Lumbridge & Draynor"
  Morytania -> "Morytania"; Varrock -> "Varrock"
  WesternProvinces -> "Western Provinces"; Wilderness -> "Wilderness"

canonicalQuestUniverse :: Set.Set String
canonicalQuestUniverse = earlyQuests <> Set.fromList
  [ "Land of the Goblins", "Sins of the Father", "Dragon Slayer I"
  , "Making Friends with My Arm", "Cabin Fever", "The Depths of Despair"
  , "Zogre Flesh Eaters", "The Path of Glouphrie"
  ]

effectiveQuestMilestones :: Progression -> Set.Set QuestMilestone
effectiveQuestMilestones progress = progressionMilestones progress <> Set.fromList
  [ LandOfTheGoblinsYuBiuskAccess | Set.member "Land of the Goblins" quests ]
  <> Set.fromList
  [ SinsOfTheFatherSlepeBoatAccess | Set.member "Sins of the Father" quests ]
 where
  quests = progressionQuests progress

benchmarkNowMinutes :: Int
benchmarkNowMinutes = 100000000

emptyGameState :: GameStateSpec
emptyGameState = GameStateSpec Map.empty Map.empty

instance Semigroup GameStateSpec where
  GameStateSpec bits players <> GameStateSpec moreBits morePlayers =
    GameStateSpec (bits <> moreBits) (players <> morePlayers)

instance Monoid GameStateSpec where
  mempty = emptyGameState

compileProgressionVars :: Progression -> CompiledVars
compileProgressionVars progress = mergeCompiledVars
  [ CompiledVars (diaryVarbits (progressionDiaries progress)) Map.empty
  , CompiledVars (compileQuestDerivedVarbits progress) Map.empty
  , CompiledVars Map.empty (compileQuetzalVars (progressionQuetzalPlatforms progress))
  , compileDefaultVars
  ]

compileQuestDerivedVarbits :: Progression -> Map.Map VarbitId Int
compileQuestDerivedVarbits progress =
  Map.insert VB.my2armStatus (if Set.member "Making Friends with My Arm" quests then makingFriendsCompleteValue else 0)
    (Map.insert VB.thzfeBlockingBarricade (if Set.member "Zogre Flesh Eaters" quests then thzfeBlockingBarricadeValue else 0)
      (Map.insert VB.hosidiusquest (if Set.member "The Depths of Despair" quests then depthsOfDespairCaveAccessValue else 0)
        (Map.insert VB.myq5 (if SinsOfTheFatherSlepeBoatAccess `Set.member` milestones then myq5BoatUnlockedValue else 0)
          (Map.insert VB.lotg (if LandOfTheGoblinsYuBiuskAccess `Set.member` milestones then lotgYuBiuskUnlockedValue else 0)
            (if Set.member "Dragon Slayer I" quests
              then Map.singleton VB.dragonslayerCrandorFoundSecretDoor 1
              else Map.empty)))))
 where
  quests = progressionQuests progress
  milestones = effectiveQuestMilestones progress

-- Benchmark semantic fallback values for the quest states required by GPS.
lotgYuBiuskUnlockedValue, myq5BoatUnlockedValue, makingFriendsCompleteValue, depthsOfDespairCaveAccessValue, thzfeBlockingBarricadeValue :: Int
lotgYuBiuskUnlockedValue = 50
myq5BoatUnlockedValue = 88
makingFriendsCompleteValue = 207
depthsOfDespairCaveAccessValue = 7
thzfeBlockingBarricadeValue = 1

compileQuetzalVars :: Set.Set QuetzalPlatform -> Map.Map VarPlayerId Int
compileQuetzalVars platforms = Map.singleton quetzalsUnlockedVarPlayer (quetzalPlatformMask platforms)

compileDefaultVars :: CompiledVars
compileDefaultVars = CompiledVars Map.empty (Map.fromList
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

spellbookVarbit :: String -> Int
spellbookVarbit "Standard" = 0
spellbookVarbit name = error ("unsupported spellbook: " <> name)

compilePohVars :: PohBuild -> CompiledVars
compilePohVars poh = CompiledVars
  (Map.singleton VB.pohHouseLocation (pohLocationVarbit (pohLocation poh)))
  Map.empty

pohLocationVarbit :: String -> Int
pohLocationVarbit = \case
  "Rimmington" -> 1
  "Taverly" -> 2
  "Pollnivneach" -> 3
  "Rellekka" -> 4
  "Brimhaven" -> 5
  "Yanille" -> 6
  "Prifddinas" -> 7
  "Hosidius" -> 8
  "Aldarin" -> 9
  name -> error ("unsupported POH location: " <> name)

quetzalsUnlockedVarPlayer :: VarPlayerId
quetzalsUnlockedVarPlayer = VP.quetzalsUnlocked

mergeCompiledVars :: [CompiledVars] -> CompiledVars
mergeCompiledVars = foldl' merge (CompiledVars Map.empty Map.empty)
 where
  merge (CompiledVars bits players) (CompiledVars moreBits morePlayers) =
    CompiledVars (mergeMap "Varbit" bits moreBits) (mergeMap "VarPlayer" players morePlayers)

  mergeMap namespace = Map.foldlWithKey' insertValue
   where
    insertValue values key value = case Map.lookup key values of
      Nothing -> Map.insert key value values
      Just old
        | old == value -> values
        | otherwise -> error (namespace <> " " <> show key <> " compiled with conflicting values " <> show old <> " and " <> show value)

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

allPlatforms :: Set.Set QuetzalPlatform
allPlatforms = Set.fromList [minBound .. maxBound]

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

itemNames :: ItemExpr -> [String]
itemNames expression = case expression of
  ItemOne term -> [itemName term]
  ItemAnd terms -> concatMap itemNames terms
  ItemOr terms -> concatMap itemNames terms

maybeToList :: Maybe a -> [a]
maybeToList = maybe [] pure

allSkills :: [String]
allSkills = ["Attack", "Strength", "Defence", "Hitpoints", "Ranged", "Prayer", "Magic", "Agility", "Herblore", "Thieving", "Crafting", "Fletching", "Slayer", "Hunter", "Mining", "Smithing", "Fishing", "Cooking", "Firemaking", "Woodcutting", "Farming", "Runecraft", "Construction", "Sailing"]

levels :: [Int] -> ItemCounts
levels values = Map.fromList (zip allSkills values)

earlyLevels, midLevels, endLevels :: ItemCounts
earlyLevels = levels [70, 75, 70, 75, 70, 60, 70, 70, 65, 65, 65, 65, 65, 65, 65, 65, 65, 70, 70, 65, 65, 60, 60, 60]
midLevels = levels [80, 85, 80, 85, 80, 70, 85, 80, 78, 82, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 83, 75, 78, 75]
endLevels = levels [90, 95, 90, 95, 90, 85, 94, 90, 90, 91, 90, 90, 95, 90, 85, 91, 90, 95, 90, 90, 91, 85, 85, 85]

earlyQuests :: Set.Set String
earlyQuests = Set.fromList ["Another Slice of H.A.M.", "Biohazard", "Bone Voyage", "Children of the Sun", "Client of Kourend", "Creature of Fenkenstrain", "Death to the Dorgeshuun", "Enter the Abyss", "Garden of Tranquillity", "Haunted Mine", "Holy Grail", "In Search of the Myreque", "Lost City", "Monkey Madness I", "Nature Spirit", "Observatory Quest", "Plague City", "Priest in Peril", "Regicide", "Sea Slug", "Shades of Mort'ton", "Tai Bwo Wannai Trio", "The Corsair Curse", "The Fremennik Trials", "The Giant Dwarf", "The Grand Tree", "The Lost Tribe", "Tree Gnome Village", "Twilight's Promise", "Watchtower"]

basicPoh, midPoh, maxedPoh :: PohBuild
basicPoh = PohBuild "Rimmington" NoJewelleryBox Set.empty False False False False False False False
midPoh = PohBuild "Rimmington" FancyJewelleryBox (Set.fromList ["Varrock Portal", "Falador Portal", "Camelot Portal", "Ardougne Portal", "Kourend Portal", "Barrows Portal"]) False False False True True True True
maxedPoh = PohBuild "Rimmington" OrnateJewelleryBox (Set.singleton "*") True True True True True True True

standardRuntime :: RuntimeState
standardRuntime = RuntimeState "Standard" CooldownReady True

earlyLoadout, midLoadout, endLoadout, maxedLoadout :: ItemLoadout
earlyLoadout = ItemLoadout (Map.fromList [("772", 1), ("2552", 1), ("3853", 1), ("1704", 1), ("8013", 1), ("995", 100000)]) Map.empty standardRunes
midLoadout = ItemLoadout (Map.fromList [("772", 1), ("2552", 1), ("1704", 1), ("3853", 1), ("11866", 1), ("11190", 1), ("8013", 1), ("29271", 1), ("995", 1000000)]) Map.empty standardRunes
endLoadout = ItemLoadout (Map.fromList [("9813", 1), ("2552", 1), ("1704", 1), ("8013", 1), ("995", 5000000)]) Map.empty standardRunes
maxedLoadout = ItemLoadout (Map.fromList [("13280", 1), ("13069", 1), ("8013", 1), ("995", 10000000)]) Map.empty standardRunes

standardRunes :: ItemCounts
standardRunes = Map.fromList [("554", 10000), ("555", 10000), ("556", 10000), ("563", 10000)]

earlyBank :: ItemCounts
earlyBank = itemBank ["772", "2552", "3853", "1704", "11118", "11105", "21146", "11980", "8013", "995"]

midBank :: ItemCounts
midBank = itemBank
  [ -- Charged jewellery.
    "2552", "3853", "11978", "11968", "11972", "11194", "11866", "11980", "21146", "21166"
  , -- Standard spellbook tablets and commonly stocked teleport scrolls.
    "8007", "8008", "8009", "8010", "8011", "8012", "8013"
  , "12402", "12403", "12404", "12406", "12407", "12409", "12410", "12938"
  , -- Reusable quest and travel unlocks.
    "772", "4251", "6707", "13393", "13660", "19564", "21389", "21760", "22400"
  , "22599", "22601", "23946", "25818", "29273", "29893", "32399"
  , -- Hard diary equipment with useful travel actions.
    "11140", "13110", "13114", "13123", "13127", "13131", "13135", "13139", "13143", "22945"
  , -- Common rune and overland-transport requirements.
    "AIR_RUNE", "WATER_RUNE", "EARTH_RUNE", "FIRE_RUNE", "LAW_RUNE", "NATURE_RUNE"
  , "COINS", "AXE", "PICKAXE", "ROPE", "MACHETE", "SHANTAY_PASS", "CROSSBOW", "MITH_GRAPPLE"
  ]

endBank :: ItemCounts
endBank = itemBank
  [ -- Mid-game staples retained at their best common charge.
    "2552", "3853", "11978", "11968", "11972", "11194", "11866", "11980", "21146", "21166"
  , "8007", "8008", "8009", "8010", "8011", "8012", "8013"
  , "12402", "12403", "12404", "12405", "12406", "12407", "12408", "12409", "12410", "12411", "12642", "12938"
  , -- Late-game reusable and earned convenience teleports.
    "772", "4251", "6707", "13393", "13660", "19564", "21268", "21389", "21760", "22400"
  , "22599", "22601", "23458", "23946", "25818", "26818", "26948", "28327", "29275", "29893", "32399", "33104"
  , -- Elite diary equipment.
    "13103", "13111", "13115", "13124", "13128", "13132", "13136", "13140", "13144", "22947"
  , "AIR_RUNE", "WATER_RUNE", "EARTH_RUNE", "FIRE_RUNE", "LAW_RUNE", "NATURE_RUNE"
  , "COINS", "AXE", "PICKAXE", "ROPE", "MACHETE", "SHANTAY_PASS", "CROSSBOW", "MITH_GRAPPLE"
  ]

itemBank :: [String] -> ItemCounts
itemBank items = Map.fromList [(item, 1000) | item <- items]
