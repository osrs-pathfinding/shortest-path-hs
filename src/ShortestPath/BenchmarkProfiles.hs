module ShortestPath.BenchmarkProfiles
  ( benchmarkProfileNames
  , benchmarkAccountSpec
  , benchmarkAccount
  , benchmarkProfileVariableGaps
  , benchmarkNowMinutes
  ) where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import ShortestPath.Account
import ShortestPath.AccountSemantics
import ShortestPath.Requirements
import ShortestPath.Transport

benchmarkProfileNames :: [String]
benchmarkProfileNames = ["early", "mid", "end", "maxed"]

benchmarkAccountSpec :: String -> [Transport] -> Maybe AccountSpec
benchmarkAccountSpec name transports = case name of
  "early" -> Just earlySpec
  "mid" -> Just midSpec
  "end" -> Just endSpec
  "maxed" -> Just (maxedSpec allItems)
  _ -> Nothing
 where
  allItems = Map.fromList
    [(item, 1000) | transport <- transports, item <- itemNames =<< maybeToList (items transport)]

benchmarkAccount :: String -> [Transport] -> Maybe AccountState
benchmarkAccount name transports = do
  accountSpec <- benchmarkAccountSpec name transports
  either (error . ("benchmark account failed to compile: " <>) . show) Just
    (compileAccount benchmarkNowMinutes accountSpec)

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

benchmarkNowMinutes :: Int
benchmarkNowMinutes = 100000000

earlySpec, midSpec, endSpec :: AccountSpec
earlySpec = spec (progression earlyLevels earlyQuests (allDiaries Medium) Set.empty Set.empty Set.empty earlyPlantedSpiritTrees earlyPermanentUnlocks) basicPoh earlyLoadout earlyBank
midSpec = spec (progression midLevels canonicalQuestUniverse (allDiaries Hard) allPlatforms allBalloonDestinations allCatacombsEntrances midPlantedSpiritTrees allPermanentUnlocks) midPoh midLoadout midBank
endSpec = spec (progression (Map.insert "Quest" 327 endLevels) canonicalQuestUniverse endDiaries allPlatforms allBalloonDestinations allCatacombsEntrances endPlantedSpiritTrees allPermanentUnlocks) maxedPoh endLoadout endBank

maxedSpec :: ItemReferences -> AccountSpec
maxedSpec allItems =
  spec
    (progression
      (Map.fromList [(skill, 99) | skill <- allSkills] <> Map.fromList [("Quest", 327), ("Total", 2376)])
      canonicalQuestUniverse
      (allDiaries Elite)
      allPlatforms
      allBalloonDestinations
      allCatacombsEntrances
      maxedPlantedSpiritTrees
      allPermanentUnlocks)
    maxedPoh
    maxedLoadout
    (allItems <> endBank)

spec :: Progression -> PohBuild -> ItemLoadout -> ItemReferences -> AccountSpec
spec progress poh carried bank =
  AccountSpec progress emptyRawGameState poh carried bank standardRuntime

progression :: SkillLevels -> Set.Set String -> Map.Map Diary DiaryTier -> Set.Set QuetzalPlatform -> Set.Set HotAirBalloonDestination -> Set.Set CatacombsEntrance -> Set.Set PlantedSpiritTree -> Set.Set PermanentUnlock -> Progression
progression skillLevels quests diaries platforms balloons catacombs plantedSpiritTrees unlocks =
  Progression skillLevels (Set.insert "Dragon Slayer I" quests) Set.empty diaries True plantedSpiritTrees platforms balloons catacombs unlocks

emptyRawGameState :: RawGameState
emptyRawGameState = RawGameState Map.empty Map.empty

allDiaries :: DiaryTier -> Map.Map Diary DiaryTier
allDiaries tier = Map.fromList [(diary, tier) | diary <- [minBound .. maxBound]]

endDiaries :: Map.Map Diary DiaryTier
endDiaries = Map.insert LumbridgeDraynor Elite (allDiaries Hard)

allPlatforms :: Set.Set QuetzalPlatform
allPlatforms = Set.fromList [minBound .. maxBound]

allBalloonDestinations :: Set.Set HotAirBalloonDestination
allBalloonDestinations = Set.fromList [minBound .. maxBound]

allCatacombsEntrances :: Set.Set CatacombsEntrance
allCatacombsEntrances = Set.fromList [minBound .. maxBound]

allPermanentUnlocks :: Set.Set PermanentUnlock
allPermanentUnlocks = Set.fromList [minBound .. maxBound]

earlyPermanentUnlocks :: Set.Set PermanentUnlock
earlyPermanentUnlocks = Set.singleton CorsairCoveResourceArea

earlyPlantedSpiritTrees, midPlantedSpiritTrees, endPlantedSpiritTrees, maxedPlantedSpiritTrees :: Set.Set PlantedSpiritTree
earlyPlantedSpiritTrees = Set.empty
midPlantedSpiritTrees = Set.singleton FarmingGuildTree
endPlantedSpiritTrees = Set.fromList [FarmingGuildTree, PortSarimTree]
maxedPlantedSpiritTrees = allPlayerPlantedSpiritTrees

canonicalQuestUniverse :: Set.Set String
canonicalQuestUniverse = earlyQuests <> Set.fromList
  [ "Land of the Goblins", "Sins of the Father", "Dragon Slayer I"
  , "Making Friends with My Arm", "Cabin Fever", "The Depths of Despair"
  , "Zogre Flesh Eaters", "The Path of Glouphrie", "Troubled Tortugans"
  , "Song of the Elves", "The Forsaken Tower", "Enlightened Journey"
  , "Legends' Quest", "Shilo Village", "Waterfall Quest", "Darkness of Hallowvale"
  , "Fishing Contest", "Mountain Daughter", "Between a Rock...", "The Golem"
  , "Icthlarin's Little Helper", "Tears of Guthix", "The Lost Tribe"
  , "Swan Song", "The Fremennik Isles", "Beneath Cursed Sands"
  , "Tree Gnome Village", "The Grand Tree", "Plague City", "Watchtower"
  , "Regicide", "Throne of Miscellania", "Mourning's End Part I"
  , "The Queen of Thieves", "The Tale of the Righteous", "Architectural Alliance"
  ]

allSkills :: [String]
allSkills = ["Attack", "Strength", "Defence", "Hitpoints", "Ranged", "Prayer", "Magic", "Agility", "Herblore", "Thieving", "Crafting", "Fletching", "Slayer", "Hunter", "Mining", "Smithing", "Fishing", "Cooking", "Firemaking", "Woodcutting", "Farming", "Runecraft", "Construction", "Sailing"]

levels :: [Int] -> SkillLevels
levels values = Map.fromList (zip allSkills values)

earlyLevels, midLevels, endLevels :: SkillLevels
earlyLevels = levels [70, 75, 70, 75, 70, 60, 70, 70, 65, 65, 65, 65, 65, 65, 65, 65, 65, 70, 70, 65, 65, 60, 60, 60]
midLevels = levels [80, 85, 80, 85, 80, 70, 85, 80, 78, 82, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 83, 75, 78, 75]
endLevels = levels [90, 95, 90, 95, 90, 85, 94, 90, 90, 91, 90, 90, 95, 90, 85, 91, 90, 95, 90, 90, 91, 85, 85, 85]

earlyQuests :: Set.Set String
earlyQuests = Set.fromList ["Another Slice of H.A.M.", "Biohazard", "Bone Voyage", "Children of the Sun", "Client of Kourend", "Creature of Fenkenstrain", "Death to the Dorgeshuun", "Enter the Abyss", "Garden of Tranquillity", "Haunted Mine", "Holy Grail", "In Search of the Myreque", "Lost City", "Monkey Madness I", "Nature Spirit", "Observatory Quest", "Plague City", "Priest in Peril", "Regicide", "Sea Slug", "Shades of Mort'ton", "Tai Bwo Wannai Trio", "The Corsair Curse", "The Fremennik Trials", "The Giant Dwarf", "The Grand Tree", "The Lost Tribe", "Tree Gnome Village", "Twilight's Promise", "Watchtower"]

basicPoh, midPoh, maxedPoh :: PohBuild
basicPoh = PohBuild Rimmington NoJewelleryBox (SelectedPohPortals Set.empty) False False False False False False False
midPoh = PohBuild Rimmington FancyJewelleryBox (SelectedPohPortals (Set.fromList ["Varrock Portal", "Falador Portal", "Camelot Portal", "Ardougne Portal", "Kourend Portal", "Barrows Portal"])) False False False True True True True
maxedPoh = PohBuild Rimmington OrnateJewelleryBox AllPohPortals True True True True True True True

standardRuntime :: RuntimeState
standardRuntime = RuntimeState Standard CooldownReady True

earlyLoadout, midLoadout, endLoadout, maxedLoadout :: ItemLoadout
earlyLoadout = ItemLoadout (Map.fromList [("772", 1), ("2552", 1), ("3853", 1), ("1704", 1), ("8013", 1), ("995", 100000)]) Map.empty standardRunes
midLoadout = ItemLoadout (Map.fromList [("772", 1), ("2552", 1), ("1704", 1), ("3853", 1), ("11866", 1), ("11190", 1), ("8013", 1), ("29271", 1), ("995", 1000000)]) Map.empty standardRunes
endLoadout = ItemLoadout (Map.fromList [("9813", 1), ("2552", 1), ("1704", 1), ("8013", 1), ("995", 5000000)]) Map.empty standardRunes
maxedLoadout = ItemLoadout (Map.fromList [("13280", 1), ("13069", 1), ("8013", 1), ("995", 10000000)]) Map.empty standardRunes

standardRunes :: ItemReferences
standardRunes = Map.fromList [("554", 10000), ("555", 10000), ("556", 10000), ("563", 10000)]

earlyBank :: ItemReferences
earlyBank = itemBank ["772", "2552", "3853", "1704", "11118", "11105", "21146", "11980", "8013", "995"]

midBank, endBank :: ItemReferences
midBank = itemBank
  [ "2552", "3853", "11978", "11968", "11972", "11194", "11866", "11980", "21146", "21166"
  , "8007", "8008", "8009", "8010", "8011", "8012", "8013", "12402", "12403", "12404", "12406", "12407", "12409", "12410", "12938"
  , "772", "4251", "6707", "13393", "13660", "19564", "21389", "21760", "22400", "22599", "22601", "23946", "25818", "29273", "29893", "32399"
  , "11140", "13110", "13114", "13123", "13127", "13131", "13135", "13139", "13143", "22945"
  , "AIR_RUNE", "WATER_RUNE", "EARTH_RUNE", "FIRE_RUNE", "LAW_RUNE", "NATURE_RUNE", "COINS", "AXE", "PICKAXE", "ROPE", "MACHETE", "SHANTAY_PASS", "CROSSBOW", "MITH_GRAPPLE"
  ]
endBank = itemBank
  [ "2552", "3853", "11978", "11968", "11972", "11194", "11866", "11980", "21146", "21166"
  , "8007", "8008", "8009", "8010", "8011", "8012", "8013", "12402", "12403", "12404", "12405", "12406", "12407", "12408", "12409", "12410", "12411", "12642", "12938"
  , "772", "4251", "6707", "13393", "13660", "19564", "21268", "21389", "21760", "22400", "22599", "22601", "23458", "23946", "25818", "26818", "26948", "28327", "29275", "29893", "32399", "33104"
  , "13103", "13111", "13115", "13124", "13128", "13132", "13136", "13140", "13144", "22947"
  , "AIR_RUNE", "WATER_RUNE", "EARTH_RUNE", "FIRE_RUNE", "LAW_RUNE", "NATURE_RUNE", "COINS", "AXE", "PICKAXE", "ROPE", "MACHETE", "SHANTAY_PASS", "CROSSBOW", "MITH_GRAPPLE"
  ]

itemBank :: [String] -> ItemReferences
itemBank itemIds = Map.fromList [(item, 1000) | item <- itemIds]

itemNames :: ItemExpr -> [String]
itemNames expression = case expression of
  ItemOne term -> [itemName term]
  ItemAnd terms -> concatMap itemNames terms
  ItemOr terms -> concatMap itemNames terms

maybeToList :: Maybe a -> [a]
maybeToList = maybe [] pure
