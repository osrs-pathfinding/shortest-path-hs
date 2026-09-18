{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module ShortestPath.BenchmarkProfiles
  ( BenchmarkProfiles(..)
  , benchmarkProfileNames
  , benchmarkAccount
  , benchmarkProfileVariableGaps
  , benchmarkNowMinutes
  , benchmarkProfileNamesFrom
  , benchmarkAccountFrom
  , benchmarkProfileVariableGapsFrom
  , discoverCorpusDir
  , loadBenchmarkProfiles
  ) where

import Data.Aeson (FromJSON(..), eitherDecodeFileStrict', withObject, (.:), (.:?))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

import ShortestPath.Account
import ShortestPath.AccountSemantics (compileItemReferences)
import ShortestPath.Requirements (VarbitId(..), VarPlayerId(..), VarReq)
import ShortestPath.Transport (Transport)

data BenchmarkProfiles = BenchmarkProfiles
  { benchmarkNowMinutesFrom :: Int
  , benchmarkAccounts :: Map.Map String AccountState
  }

data ProfileFile = ProfileFile Int Int (Map.Map String Profile)

data Profile = Profile
  { profileLevels :: Map.Map String Int
  , profileQuests :: [String]
  , profileVarbits :: Map.Map String Int
  , profileVarplayers :: Map.Map String Int
  , profileInventory :: Map.Map String Int
  , profileEquipment :: Map.Map String Int
  , profileRunePouch :: Map.Map String Int
  , profileBank :: Map.Map String Int
  , profileDiaries :: Map.Map String String
  , profilePoh :: PohProfile
  , profilePlantedSpiritTrees :: [String]
  , profileFairyRingsUnlocked :: Bool
  , profileRuntime :: RuntimeProfile
  }

data PohProfile = PohProfile
  { pohProfileLocation :: String
  , pohProfileJewelleryBox :: String
  , pohProfilePortals :: PortalProfile
  , pohProfileFairyRing :: Bool
  , pohProfileSpiritTree :: Bool
  , pohProfileObelisk :: Bool
  , pohProfileMountedGlory :: Bool
  , pohProfileMountedXerics :: Bool
  , pohProfileMountedDigsite :: Bool
  , pohProfileMountedMythical :: Bool
  }

data PortalProfile = PortalProfile String [String]
data RuntimeProfile = RuntimeProfile String CooldownProfile Bool
data CooldownProfile = CooldownProfile String (Maybe Int)

instance FromJSON ProfileFile where
  parseJSON = withObject "account profile file" $ \o ->
    ProfileFile <$> o .: "formatVersion" <*> o .: "benchmarkNowMinutes" <*> o .: "profiles"

instance FromJSON Profile where
  parseJSON = withObject "account profile" $ \o ->
    Profile <$> o .: "levels" <*> o .: "completedQuests" <*> o .: "varbits" <*> o .: "varplayers"
      <*> o .: "inventory" <*> o .: "equipment" <*> o .: "runePouch" <*> o .: "bank"
      <*> o .: "diaries" <*> o .: "poh" <*> o .: "plantedSpiritTrees"
      <*> o .: "fairyRingsUnlocked" <*> o .: "runtime"

instance FromJSON PohProfile where
  parseJSON = withObject "POH profile" $ \o -> do
    location <- o .: "location"
    jewellery <- o .: "jewelleryBox"
    portals <- o .: "portals"
    PohProfile location jewellery portals <$> o .: "fairyRing" <*> o .: "spiritTree"
      <*> o .: "obelisk" <*> o .: "mountedGlory" <*> o .: "mountedXerics"
      <*> o .: "mountedDigsite" <*> o .: "mountedMythical"

instance FromJSON PortalProfile where
  parseJSON = withObject "POH portal profile" $ \o -> PortalProfile <$> o .: "mode" <*> o .: "destinations"

instance FromJSON RuntimeProfile where
  parseJSON = withObject "runtime profile" $ \o -> RuntimeProfile <$> o .: "spellbook" <*> o .: "minigameTeleport" <*> o .: "arriveInsidePoh"

instance FromJSON CooldownProfile where
  parseJSON = withObject "cooldown profile" $ \o -> CooldownProfile <$> o .: "state" <*> o .:? "minutes"

benchmarkProfileNames :: [String]
benchmarkProfileNames = benchmarkProfileNamesFrom defaultBenchmarkProfiles

benchmarkAccount :: String -> [Transport] -> Maybe AccountState
benchmarkAccount name _ = benchmarkAccountFrom defaultBenchmarkProfiles name

benchmarkProfileVariableGaps :: [Transport] -> [(String, [VarReq])]
benchmarkProfileVariableGaps = benchmarkProfileVariableGapsFrom defaultBenchmarkProfiles

benchmarkNowMinutes :: Int
benchmarkNowMinutes = benchmarkNowMinutesFrom defaultBenchmarkProfiles

benchmarkProfileNamesFrom :: BenchmarkProfiles -> [String]
benchmarkProfileNamesFrom profiles = filter (`Map.member` benchmarkAccounts profiles) ["early", "mid", "end", "maxed"]

benchmarkAccountFrom :: BenchmarkProfiles -> String -> Maybe AccountState
benchmarkAccountFrom profiles name = Map.lookup name (benchmarkAccounts profiles)

benchmarkProfileVariableGapsFrom :: BenchmarkProfiles -> [Transport] -> [(String, [VarReq])]
benchmarkProfileVariableGapsFrom profiles transports =
  [ (name, Set.toList (Set.fromList unknown))
  | name <- benchmarkProfileNamesFrom profiles
  , Just account <- [benchmarkAccountFrom profiles name]
  , let context = RequirementContext account CarriedAndBank (benchmarkNowMinutesFrom profiles)
        unknown = concat
          [ requirements
          | transport <- transports
          , Unavailable failures <- [transportAvailability context transport]
          , UnknownVarRequirements requirements <- failures
          ]
  , not (null unknown)
  ]

discoverCorpusDir :: Maybe FilePath -> IO FilePath
discoverCorpusDir explicit = do
  environment <- lookupEnv "SHORTEST_PATH_CORPUS_DIR"
  choose (maybe [] pure explicit <> maybe [] pure environment <> ["../shortest-path-corpus"])
 where
  choose [] = fail "canonical benchmark corpus not found; pass --corpus-dir DIR or set SHORTEST_PATH_CORPUS_DIR"
  choose (candidate:rest) = do
    directory <- doesDirectoryExist candidate
    manifest <- doesFileExist (candidate </> "manifest.json")
    if directory && manifest then pure candidate else choose rest

loadBenchmarkProfiles :: FilePath -> IO BenchmarkProfiles
loadBenchmarkProfiles root = do
  let path = root </> "accounts/account-profiles-v1.json"
  decoded <- either fail pure =<< eitherDecodeFileStrict' path
  if formatVersion decoded /= 1
    then fail (path <> ": unsupported account profile formatVersion")
    else BenchmarkProfiles (profileNow decoded) <$> traverse compileProfile (profiles decoded)
 where
  compileProfile profile = do
    inventory <- compileItems "inventory" (profileInventory profile)
    equipment <- compileItems "equipment" (profileEquipment profile)
    runePouch <- compileItems "rune pouch" (profileRunePouch profile)
    bank <- compileItems "bank" (profileBank profile)
    varbits <- parseVars "varbit" VarbitId (profileVarbits profile)
    varplayers <- parseVars "varplayer" VarPlayerId (profileVarplayers profile)
    diaries <- traverse parseDiary (Map.toList (profileDiaries profile))
    poh <- parsePoh (profilePoh profile)
    planted <- traverse parseSpiritTree (profilePlantedSpiritTrees profile)
    runtime <- parseRuntime (profileRuntime profile)
    pure AccountState
      { accountLevels = profileLevels profile
      , accountCompletedQuests = Set.fromList (profileQuests profile)
      , accountVarbits = Map.fromList varbits
      , accountVarPlayers = Map.fromList varplayers
      , accountInventory = inventory
      , accountEquipment = equipment
      , accountRunePouch = runePouch
      , accountBank = bank
      , accountDiaries = Map.fromList diaries
      , accountPoh = poh
      , accountPlantedSpiritTrees = Set.fromList planted
      , accountFairyRingsUnlocked = profileFairyRingsUnlocked profile
      , accountRuntime = runtime
      }

  compileItems field values = either (fail . show) pure (compileItemReferences field values)

  parseVars field constructor values = traverse parseOne (Map.toList values)
   where
    parseOne (key, value) = maybe (fail (field <> " has invalid ID: " <> key)) (\identifier -> pure (constructor identifier, value)) (readMaybe key)

  parseDiary (name, tier) = do
    diary <- parseDiaryName name
    value <- parseDiaryTier tier
    pure (diary, value)

  parsePoh (PohProfile location jewellery (PortalProfile mode destinations) fairyRing spiritTree obelisk glory xerics digsite mythical) = do
    pohLocationValue <- parsePohLocation location
    jewelleryValue <- parseJewelleryBox jewellery
    portals <- case mode of
      "all" -> pure AllPohPortals
      "selected" -> pure (SelectedPohPortals (Set.fromList destinations))
      _ -> fail ("unknown POH portal mode: " <> mode)
    pure (PohBuild pohLocationValue jewelleryValue portals fairyRing spiritTree obelisk glory xerics digsite mythical)

  parseRuntime (RuntimeProfile spellbook (CooldownProfile state minutes) arriveInside) = do
    spellbookValue <- parseSpellbook spellbook
    cooldown <- case state of
      "ready" -> pure CooldownReady
      "usedAt" -> maybe (fail "usedAt cooldown has no minutes") (pure . CooldownUsedAt) minutes
      _ -> fail ("unknown cooldown state: " <> state)
    pure (RuntimeState spellbookValue cooldown arriveInside)

  parseSpiritTree name = parseSpiritTreeName (case name of
    "FARMING_GUILD" -> "FarmingGuildTree"
    "PORT_SARIM" -> "PortSarimTree"
    "ETCETERIA" -> "EtceteriaTree"
    "BRIMHAVEN" -> "BrimhavenTree"
    "HOSIDIUS" -> "HosidiusTree"
    other -> other)

  parseDiaryName = \case
    "Ardougne" -> pure Ardougne
    "Desert" -> pure Desert
    "Falador" -> pure Falador
    "Fremennik" -> pure Fremennik
    "Kandarin" -> pure Kandarin
    "Karamja" -> pure Karamja
    "KourendKebos" -> pure KourendKebos
    "LumbridgeDraynor" -> pure LumbridgeDraynor
    "Morytania" -> pure Morytania
    "Varrock" -> pure Varrock
    "WesternProvinces" -> pure WesternProvinces
    "Wilderness" -> pure Wilderness
    value -> fail ("unknown diary: " <> value)

  parseDiaryTier = \case
    "NoDiary" -> pure NoDiary
    "Easy" -> pure Easy
    "Medium" -> pure Medium
    "Hard" -> pure Hard
    "Elite" -> pure Elite
    value -> fail ("unknown diary tier: " <> value)

  parsePohLocation = \case
    "Rimmington" -> pure Rimmington
    "Taverley" -> pure Taverley
    "Pollnivneach" -> pure Pollnivneach
    "Rellekka" -> pure Rellekka
    "Brimhaven" -> pure Brimhaven
    "Yanille" -> pure Yanille
    "Prifddinas" -> pure Prifddinas
    "Hosidius" -> pure Hosidius
    "Aldarin" -> pure Aldarin
    value -> fail ("unknown POH location: " <> value)

  parseJewelleryBox = \case
    "NoJewelleryBox" -> pure NoJewelleryBox
    "FancyJewelleryBox" -> pure FancyJewelleryBox
    "OrnateJewelleryBox" -> pure OrnateJewelleryBox
    value -> fail ("unknown jewellery box: " <> value)

  parseSpellbook = \case
    "Standard" -> pure Standard
    "Ancient" -> pure Ancient
    "Lunar" -> pure Lunar
    "Arceuus" -> pure Arceuus
    value -> fail ("unknown spellbook: " <> value)

  parseSpiritTreeName = \case
    "FarmingGuildTree" -> pure FarmingGuildTree
    "PortSarimTree" -> pure PortSarimTree
    "EtceteriaTree" -> pure EtceteriaTree
    "BrimhavenTree" -> pure BrimhavenTree
    "HosidiusTree" -> pure HosidiusTree
    value -> fail ("unknown planted spirit tree: " <> value)

defaultBenchmarkProfiles :: BenchmarkProfiles
defaultBenchmarkProfiles = unsafePerformIO (discoverCorpusDir Nothing >>= loadBenchmarkProfiles)
{-# NOINLINE defaultBenchmarkProfiles #-}

formatVersion :: ProfileFile -> Int
formatVersion (ProfileFile value _ _) = value

profileNow :: ProfileFile -> Int
profileNow (ProfileFile _ value _) = value

profiles :: ProfileFile -> Map.Map String Profile
profiles (ProfileFile _ _ value) = value
