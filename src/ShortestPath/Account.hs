module ShortestPath.Account
  ( ItemCounts
  , AccountBuild(..)
  , DiaryTier(..)
  , JewelleryBoxTier(..)
  , PohBuild(..)
  , RuntimeState(..)
  , ItemAccess(..)
  , RequirementMode(..)
  , RequirementContext(..)
  , RequirementFailure(..)
  , VarRequirementResult(..)
  , TransportAvailability(..)
  , emptyAccountBuild
  , availableItems
  , transportAvailability
  , requirementsSatisfied
  ) where

import Data.Bits ((.&.))
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import ShortestPath.Requirements
import ShortestPath.Transport (Transport(..))

type ItemCounts = Map.Map String Int

data AccountBuild = AccountBuild
  { accountLevels :: ItemCounts
  , accountCompletedQuests :: Set.Set String
  , accountVarbits :: Map.Map VarbitId Int
  , accountVarPlayers :: Map.Map VarPlayerId Int
  , accountInventory :: ItemCounts
  , accountEquipment :: ItemCounts
  , accountRunePouch :: ItemCounts
  , accountBank :: ItemCounts
  , accountDiaries :: Map.Map String DiaryTier
  , accountPoh :: PohBuild
  , accountFairyRingsUnlocked :: Bool
  , accountQuetzalPlatforms :: Set.Set Int
  , accountRuntime :: RuntimeState
  }
  deriving stock (Eq, Show)

data DiaryTier = NoDiary | Easy | Medium | Hard | Elite
  deriving stock (Eq, Ord, Show)

data JewelleryBoxTier = NoJewelleryBox | FancyJewelleryBox | OrnateJewelleryBox
  deriving stock (Eq, Ord, Show)

data PohBuild = PohBuild
  { pohLocation :: String
  , pohJewelleryBox :: JewelleryBoxTier
  , pohPortalDestinations :: Set.Set String
  , pohFairyRing :: Bool
  , pohSpiritTree :: Bool
  , pohObelisk :: Bool
  , pohMountedGlory :: Bool
  , pohMountedXerics :: Bool
  , pohMountedDigsite :: Bool
  , pohMountedMythical :: Bool
  }
  deriving stock (Eq, Show)

data RuntimeState = RuntimeState
  { runtimeSpellbook :: String
  , runtimeCooldownsReady :: Bool
  , runtimeArriveInsidePoh :: Bool
  }
  deriving stock (Eq, Show)

data ItemAccess = CarriedOnly | CarriedAndBank
  deriving stock (Eq, Ord, Show)

data RequirementMode = IgnoreRequirements | ConfiguredRequirements AccountBuild
  deriving stock (Eq, Show)

data RequirementContext = RequirementContext
  { requirementAccount :: AccountBuild
  , requirementItemAccess :: ItemAccess
  , requirementNowMinutes :: Int
  }
  deriving stock (Eq, Show)

data RequirementFailure
  = MissingItems ItemExpr
  | MissingSkills [SkillReq]
  | MissingQuests [String]
  | FailedVarRequirements [VarReq]
  | UnknownVarRequirements [VarReq]
  | MissingCapability String
  deriving stock (Eq, Show)

data TransportAvailability = Available | TransportTypeDisabled String | Unavailable [RequirementFailure]
  deriving stock (Eq, Show)

emptyAccountBuild :: AccountBuild
emptyAccountBuild =
  AccountBuild Map.empty Set.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty emptyPoh False Set.empty (RuntimeState "Standard" True True)

emptyPoh :: PohBuild
emptyPoh = PohBuild "Rimmington" NoJewelleryBox Set.empty False False False False False False False

availableItems :: AccountBuild -> ItemAccess -> ItemCounts
availableItems account access =
  foldr (Map.unionWith (+)) Map.empty sources
 where
  sources = [accountInventory account, accountEquipment account, accountRunePouch account] <> [accountBank account | access == CarriedAndBank]

transportAvailability :: RequirementContext -> Transport -> TransportAvailability
transportAvailability context transport =
  case failures of
    [] -> Available
    _ -> Unavailable failures
 where
  account = requirementAccount context
  failures =
    [ MissingItems expression | Just expression <- [items transport], not (itemExprSatisfied (availableItems account (requirementItemAccess context)) expression) ]
      <> [ MissingSkills missingSkills | not (null missingSkills) ]
      <> [ MissingQuests missingQuests | not (null missingQuests) ]
      <> [ FailedVarRequirements failedVars | not (null failedVars) ]
      <> [ UnknownVarRequirements unknownVars | not (null unknownVars) ]
      <> specialFailures context transport
  missingSkills = [requirement | requirement <- skills transport, Map.findWithDefault 0 (skillName requirement) (accountLevels account) < skillLevel requirement]
  missingQuests = filter (`Set.notMember` accountCompletedQuests account) (quests transport)
  variableRequirements = varbits transport <> varPlayers transport
  failedVars = [requirement | requirement <- variableRequirements, varRequirementResult context requirement == VarUnsatisfied]
  unknownVars = [requirement | requirement <- variableRequirements, varRequirementResult context requirement == VarUnknown]

specialFailures :: RequirementContext -> Transport -> [RequirementFailure]
specialFailures context transport =
  case transportType transport of
    "FAIRY_RING"
      | not (accountFairyRingsUnlocked account) -> [MissingCapability "Fairy rings are not unlocked"]
      | hasLumbridgeElite || hasItem "772" -> []
      | otherwise -> [MissingCapability "Fairy rings require a Dramen or Lunar staff"]
    "TELEPORTATION_BOX"
      | "Basic" `isInfixOf` displayInfo transport && pohJewelleryBox poh == NoJewelleryBox -> [MissingCapability "Basic jewellery box is not built"]
      | "Ornate" `isInfixOf` displayInfo transport && pohJewelleryBox poh < OrnateJewelleryBox -> [MissingCapability "Ornate jewellery box is not built"]
      | "Fancy" `isInfixOf` displayInfo transport && pohJewelleryBox poh < FancyJewelleryBox -> [MissingCapability "Fancy jewellery box is not built"]
      | otherwise -> []
    "TELEPORTATION_PORTAL_POH"
      | Set.member "*" (pohPortalDestinations poh) || Set.member (displayInfo transport) (pohPortalDestinations poh) -> []
      | otherwise -> [MissingCapability "POH portal is not built"]
    _ -> []
 where
  account = requirementAccount context
  poh = accountPoh account
  hasLumbridgeElite = Map.findWithDefault NoDiary "Lumbridge & Draynor" (accountDiaries account) >= Elite
  hasItem item = Map.findWithDefault 0 item (availableItems account (requirementItemAccess context)) > 0

requirementsSatisfied :: RequirementContext -> Transport -> Bool
requirementsSatisfied context transport = transportAvailability context transport == Available

itemExprSatisfied :: ItemCounts -> ItemExpr -> Bool
itemExprSatisfied counts expression =
  case expression of
    ItemOne (ItemTerm name quantity)
      | quantity <= 0 -> Map.findWithDefault 0 name counts <= 0
      | otherwise -> Map.findWithDefault 0 name counts >= quantity
    ItemAnd expressions -> all (itemExprSatisfied counts) expressions
    ItemOr expressions -> any (itemExprSatisfied counts) expressions

data VarRequirementResult = VarSatisfied | VarUnsatisfied | VarUnknown
  deriving stock (Eq, Ord, Show)

varRequirementResult :: RequirementContext -> VarReq -> VarRequirementResult
varRequirementResult context requirement =
  case value of
    Nothing -> VarUnknown
    Just actual
      | satisfies actual -> VarSatisfied
      | otherwise -> VarUnsatisfied
 where
  account = requirementAccount context
  value = case varRef requirement of
    GameVarbit identifier -> Map.lookup identifier (accountVarbits account)
    GameVarPlayer identifier -> Map.lookup identifier (accountVarPlayers account)
  satisfies actual = case varOp requirement of
    VarEq -> actual == varValue requirement
    VarGt -> actual > varValue requirement
    VarLt -> actual < varValue requirement
    VarMask -> actual .&. varValue requirement == varValue requirement
    VarCooldownMinutes -> actual + varValue requirement <= requirementNowMinutes context
