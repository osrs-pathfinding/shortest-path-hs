module Main (main) where

import Data.List (find, isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import ShortestPath.Account
import ShortestPath.BenchmarkProfiles (benchmarkAccount, benchmarkNowMinutes)
import ShortestPath.Transport
import ShortestPath.Tile (packTile)

main :: IO ()
main = do
  transports <- loadTransports defaultSourcePaths
  let context account = RequirementContext account CarriedOnly benchmarkNowMinutes
      available account = requirementsSatisfied (context account)
      mounted name = must name (find (\transport -> transportType transport == "TELEPORTATION_BOX" && name `isInfixOf` objectInfo transport) transports)
      plantedDestination name tile = must name (find (\transport -> transportType transport == "SPIRIT_TREE" && destination transport == Just tile) transports)
      farmingGuildTree = plantedDestination "Farming Guild spirit tree" (packTile 1251 3750 0)
      portSarimTree = plantedDestination "Port Sarim spirit tree" (packTile 3058 3257 0)
      permanentTree = must "Tree Gnome Village spirit tree" (find (\transport -> transportType transport == "SPIRIT_TREE" && origin transport == Just (packTile 2543 3167 0) && destination transport == Just (packTile 3185 3508 0)) transports)
      pohTree = must "POH spirit tree" (find (\transport -> transportType transport == "SPIRIT_TREE" && maybe False isInsidePoh (origin transport) && destination transport == Just (packTile 2542 3170 0)) transports)
      basicBox = mounted "Basic Jewellery Box"
      ornateBox = mounted "Ornate Jewellery Box"
      xerics = mounted "Xeric's Talisman"
      carriedXerics = must "carried Xeric's talisman" (find (\transport -> transportType transport == "TELEPORTATION_ITEM" && "Xeric's talisman:" `isInfixOf` displayInfo transport) transports)
      gates =
        [ (mounted "Amulet of Glory", \value poh -> poh {pohMountedGlory = value})
        , (xerics, \value poh -> poh {pohMountedXerics = value})
        , (mounted "Digsite Pendant", \value poh -> poh {pohMountedDigsite = value})
        , (mounted "Mythical cape", \value poh -> poh {pohMountedMythical = value})
        ]
      base = emptyAccountState
      enable gate = base {accountPoh = gate True (accountPoh base)}
      mixed = enable (\value poh -> poh {pohMountedXerics = value})
      early = mustAccount "early profile" (benchmarkAccount "early" transports)
      mid = mustAccount "mid profile" (benchmarkAccount "mid" transports)
      end = mustAccount "end profile" (benchmarkAccount "end" transports)
      maxed = mustAccount "maxed profile" (benchmarkAccount "maxed" transports)
      outdoorOnly = early {accountPlantedSpiritTrees = allPlayerPlantedSpiritTrees}
      pohOnly = end {accountPlantedSpiritTrees = Set.empty}
  assert (not (pohMountedXerics (accountPoh early)))
  assert (not (Map.member 13393 (accountInventory early) || Map.member 13393 (accountBank early)))
  assert (not (available early xerics))
  assert (not (available early basicBox))
  assert (not (available early ornateBox))
  assert (available mid basicBox)
  assert (not (available mid ornateBox))
  assert (available maxed ornateBox)
  assert (accountPlantedSpiritTrees early == Set.empty)
  assert (accountPlantedSpiritTrees mid == Set.singleton FarmingGuildTree)
  assert (accountPlantedSpiritTrees end == Set.fromList [FarmingGuildTree, PortSarimTree])
  assert (accountPlantedSpiritTrees maxed == allPlayerPlantedSpiritTrees)
  assert (not (available early farmingGuildTree))
  assert (available mid farmingGuildTree)
  assert (not (available mid portSarimTree))
  assert (available end portSarimTree)
  assert (available early permanentTree)
  assert (not (available outdoorOnly pohTree))
  assert (available pohOnly pohTree)
  assert (not (available pohOnly farmingGuildTree))
  assert (all (not . available base . fst) gates)
  assert (all (\(transport, gate) -> available (enable gate) transport) gates)
  assert (available mixed xerics)
  assert (all (not . available mixed . fst) (filter ((/= xerics) . fst) gates))
  assert (not (available base carriedXerics))
  assert (available (base {accountInventory = Map.singleton 13393 1}) carriedXerics)

assert :: Bool -> IO ()
assert True = pure ()
assert False = fail "assertion failed"

must :: String -> Maybe Transport -> Transport
must _ (Just transport) = transport
must name Nothing = error ("missing " <> name <> " transport")

mustAccount :: String -> Maybe AccountState -> AccountState
mustAccount _ (Just account) = account
mustAccount name Nothing = error ("missing " <> name)
