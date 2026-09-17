module Main (main) where

import Data.List (find, isInfixOf)
import qualified Data.Map.Strict as Map

import ShortestPath.Account
import ShortestPath.BenchmarkProfiles (benchmarkAccount, benchmarkNowMinutes)
import ShortestPath.Transport

main :: IO ()
main = do
  transports <- loadTransports defaultSourcePaths
  let context account = RequirementContext account CarriedOnly benchmarkNowMinutes
      available account = requirementsSatisfied (context account)
      mounted name = must name (find (\transport -> transportType transport == "TELEPORTATION_BOX" && name `isInfixOf` objectInfo transport) transports)
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
  assert (not (pohMountedXerics (accountPoh early)))
  assert (not (Map.member 13393 (accountInventory early) || Map.member 13393 (accountBank early)))
  assert (not (available early xerics))
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
