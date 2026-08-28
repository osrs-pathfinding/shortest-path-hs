module Main (main) where

import ShortestPath.Requirements
import ShortestPath.Tile
import ShortestPath.Transport

main :: IO ()
main = do
  assert (unpackTile (packTile 3200 3201 2) == (3200, 3201, 2))
  assert (parseSkills "75 Construction;83 Farming" == [SkillReq 75 "Construction", SkillReq 83 "Farming"])
  assert (parseVars Varbit "4070=0;4560&2" == [VarReq Varbit 4070 0 VarEq, VarReq Varbit 4560 2 VarMask])
  assert (parseTileField "3221 3218 0" == Just (packTile 3221 3218 0))

assert :: Bool -> IO ()
assert True = pure ()
assert False = fail "assertion failed"
