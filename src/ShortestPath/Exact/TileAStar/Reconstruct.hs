module ShortestPath.Exact.TileAStar.Reconstruct
  ( reconstructRouteSteps
  ) where

import Control.Monad.ST (ST)
import qualified Data.Vector.Mutable as BoxedMutable
import qualified Data.Vector.Unboxed.Mutable as Mutable

import ShortestPath.Pathfinder
import ShortestPath.Tile

-- | Interpret predecessor transitions recorded by the search. All movement and
-- account semantics have already been resolved before entries reach this table.
reconstructRouteSteps ::
  (Int -> Tile) ->
  Mutable.MVector s Int ->
  Mutable.MVector s Int ->
  BoxedMutable.MVector s String ->
  Int ->
  ST s [RouteStep]
reconstructRouteSteps stateTile prevState prevKind prevLabel state = reverse <$> collect state
 where
  collect current = do
    previous <- Mutable.read prevState current
    if previous == maxBound
      then pure []
      else do
        kind <- Mutable.read prevKind current
        rest <- collect previous
        case kind of
          0 -> pure (Walk (stateTile current) : rest)
          1 -> (: rest) <$> (UseTransport <$> BoxedMutable.read prevLabel current <*> pure (stateTile current))
          _ -> pure rest
