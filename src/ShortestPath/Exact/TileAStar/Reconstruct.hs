module ShortestPath.Exact.TileAStar.Reconstruct
  ( reconstructRouteSteps
  ) where

import Control.Monad.ST (ST)
import qualified Data.Vector.Mutable as BoxedMutable
import qualified Data.Vector.Unboxed.Mutable as Mutable

import ShortestPath.Pathfinder

-- | Interpret predecessor transitions recorded by the search. All movement and
-- account semantics have already been resolved before entries reach this table.
reconstructRouteSteps ::
  Mutable.MVector s Int ->
  BoxedMutable.MVector s (Maybe RouteStep) ->
  Int ->
  ST s [RouteStep]
reconstructRouteSteps prevState prevStep state = reverse <$> collect state
 where
  collect current = do
    previous <- Mutable.read prevState current
    step <- BoxedMutable.read prevStep current
    case step of
      Nothing -> pure []
      Just value -> (value :) <$> collect previous
