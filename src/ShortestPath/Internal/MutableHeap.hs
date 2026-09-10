module ShortestPath.Internal.MutableHeap
  ( MutableHeap
  , heapNew
  , heapPop
  , heapPush
  ) where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector.Unboxed.Mutable as Mutable

data MutableHeap s = MutableHeap
  { heapPrioritiesRef :: STRef s (Mutable.MVector s Int)
  , heapStatesRef :: STRef s (Mutable.MVector s Int)
  , heapCostsRef :: STRef s (Mutable.MVector s Int)
  , heapSizeRef :: STRef s Int
  }

heapNew :: Int -> ST s (MutableHeap s)
heapNew requestedCapacity = do
  let capacity = max 1 requestedCapacity
  priorities <- Mutable.new capacity >>= newSTRef
  states <- Mutable.new capacity >>= newSTRef
  costs <- Mutable.new capacity >>= newSTRef
  size <- newSTRef 0
  pure (MutableHeap priorities states costs size)

heapPush :: MutableHeap s -> Int -> Int -> Int -> ST s ()
heapPush heap priority state cost = do
  size <- readSTRef (heapSizeRef heap)
  ensureCapacity heap size
  priorities <- readSTRef (heapPrioritiesRef heap)
  states <- readSTRef (heapStatesRef heap)
  costs <- readSTRef (heapCostsRef heap)
  Mutable.write priorities size priority
  Mutable.write states size state
  Mutable.write costs size cost
  writeSTRef (heapSizeRef heap) (size + 1)
  bubbleUp heap size

heapPop :: MutableHeap s -> ST s (Maybe (Int, Int, Int))
heapPop heap = do
  size <- readSTRef (heapSizeRef heap)
  if size == 0
    then pure Nothing
    else do
      entry <- heapEntry heap 0
      let lastIx = size - 1
      writeSTRef (heapSizeRef heap) lastIx
      when (lastIx > 0) $ do
        moveEntry heap lastIx 0
        bubbleDown heap 0
      pure (Just entry)

ensureCapacity :: MutableHeap s -> Int -> ST s ()
ensureCapacity heap size = do
  priorities <- readSTRef (heapPrioritiesRef heap)
  when (size >= Mutable.length priorities) $ do
    let extra = Mutable.length priorities
    Mutable.grow priorities extra >>= writeSTRef (heapPrioritiesRef heap)
    readSTRef (heapStatesRef heap) >>= flip Mutable.grow extra >>= writeSTRef (heapStatesRef heap)
    readSTRef (heapCostsRef heap) >>= flip Mutable.grow extra >>= writeSTRef (heapCostsRef heap)

bubbleUp :: MutableHeap s -> Int -> ST s ()
bubbleUp heap ix
  | ix <= 0 = pure ()
  | otherwise = do
      let parent = (ix - 1) `div` 2
      childEntry <- heapEntry heap ix
      parentEntry <- heapEntry heap parent
      if entryLess childEntry parentEntry
        then swapEntries heap ix parent >> bubbleUp heap parent
        else pure ()

bubbleDown :: MutableHeap s -> Int -> ST s ()
bubbleDown heap ix = do
  size <- readSTRef (heapSizeRef heap)
  let left = ix * 2 + 1
      right = left + 1
  if left >= size
    then pure ()
    else do
      smallest <- do
        leftEntry <- heapEntry heap left
        if right >= size
          then pure left
          else do
            rightEntry <- heapEntry heap right
            pure (if entryLess rightEntry leftEntry then right else left)
      here <- heapEntry heap ix
      child <- heapEntry heap smallest
      if entryLess child here
        then swapEntries heap ix smallest >> bubbleDown heap smallest
        else pure ()

heapEntry :: MutableHeap s -> Int -> ST s (Int, Int, Int)
heapEntry heap ix = do
  priorities <- readSTRef (heapPrioritiesRef heap)
  states <- readSTRef (heapStatesRef heap)
  costs <- readSTRef (heapCostsRef heap)
  (,,) <$> Mutable.read priorities ix <*> Mutable.read states ix <*> Mutable.read costs ix

entryLess :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
entryLess (leftPriority, leftState, leftCost) (rightPriority, rightState, rightCost) =
  (leftPriority, leftCost, leftState) < (rightPriority, rightCost, rightState)

swapEntries :: MutableHeap s -> Int -> Int -> ST s ()
swapEntries heap left right = do
  entry <- heapEntry heap left
  moveEntry heap right left
  writeEntry heap right entry

moveEntry :: MutableHeap s -> Int -> Int -> ST s ()
moveEntry heap from to = heapEntry heap from >>= writeEntry heap to

writeEntry :: MutableHeap s -> Int -> (Int, Int, Int) -> ST s ()
writeEntry heap ix (priority, state, cost) = do
  priorities <- readSTRef (heapPrioritiesRef heap)
  states <- readSTRef (heapStatesRef heap)
  costs <- readSTRef (heapCostsRef heap)
  Mutable.write priorities ix priority
  Mutable.write states ix state
  Mutable.write costs ix cost
