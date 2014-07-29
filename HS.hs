module HS where

import Common
import Data.Array
import Data.Array.ST
import Data.Foldable (traverse_, for_)
import Control.Monad (when)
import Debug.Trace

main = do
  n  <- readLn     :: IO Int
  xs <- readListIO :: IO [Int]

  let a = listArray (1,n) xs
      b = heapSort a

  printList b

heapSort :: (Show a, Ord a) => Array Int a -> Array Int a
heapSort a = runSTArray $ do
  a       <- thaw a
  (lo,hi) <- getBounds a

  -- Max heap the whole thing
  traverse_ (heapify1 a) [lo .. hi]

  for_ [hi, hi-1 .. lo] $ \i ->
    do -- Move biggest to correct location
       biggest <- readArray a lo
       endest  <- readArray a i
       writeArray a i  biggest
       writeArray a lo endest

       -- restore heap
       siftDown a lo (i-1)

  return a

siftDown a lo hi
  | 2*lo > hi = return ()
  | 2*lo+1 > hi = do x <- readArray a lo
                     y <- readArray a (2*lo)
                     when (x < y) $ do
                       writeArray a lo y
                       writeArray a (2*lo) x
  | otherwise = do
      x <- readArray a lo
      y1 <- readArray a (2*lo)
      y2 <- readArray a (2*lo+1)
      if x < y1 && y2 <= y1
         then do writeArray a lo y1
                 writeArray a (2*lo) x
                 siftDown a (2*lo) hi
         else if x < y2
           then do writeArray a lo y2
                   writeArray a (2*lo+1) x
                   siftDown a (2*lo+1) hi
         else return ()


heapify1 :: (MArray a e m, Ix i, Ord e, Integral i) => a i e -> i -> m ()
heapify1 a i =
  when (i > 1) $ do
    let pi = i`div`2
    p <- readArray a pi
    x <- readArray a i
    when (x > p) $ do
      writeArray a pi x
      writeArray a i  p
      heapify1 a pi
