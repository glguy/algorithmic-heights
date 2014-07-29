module HEA where

import Common
import Data.Array
import Data.Array.ST
import Data.Foldable (traverse_)
import Control.Monad (when)

main = do
  n  <- readLn     :: IO Int
  xs <- readListIO :: IO [Int]

  let a = listArray (1,n) xs
      b = heapify a

  printList b

heapify :: Ord a => Array Int a -> Array Int a
heapify a = runSTArray $ do
  a       <- thaw a
  (lo,hi) <- getBounds a
  traverse_ (heapify1 a) [lo .. hi]
  return a

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
