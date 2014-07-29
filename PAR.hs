module PAR where

import Common
import Data.Array
import Data.Array.ST

main = do
  n  <- readLn     :: IO Int
  xs <- readListIO :: IO [Int]

  let a = listArray (1,n) xs
      b = partition a
  printList b

partition a = runSTArray $ do
  a <- thaw a
  (lo,hi) <- getBounds a
  p <- readArray a lo
  partitionLoop a p lo (lo+1) hi
  return a

partitionLoop a p pi lo hi
  | lo <= hi = do x <- readArray a lo
                  if x <= p then do writeArray a pi x
                                    writeArray a lo =<< readArray a (pi+1)
                                    writeArray a (pi+1) p
                                    partitionLoop a p (pi+1) (lo+1) hi
                            else do partitionLoop a p pi (lo+1) hi
  | otherwise = return ()
