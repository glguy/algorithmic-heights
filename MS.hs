module MS where

import Common
import Data.List

main = do
  n  <- readLn :: IO Int
  xs <- readListIO :: IO [Int]
  printList (sort xs)

{-
mergeSort :: Ord a => Array Int a -> Array Int a
mergeSort inArray = runSTArray $ do
  a <- thaw inArray
  (lo,hi) <- getBounds a
  mergeSort' a lo hi
  return a

mergeSort' :: Ord a => STArray s Int a -> Int -> Int -> ST s (STArray s Int a)
mergeSort' a lo hi =
  when (hi - lo > 1) $ do
    let mid = lo + (hi - lo)`div`2
    mergeSort' a lo mid
    mergeSort' a (mid + 1) hi
    mergeArrays a lo (mid + 1) hi

mergeArrays :: Ord a => STArray s Int a -> Int -> Int -> Int -> Int -> STArrayST s ()
mergeArrays a al ah bl bh out outI
  | al <= ah && bl <= bh =
       do x <- readArray a al
          y <- readArray a bl
          if x <= y then mergeArrays a (al+1) bl bh
                    else do writeArray a al y
                            writeArray a bl x
                            mergeArrays a al (bl+1) bh
  | otherwise = return ()
-}
