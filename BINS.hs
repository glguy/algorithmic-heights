module BINS where

import Data.Array

main = do
  arraySize <- readLn
  _querySize <- getLine
  sortedArray <- fmap (listArray (1,arraySize)) readLnN :: IO (Array Int Int)
  queries   <- readLnN :: IO [Int]
  putStrLn (unwords (map (show . searchExact sortedArray) queries))

readLnN :: Read a => IO [a]
readLnN = mapM readIO . words =<< getLine

searchExact :: Ord a => Array Int a -> a -> Int
searchExact a k
  | a ! i == k = i
  | otherwise  = -1
  where
  i = search a k

search :: Ord a => Array Int a -> a -> Int
search a k = let (lo,hi) = bounds a in aux lo hi
  where
  aux lo hi
    | lo == hi   = lo
    | k <= a ! i = aux lo i
    | otherwise  = aux (i+1) hi
    where
    i = lo + (hi - lo) `div` 2
