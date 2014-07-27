{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad.ST
import Control.Monad
import Data.STRef.Strict
import Data.Array.ST
import Data.Array (Array, listArray)
import Data.Foldable (for_)

main :: IO ()
main = do
  n  <- readLn
  xs <- readLnN :: IO [Int]
  print (insertionSort (listArray (1,n) xs))

readLnN :: Read a => IO [a]
readLnN = mapM readIO . words =<< getLine

insertionSort :: forall a. Ord a => Array Int a -> Int
insertionSort a' = runST $ do
  a <- thaw a' :: ST s (STArray s Int a)
  swaps <- newSTRef 0

  let swap i j = do
        tmp <- readArray a i
        writeArray a i =<< readArray a j
        writeArray a j tmp
        modifySTRef swaps (+1)

  (1,n) <- getBounds a
  for_ [2 .. n] $ \i ->
    let loop k =
          when (k > 1) $ do
            ak  <- readArray a k
            ak1 <- readArray a (k-1)
            when (ak < ak1) $ do
              swap k (k-1)
              loop (k-1)
    in loop i

  readSTRef swaps
