module TWOSUM where

import Common
import Control.Monad
import Data.Foldable (for_)
import qualified Data.IntMap as IntMap

main = do
  [k,n] <- readListIO
  xs    <- replicateM k readListIO
  for_ (map twosum xs) $ \res ->
    printList $ case res of
      Nothing    -> [-1]
      Just (i,j) -> [i,j]

twosum :: [Int] -> Maybe (Int, Int)
twosum xs = foldr step (const Nothing) (zip [1..] xs) IntMap.empty
  where
  step (i,x) next prev = mplus here there
    where
    here = do j <- IntMap.lookup (-x) prev
              return (j,i)

    there = next (IntMap.insert x i prev)
