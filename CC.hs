module CC where

import Common
import qualified Data.IntMap as IntMap
import Data.List

main :: IO ()
main = do
  g <- readUGraph
  print (countCCs g)


countCCs :: Graph -> Int
countCCs = aux 0
  where
  aux n g = case IntMap.minViewWithKey g of
    Nothing -> n
    Just ((v,neighs), g') -> aux (n+1) (foldl' removeCC g' neighs)

removeCC :: Graph -> Int -> Graph
removeCC g x = case IntMap.lookup x g of
  Nothing -> g
  Just ys -> foldl' removeCC (IntMap.delete x g) ys
