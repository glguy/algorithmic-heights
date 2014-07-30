module DIJ where

import qualified Data.IntMap as IntMap
import           Data.IntMap (IntMap)
import           Data.Monoid

import           Common
import           PriorityMinQueue

main :: IO ()
main = do
  g <- readWGraph
  printList (solve g)

-- | Dijkstra's algorithm with the addition of -1 weights for unreachable nodes
solve :: IntMap [WeightedEdge] -> IntMap Int
solve g = dijkstra g <> fmap (const (-1)) g

dijkstra :: IntMap [WeightedEdge] -> IntMap Int
dijkstra g = aux (singleton 1 0) IntMap.empty
  where
  aux :: PriorityMinQueue Int -> IntMap Int -> IntMap Int
  aux q acc = case pop q of
    Nothing -> acc
    Just (dist, v, q1)
      | IntMap.member v acc -> aux q1 acc -- already complete
      | otherwise ->
         let acc' = IntMap.insert v dist acc
             q2   = fromList [ (edgeTarget e, edgeWeight e + dist) | e <- neighbors g v]
         in aux (q2 <> q1) acc'

neighbors :: IntMap [WeightedEdge] -> Int -> [WeightedEdge]
neighbors g v = IntMap.findWithDefault [] v g
