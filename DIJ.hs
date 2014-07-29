module DIJ where

import Common
import qualified Data.IntMap as IntMap
import           Data.IntMap (IntMap)
import Data.Monoid

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

--
--
--

newtype PriorityMinQueue a = PMQ (IntMap [a])

instance Monoid (PriorityMinQueue a) where
  mappend (PMQ x) (PMQ y) = PMQ (IntMap.unionWith (++) x y)
  mempty = PMQ IntMap.empty

singleton :: a -> Int -> PriorityMinQueue a
singleton v p = PMQ (IntMap.singleton p [v])

fromList :: [(a,Int)] -> PriorityMinQueue a
fromList xs = PMQ (IntMap.fromListWith (++) [ (p,[v]) | (v,p) <- xs ])

push :: a -> Int -> PriorityMinQueue a -> PriorityMinQueue a
push v p (PMQ q) = PMQ (IntMap.insertWith (++) p [v] q)

pop :: PriorityMinQueue a -> Maybe (Int, a, PriorityMinQueue a)
pop (PMQ q) = do
  ((k,vs),q1) <- IntMap.minViewWithKey q
  case vs of
    []     -> pop    (PMQ q1) -- probably shouldn't happen
    [v]    -> return (k, v, PMQ q1)
    (v:vs) -> return (k, v, PMQ (IntMap.insert k vs q1))
