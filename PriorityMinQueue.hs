module PriorityMinQueue where

import qualified Data.IntMap as IntMap
import           Data.IntMap (IntMap)
import           Data.Monoid

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
