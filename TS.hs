module TS where

import           Control.Monad (replicateM)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.List (foldl')

import           Common
import qualified PriorityMinQueue as PMQ

main :: IO ()
main = do
  g <- readDGraph
  printList (topoSort g)

invertGraph :: IntMap [Int] -> IntMap [Int]
invertGraph g =
  IntMap.fromListWith (++)
    [ (n, [v]) | (v,ns) <- IntMap.toList g, n <- ns ]

topoSort :: Graph -> [Int]
topoSort g0 = foldr removeLeaf (const []) leaves g'
  where
  g'     = invertGraph g0
  leaves = IntMap.keys (g0 IntMap.\\ g')

  removeLeaf :: Int -> (Graph -> [Int]) -> Graph -> [Int]
  removeLeaf v k g = v : foldr removeLeaf k newLeaves g2
    where
    deps = IntMap.findWithDefault [] v g0
    g1   = IntMap.delete v g -- remove leaf
    g2   = foldl' (removeEdge v) g1 deps
    newLeaves = [ l | l <- deps, null (IntMap.findWithDefault [] l g2) ]

  removeEdge :: Int -> IntMap [Int] -> Int -> IntMap [Int]
  removeEdge e g v = IntMap.update (Just . filter (/= e)) v g
