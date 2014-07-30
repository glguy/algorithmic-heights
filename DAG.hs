module DAG where

import           Control.Monad (replicateM)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.List (foldl')

import           Common
import qualified PriorityMinQueue as PMQ

main :: IO ()
main = do
  n  <- readLn
  gs <- replicateM n (getLine >> readDGraph)
  printList (map (outputFormat . isAcyclic) gs)

outputFormat True = 1
outputFormat False = -1

invertGraph :: IntMap [Int] -> IntMap [Int]
invertGraph g =
  IntMap.fromListWith (++)
    [ (n, [v]) | (v,ns) <- IntMap.toList g, n <- ns ]

isAcyclic :: Graph -> Bool
isAcyclic g0 = foldr removeLeaf IntMap.null leaves g0
  where
  g' = invertGraph g0
  leaves = [ v | (v,[]) <- IntMap.toList g0 ]

  removeLeaf :: Int -> (Graph -> Bool) -> Graph -> Bool
  removeLeaf v k g = foldr removeLeaf k newLeaves g2
    where
    deps = IntMap.findWithDefault [] v g'
    g1   = IntMap.delete v g -- remove leaf
    g2   = foldl' (removeEdge v) g1 deps
    newLeaves = [ l | l <- deps, null (IntMap.findWithDefault [] l g2) ]

  removeEdge :: Int -> IntMap [Int] -> Int -> IntMap [Int]
  removeEdge e g v = IntMap.update (Just . filter (/= e)) v g
