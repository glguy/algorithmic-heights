module BIP where

import           Control.Monad (replicateM)
import qualified Data.IntMap as IntMap
import           Data.IntMap (IntMap)

import           Common (Graph, readUGraph, printList)

main :: IO ()
main = do
  n  <- readLn
  gs <- replicateM n (getLine >> readUGraph)
  printList (map (outputFormat . isBipartite) gs)

-- | Map 'True' to '1' and 'False' to '-1' as required for BIP
outputFormat :: Bool -> Int
outputFormat True  =  1
outputFormat False = -1

-- | Return True when given undirected graph is bipartite.
isBipartite :: Graph -> Bool
isBipartite g =
  case IntMap.minViewWithKey g of
    Nothing        -> True
    Just ((v,_),_) -> processVertex True v (const isBipartite) IntMap.empty g

processVertex ::
  Bool                           {- ^ Current parity -} ->
  Int                            {- ^ Current vertex -} ->
  (IntMap Bool -> Graph -> Bool) {- ^ Continuation   -} ->
  IntMap Bool                    {- ^ Graph parities -} ->
  Graph                          {- ^ Graph edges    -} ->
  Bool                           {- ^ is bipartite?  -}
processVertex parity v k parities g =
  case IntMap.lookup v parities of
    Just p  -> p == parity && k parities g
    Nothing -> foldr (processVertex (not parity))      -- flip parity for neighbors
                     k
                     (IntMap.findWithDefault [] v g)   -- neighbor list
                     (IntMap.insert v parity parities) -- mark parity
                     (IntMap.delete v g)               -- shrink graph
