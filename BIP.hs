module BIP where

import Common
import Control.Monad
import qualified Data.IntMap as IntMap
import           Data.IntMap (IntMap)

main = do
  n <- readLn
  gs <- replicateM n (getLine >> readUGraph)
  printList (map (outputFormat . isBipartite) gs)

outputFormat :: Bool -> Int
outputFormat True  = 1
outputFormat False = -1

isBipartite :: Graph -> Bool
isBipartite g =
  case IntMap.minViewWithKey g of
    Nothing        -> True
    Just ((v,_),_) -> processVertex True v (const isBipartite) IntMap.empty g

processVertex ::
  Bool                                  {- Current parity -} ->
  Int                                   {- Current vertex -} ->
  (IntMap Bool -> IntMap [Int] -> Bool) {- Continuation   -} ->
  IntMap Bool                           {- Graph parities -} ->
  IntMap [Int]                          {- Graph edges    -} ->
  Bool
processVertex parity v k parities g =
  case IntMap.lookup v parities of
    Just p  -> p == parity && k parities g
    Nothing -> foldr (processVertex (not parity))
                     k
                     (IntMap.findWithDefault [] v g)
                     (IntMap.insert v parity parities)
                     (IntMap.delete v g)
