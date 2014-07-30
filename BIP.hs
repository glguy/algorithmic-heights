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


-- | Domain-specific "boolean"
data Color = Black | White deriving (Eq)

alternateColor :: Color -> Color
alternateColor Black = White
alternateColor White = Black


-- | Return True when given undirected graph is bipartite.
isBipartite :: Graph -> Bool
isBipartite g =
  case IntMap.minViewWithKey g of
    Nothing        -> True
    Just ((v,_),_) -> check' IntMap.empty g
      where
      check   = const isBipartite                             :: BipartiteCheck
      check'  = processVertex White v check                   :: BipartiteCheck

-- | Graph colors -> Graph edges -> Is Bipartite?
type BipartiteCheck = IntMap Color -> Graph -> Bool

-- | Check a component of the graph starting at the given vertex with the
-- given color. Continue checking the rest of the graph with the given
-- continuation after checking this vertex's neighbors.
processVertex ::
  Color          {- ^ Current color  -} ->
  Int            {- ^ Current vertex -} ->
  BipartiteCheck {- ^ Resume check on rest of graph -} ->
  BipartiteCheck
processVertex color v check colors g =
  case IntMap.lookup v colors of
    Just c  -> c == color
            && check  colors  g
    Nothing -> check' colors' g'
     where
     check'    = foldr (processVertex color') check neighbors :: BipartiteCheck
     color'    = alternateColor color                         :: Color
     colors'   = IntMap.insert v color colors                 :: IntMap Color
     g'        = IntMap.delete v g                            :: Graph
     neighbors = IntMap.findWithDefault [] v g                :: [Int]
