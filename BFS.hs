module BFS where

import Common
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import           Data.IntMap (IntMap)
import qualified Data.Sequence as Seq
import Data.Maybe
import Data.List
import Data.Monoid


main :: IO ()
main = do
  g <- readDGraph
  let dists = bfs mempty (Seq.fromList [(1,0)]) g
           <> fmap (const (-1)) g
  printList dists

neighbors :: Int -> Graph -> [Int]
neighbors v g = fromMaybe [] (IntMap.lookup v g)

bfs :: IntMap Int -> Seq.Seq (Int, Int)-> IntMap [Int] -> IntMap Int
bfs visited q g =
  case Seq.viewl q of
    Seq.EmptyL -> visited
    (v,dist) Seq.:< q1
       | IntMap.member v visited -> bfs visited q1 g
       | otherwise -> let visited' = IntMap.insert v dist visited
                          q2 = q1 <> Seq.fromList [ (n,dist+1) | n <- neighbors v g ]
                      in bfs visited' q2 g
