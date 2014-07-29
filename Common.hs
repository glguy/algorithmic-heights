module Common where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Control.Monad
import Data.List
import Data.Foldable (Foldable, toList)

type Graph = IntMap [Int]

readListIO :: Read a => IO [a]
readListIO = mapM readIO . words =<< getLine

printList :: (Foldable t, Show a) => t a -> IO ()
printList = putStrLn . unwords . map show . toList

readUGraph :: IO Graph
readUGraph = do
  [vs,es] <- readListIO
  edges <- replicateM es readListIO
  let g0 = IntMap.fromList [(i,[]) | i <- [1..vs]]
      addEdge x y = IntMap.insertWith (++) x [y]
      addUEdge x y = addEdge x y . addEdge y x
  return (foldl' (\g [x,y] -> addUEdge x y g) g0 edges)

readDGraph :: IO Graph
readDGraph = do
  [vs,es] <- readListIO
  edges <- replicateM es readListIO
  let g0 = IntMap.fromList [(i,[]) | i <- [1..vs]]
      addEdge x y = IntMap.insertWith (++) x [y]
  return (foldl' (\g [x,y] -> addEdge x y g) g0 edges)

data WeightedEdge = WeightedEdge
  { edgeTarget :: Int
  , edgeWeight :: Int
  }

readWGraph :: IO (IntMap [WeightedEdge])
readWGraph = do
  [vs,es] <- readListIO
  edges <- replicateM es readListIO
  let g0 = IntMap.fromList [(i,[]) | i <- [1..vs]]
      addEdge x y w = IntMap.insertWith (++) x [WeightedEdge y w]
  return (foldl' (\g [x,y,w] -> addEdge x y w g) g0 edges)
