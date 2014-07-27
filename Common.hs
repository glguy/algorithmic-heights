module Common where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Control.Monad
import Data.List

readListIO :: Read a => IO [a]
readListIO = mapM readIO . words =<< getLine

printList :: Show a => [a] -> IO ()
printList = putStrLn . unwords . map show

readGraph :: IO (IntMap [Int])
readGraph = do
  [vs,es] <- readListIO
  edges <- replicateM es readListIO
  let g0 = IntMap.fromList [(i,[]) | i <- [1..vs]]
      addEdge x y = IntMap.insertWith (++) x [y]
      addUEdge x y = addEdge x y . addEdge y x
  return (foldl' (\g [x,y] -> addUEdge x y g) g0 edges)
