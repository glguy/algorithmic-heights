module DEG where

import Control.Monad
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)

readLnN :: Read a => IO [a]
readLnN = mapM readIO . words =<< getLine

inc :: Int -> IntMap Int -> IntMap Int
inc x = IntMap.insertWith (+) x 1

main = do
  [verticies, edges] <- readLnN

  let step degrees _ = do
        [x,y] <- readLnN
        return $! inc x (inc y degrees)

  let start = IntMap.fromList [(i,0) | i <- [1..verticies]]
  degs <- foldM step start [1..edges]
  putStrLn (unwords (map show (IntMap.elems degs)))
