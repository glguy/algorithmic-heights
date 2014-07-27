module DEG where

import Control.Monad
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)

import Common

inc :: Int -> IntMap Int -> IntMap Int
inc x = IntMap.insertWith (+) x 1

main = do
  g <- readGraph
  printList [ length ns | ns <- IntMap.elems g ]
