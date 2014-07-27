module DDEG where

import Control.Monad
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Data.List (foldl')

import Common

main = do
  g <- readGraph
  printList [ sum (map (degree g) ns) | (i,ns) <- IntMap.toList g ]

degree g x = maybe 0 length (IntMap.lookup x g)

