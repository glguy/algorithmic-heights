module Main where

import Common
import Data.Word (Word16)
import Data.List (tails)
import Control.Monad (replicateM)
import qualified Data.IntMap as IntMap
import           Data.IntMap (IntMap)

data IxPair = P {-# UNPACK #-} !Word16 {-# UNPACK #-} !Word16

main = do
  [k,_n] <- readListIO
  replicateM k testCase

testCase = do
  xs <- readListIO
  printList (formatOutput (go xs))

formatOutput [] = [-1]
formatOutput (x:_) = x

go :: [Int] -> [[Int]]
go xs =
  [ [i,j,k]
     | (i,x:ys) <- zip [1..] (tails xs)
     , (j,y)    <- zip [i+1..] ys
     , k        <- IntMap.findWithDefault [] (negate (x+y)) m
     , j < k
     ]
  where
  m = IntMap.fromListWith (++) [(x,[i]) | (i,x) <- zip [1..] xs]
