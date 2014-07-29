module Main where

import Common
import Data.List

main = do
  _ <- getLine
  xs <- readListIO :: IO [Int]
  print (inversions xs)


inversions :: Ord a => [a] -> Int
inversions xs = length [ () | y:ys <- tails xs, z <- ys, y > z ]
