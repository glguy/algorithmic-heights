module FIBO where

import Data.List

main :: IO ()
main = do
  n <- readLn
  print (fibs !! n)

fibs :: [Integer]
fibs = unfoldr gen (0,1)
  where
  gen (x,y) = Just (x,(y, x+y))
