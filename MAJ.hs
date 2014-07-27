module MAJ where

import Debug.Trace
import Control.Monad -- (replicateM)
import Data.List
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

readLnN :: Read a => IO [a]
readLnN = mapM readIO . words =<< getLine

main = do
  [n,len] <- readLnN
  xs    <- replicateM n readLnN :: IO [[Int]]
  when (any (\x -> length x /= len) xs) (fail "bad input")
  putStrLn (unwords (map (show . fromMaybe (-1) . majority) xs))

majority :: Eq a => [a] -> Maybe a
majority xs = do
  c <- majorityCandidate xs
  guard (length xs < 2 * length (filter (c ==) xs))
  return c

majorityCandidate :: Eq a => [a] -> Maybe a
majorityCandidate xs = foldr step done xs Nothing (0 :: Int)
  where
  done elt n | n > 0     = elt
             | otherwise = Nothing

  step x next _       0 = next (Just x) 1
  step x next (Just y) n
      | x == y    = next (Just y) $! (n+1)
      | otherwise = next (Just y) $! (n-1)
