module MER where

readLnN :: Read a => IO [a]
readLnN = mapM readIO . words =<< getLine

printList :: Show a => [a] -> IO ()
printList = putStrLn . unwords . map show

main = do
  _ <- getLine
  xs <- readLnN
  _ <- getLine
  ys <- readLnN
  printList (merge xs ys :: [Int])

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys
