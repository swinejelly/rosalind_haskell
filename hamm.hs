numDiff :: (Eq a) => [a] -> [a] -> Int
numDiff left right = foldr (+) 0 $
              zipWith (\x y -> if x /= y then 1 else 0) left right

main = do
  txt <- readFile "hamm.txt"
  let inputLines = lines txt
      s = inputLines !! 0
      t = inputLines !! 1
  putStr $ show (numDiff s t)
