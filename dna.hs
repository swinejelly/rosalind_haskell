import Data.List

count :: (Eq a) => a -> [a] -> Int
count n (x:xs) =
  if n == x
  then 1 + count n xs
  else count n xs
count _ _ = 0

nucleotides :: String -> [Int]
nucleotides strand = 
  [count 'A' strand,
   count 'C' strand,
   count 'G' strand,
   count 'T' strand]

main = do
  inpstr <- getLine
  let result = nucleotides inpstr
  putStrLn (intercalate " " (map show result)) 
