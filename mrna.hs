import Data.List (group, groupBy, sort, sortBy)

parseCodonTable :: String -> [(Char, String)]
parseCodonTable s = parseCodonTable' tokens
  where tokens = words s
        parseCodonTable' (rna:amino:xs) = 
          if amino /= "Stop" 
            then (head amino, rna) : parseCodonTable' xs
            else parseCodonTable' xs
        parseCodonTable' [] = []
          
countAminoCodons :: [(Char, String)] -> [(Char, Int)]
countAminoCodons = map (\l -> (fst $ head l, length l)) . 
                     (groupBy (\(c1, _) (c2, _) -> c1 == c2)) . 
                     (sortBy (\(c1, _) (c2, _) -> c1 `compare` c2))

rnaPossibilitiesModN :: Int -> [(Char, Int)] -> String -> Int
rnaPossibilitiesModN n aminoCount (c:cs) = 
  (case lookup c aminoCount of
      Just p -> p
      Nothing -> 1)
  * rnaPossibilitiesModN n aminoCount cs `mod` n
rnaPossibilitiesModN _ _ [] = 3 --b/c of the STOP codons. clever, no?

main = do
  tStr <- readFile "codontable.table"
  aStr <- readFile "mrna.txt"
  let table = parseCodonTable tStr
      aminoCount = countAminoCodons table
  putStrLn $ show $ rnaPossibilitiesModN 1000000 aminoCount aStr
