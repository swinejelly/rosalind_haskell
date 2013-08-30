choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k

main = do
  inpStr <- readFile "lia.txt"
  let (k:nMin:_) = map read $ words inpStr :: [Integer] -- n is n in binom
      numOrganisms = 2^k :: Integer
      result = sum $
                [(fromIntegral (choose numOrganisms n)) * (0.25 ^ n) * 
                (0.75 ^ (numOrganisms - n)) |
                n <- [nMin..numOrganisms]]
  putStrLn $ show result
