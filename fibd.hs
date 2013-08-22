import Data.List (unfoldr)

-- A list of tuples (n, m) where n = number of immature rabbits and m = number of mature rabbits
-- This does not account for mortality and is only meant to provide a starter list
simpleFib :: [(Integer, Integer)]
simpleFib = (1, 0) : unfoldr nextStep (1, 0)
  where nextStep (n, m) = Just (result, result)
          where result = (m, n+m)

mortalityFib :: Int -> [(Integer, Integer)]
mortalityFib life = mortalityFibList
  where mortalityFibList = (take life $ simpleFib) 
                           ++ zipWith nextStepMortal mortalityFibList 
                           (drop (life-1) mortalityFibList)
        nextStepMortal prev@(n1, m1) nPast@(n2, m2) = (m2, m2 + n2 - n1)

mortalityFibN :: Int -> Int -> Integer
mortalityFibN generations life = immature + mature
  where (immature, mature) = (!!(generations-1)) $ mortalityFib life

main = do
        inpStr <- readFile "fibd.txt"
        let (n:m:_) = map (read :: String -> Int) (words inpStr)
        putStrLn $ show (mortalityFibN n m)
        
