import Data.List.Split (splitWhen)

dominantProbability :: Double -> Double -> Double -> Double
dominantProbability k m n = 
  let total = k + m + n
  -- calculate inverse of probability that child is homozygous recessive
  in 1 
     - (n/total * (n-1)/(total-1)) -- probability of two homozygous recessive
     - 0.5 * (m/total * n/(total-1)) -- probability of first hetero and second homo 
                                     -- 50% chance child is homo recessive
     - 0.5 * (n/total * m/(total-1)) -- prob of first homo and second hetero
                                     -- 50% chance child is homo recessive
     - 0.25 * (m/total * (m-1)/(total-1)) -- prob of both hetero
                                          --25% chance child is homo recessive
        
main = do
  inpStr <- getLine
  let numbers = map (read :: String -> Double) (splitWhen (==' ') inpStr)
      answer = dominantProbability (numbers!!0) (numbers!!1) (numbers!!2)
  putStrLn (show answer)
